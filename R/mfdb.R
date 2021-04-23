# Init mfdb object and open connection to database
mfdb <- function(case_study_name = "",
                 db_params = list(),
                 destroy_schema = FALSE,
                 check_db_available = FALSE,
                 save_temp_tables = FALSE) {
    logger <- logging::getLogger('mfdb')

    # Try a selection of host strings until we connect to something
    db_params <- as.list(db_params)
    db_defaults <- list(dbname = "mf", drv = RPostgres::Postgres())
    db_guesses <- list(
        list(host = "/tmp"),
        list(host = "/var/tmp"),
        list(host = "/var/run/postgresql"),
        list(host = "localhost"),
        list(host = "localhost", user = "mf", password = "mf"),
        list(host = "/tmp/pg_mfdb"))

    # Override db_params with MFDB_USER / MFDB_PORT / .. env vars if available
    for (p in c('user', 'host', 'port', 'password', 'dbname')) {
        env_name <- paste0("MFDB_", toupper(p))
        if (nzchar(Sys.getenv(env_name))) {
            db_params[[p]] <- Sys.getenv(env_name)
        }
    }

    if (identical(db_params$host, 'mfdb.rhi.hi.is')) {
        # Enforce user
        if (!isTRUE(nzchar(db_params$user)) && interactive()) {
            db_params$user <- readline("Username: ")
        }
        # Enforce password
        if (!isTRUE(nzchar(db_params$password)) && interactive()) {
            db_params$password <- getPass::getPass("Password: ")
        }
        # Enforce SSL, no point having lots of guesses
        db_guesses <- list(list(sslmode = "require"))
    }

    for (guess in db_guesses) {
        db_combined <- c(db_params, guess, db_defaults)
        db_combined <- db_combined[!duplicated(names(db_combined))]

        # SSL mode is actually added to dbname
        if (isTRUE(nzchar(db_combined$sslmode))) {
            db_combined$dbname <- paste(
                c("dbname", "sslmode"),
                c(db_combined$dbname, db_combined$sslmode),
                sep = "=", collapse = " ")
            db_combined$sslmode <- NULL
        }

        logger$debug(paste0(
            "Trying to connect to: ",
            paste(capture.output(str(db_combined)), collapse = "\n")))
        db_connection <- tryCatch(do.call(DBI::dbConnect, db_combined), error = function (e) e)
        if ("error" %in% class(db_connection)) {
            logger$debug(paste0("Failed: ", paste(db_connection$message, collapse = "\n")))
        } else {
            break
        }
    }

    if (check_db_available) {
        # Just check we managed to make a connection, for examples
        if (nzchar(Sys.getenv('MFDB_FORCE_AVAILABLE'))) return (TRUE)
        if (!("error" %in% class(db_connection))) return(TRUE)
        return(FALSE)
    }

    if ("error" %in% class(db_connection)) {
        stop("Could not connect to mf database: ", paste(db_connection$message, collapse = "\n"))
    }

    # Create mdb object, set connection to use selected schema
    mdb <- structure(list(
            logger = logger,
            save_temp_tables = save_temp_tables,
            state = new.env(),
            schema = if (nzchar(case_study_name)) gsub('\\W', '_', tolower(case_study_name)) else "public",
            db_args = db_combined,
            db = db_connection), class = "mfdb")
    if (mfdb_is_postgres(mdb)) {
        schema_count <- mfdb_fetch(mdb,
            "SELECT COUNT(*)",
            " FROM pg_catalog.pg_namespace",
            " WHERE nspname IN ", sql_quote(mdb$schema, always_bracket = TRUE))[, c(1)]

        if (!nzchar(case_study_name)) {
            names <- mfdb_fetch(mdb,
                "SELECT table_schema",
                " FROM information_schema.tables",
                " WHERE table_schema != 'public' AND table_name = 'mfdb_schema'")
            names <- if (ncol(names) > 0) names[,1] else c()
            stop("You must supply a schema name for case_study_name to use in the database. Available names:-\n", paste0("* ", names, collapse = "\n"))
        }

        if (destroy_schema) {
            if (schema_count == 0) {
                mdb$logger$info(paste0("Schema ", mdb$schema, " does not exist. Doing nothing"))
            } else {
                mfdb_destroy_schema(mdb)
                mdb$logger$info(paste0("Schema ", mdb$schema, " removed, connect again to repopulate DB."))
            }
            dbDisconnect(mdb$db)
            return(invisible(NULL))
        }

        if (case_study_name == 'public') {
            stop("Can't connect to the public schema, since the database tables will be different. ",
                "Instead, use the case study name and copy the data out of public")
        }

        # Update schema and taxonomies
        if (schema_count > 0) {
            mfdb_send(mdb, "SET search_path TO ", paste(mdb$schema, 'pg_temp', sep =","))
        } else {
            logger$info(paste0("No schema, creating ", mdb$schema))
            mfdb_transaction(mdb, {
                mfdb_send(mdb, "CREATE SCHEMA ", mdb$schema)
            })
            mfdb_send(mdb, "SET search_path TO ", paste(mdb$schema, 'pg_temp', sep =","))

            # If schema didn't exist before, see if there's data to be had in the old public tables
            if (mfdb_table_exists(mdb, 'case_study', schema_name = 'public')) {
                res <- mfdb_fetch(mdb,
                    "SELECT case_study_id",
                    " FROM public.case_study",
                    " WHERE name = ", sql_quote(case_study_name))
            } else {
                res <- c()
            }
            if (length(res) > 0 && nrow(res) > 0) {
                logger$info(paste0("Copying data from ", case_study_name))
                # A case study exists by this ID
                old_case_study_id <- res[1,1]

                # Upgrade the old database to a known state
                mdb_public <- structure(list(
                    logger = mdb$logger,
                    save_temp_tables = mdb$save_temp_tables,
                    state = mdb$state,
                    schema = "public",
                    db_args = db_combined,
                    db = mdb$db), class = "mfdb")
                mfdb_send(mdb_public, "SET search_path TO ", paste('public', 'pg_temp', sep =","))
                mfdb_update_schema(mdb_public, target_version = 4)

                # Create new schema, to known state
                mfdb_update_schema(mdb, target_version = 7)

                # Copy data from old tables
                mfdb4_cs_taxonomy <- c("areacell", "sampling_type", "data_source", "index_type", "tow", "vessel")
                mfdb4_measurement_tables <- c('survey_index', 'division', 'sample', 'predator', 'prey')
                for (table_name in c(mfdb4_cs_taxonomy, mfdb4_measurement_tables)) {
                    cols <- mfdb_fetch(mdb, "SELECT column_name",
                        " FROM information_schema.columns",
                        " WHERE table_schema = 'public'",
                        " AND table_name = ", sql_quote(table_name),
                        NULL
                    )[,1]
                    cols <- cols[cols != "case_study_id"]

                    mfdb_send(mdb,
                        "INSERT INTO ", table_name,
                        " (", paste(cols, collapse = ","), ")",
                        " SELECT ", paste(cols, collapse = ","),
                        " FROM public.", table_name,
                        if (table_name == 'prey')
                            c(" WHERE predator_id IN (SELECT DISTINCT predator_id FROM public.predator WHERE case_study_id = ", old_case_study_id, ")")
                        else if (table_name == 'index_type')
                            c(" WHERE case_study_id = ", old_case_study_id, " AND index_type_id != 99999")
                        else
                            c(" WHERE case_study_id = ", old_case_study_id),
                        "");

                    # Ensure the sequence is up to date for the table
                    table_primary_key <- paste0(table_name, '_id')
                    mfdb_fetch(mdb,
                        "SELECT pg_catalog.setval(pg_get_serial_sequence('", table_name, "', '", table_primary_key, "'), MAX(", table_primary_key, "))",
                        " FROM ", table_name)
                }
            }
        }
    } else if (mfdb_is_sqlite(mdb)) {
        RSQLite::initRegExp(mdb$db)
        mfdb_send(mdb, "PRAGMA foreign_keys = ON;")

        # A sqlite database doesn't have separate schema
        if (destroy_schema) {
            if (mdb$db_args$dbname != ':memory:') stop('TODO: Delete all tables')
            mdb$logger$info(paste0("Schema ", mdb$schema, " removed, connect again to repopulate DB."))
            dbDisconnect(mdb$db)
            return(invisible(NULL))
        }
    }

    # Now we've done any data fetching, make sure our schema is up-to-date.
    mfdb_update_schema(mdb)

    if (mdb$schema == 'examples') {
        # Add data used in examples
        mfdb_populate_example_data(mdb)
    }

    invisible(mdb)
}

# Create indexes if not already there
mfdb_finish_import <- function(mdb) {
    if (!exists('index_created', where = mdb$state)) {
        if (mfdb_is_postgres(mdb)) {
            tables <- mfdb_fetch(mdb,
                "SELECT table_name",
                " FROM information_schema.tables",
                " WHERE (table_schema IN ", sql_quote(mdb$schema, always_bracket = TRUE),
                " OR table_schema = (SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema()))",
                "")[, c(1)]
        } else {
            tables <- mfdb_fetch(mdb, "
                SELECT name
                  FROM sqlite_schema
                 WHERE type = 'table'
                ")[, c(1)]
        }
        for (t in tables) mfdb_send(mdb, "ANALYZE ", t)
        assign('index_created', TRUE, pos = mdb$state)
    }
}

# Stop it and tidy up
mfdb_disconnect <- function(mdb) {
    mfdb_finish_import(mdb) # Might have just been an import session
    dbDisconnect(mdb$db)
}
