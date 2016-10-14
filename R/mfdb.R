# Init mfdb object and open connection to database
mfdb <- function(case_study_name = "",
                 db_params = list(),
                 destroy_schema = FALSE,
                 save_temp_tables = FALSE) {
    logger <- getLogger('mfdb')

    # Try a selection of host strings until we connect to something
    db_params <- as.list(db_params)
    db_defaults <- list(dbname = "mf", drv = PostgreSQL())
    db_guesses <- list(
        list(host = "/tmp"),
        list(host = "/var/tmp"),
        list(host = "/var/run/postgresql"),
        list(host = "localhost"),
        list(host = "localhost", user = "mf", password = "mf"),
        list(host = "/tmp/pg_mfdb"))

    if (identical(db_params$host, 'mfdb.rhi.hi.is')) {
        # Enforce user
        if (!isTRUE(nzchar(db_params$user))) {
            db_params$user <- readline("Username: ")
        }
        # Enforce password
        if (!isTRUE(nzchar(db_params$password))) {
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

        logger$info(paste0(
            "Trying to connect to: ",
            paste(capture.output(str(db_combined)), collapse = "\n")))
        db_connection <- tryCatch(do.call(DBI::dbConnect, db_combined), error = function (e) e)
        if ("error" %in% class(db_connection)) {
            logger$info(paste0("Failed: ", paste(db_connection$message, collapse = "\n")))
        } else {
            break
        }
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
            db = db_connection), class = "mfdb")
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

    # Set search path for connection, so we look in correct schema
    mfdb_send(mdb, "SET search_path TO ", paste(mdb$schema, 'pg_temp', sep =","))

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
    if (schema_count == 0) {
        logger$info(paste0("No schema, creating ", mdb$schema))
        mfdb_send(mdb, "CREATE SCHEMA ", mdb$schema)

        # If schema didn't exist before, see if there's data to be had in the old public tables
        res <- tryCatch(
            mfdb_fetch(mdb,
                "SELECT case_study_id",
                " FROM public.case_study",
                " WHERE name = ", sql_quote(case_study_name)),
            error = function(e) c())
        if (length(res) == 1) {
            logger$info(paste0("Copying data from ", case_study_name))
            # A case study exists by this ID
            old_case_study_id <- res[1,1]

            # Upgrade the old database to a known state
            mfdb_send(mdb, "SET search_path TO public, pg_temp")
            mfdb_update_schema(mdb, target_version = 4)
            mfdb_send(mdb, "SET search_path TO ", paste(mdb$schema, 'pg_temp', sep =","))

            # Create new schema, to known state
            mfdb_update_schema(mdb, target_version = 5)
            mfdb_update_taxonomy(mdb)

            # Copy data from old tables
            mfdb4_cs_taxonomy <- c("areacell", "sampling_type", "data_source", "index_type", "tow", "vessel")
            mfdb4_measurement_tables <- c('survey_index', 'division', 'sample', 'predator', 'prey')
            for (table_name in c(mfdb4_cs_taxonomy, mfdb4_measurement_tables)) {
                cols <- mfdb_fetch(mdb, "SELECT column_name",
                    " FROM information_schema.columns",
                    " WHERE table_schema = ", sql_quote(mdb$schema),
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
                    else
                        c(" WHERE case_study_id = ", old_case_study_id),
                    "");
            }
        }
    }

    # Now we've done any data fetching, make sure our schema is up-to-date.
    mfdb_update_schema(mdb)
    mfdb_update_taxonomy(mdb)
    mfdb_update_cs_taxonomy(mdb)
    mfdb_update_functions(mdb)

    invisible(mdb)
}

# Create indexes if not already there
mfdb_finish_import <- function(mdb) {
    if (!exists('index_created', where = mdb$state)) {
        tables <- mfdb_fetch(mdb,
            "SELECT table_name",
            " FROM information_schema.tables",
            " WHERE (table_schema IN ", sql_quote(mdb$schema, always_bracket = TRUE),
            " OR table_schema = (SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema()))",
            "")[, c(1)]
        for (t in tables) mfdb_send(mdb, "ANALYZE ", t)
        assign('index_created', TRUE, pos = mdb$state)
    }
}

# Stop it and tidy up
mfdb_disconnect <- function(mdb) {
    mfdb_finish_import(mdb) # Might have just been an import session
    dbDisconnect(mdb$db)
}
