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

    if (destroy_schema) {
        if (schema_count == 0) {
            mdb$logger$info(paste0("Schema ", mdb$schema, " does not exist. Doing nothing"))
        } else {
            mfdb_send(mdb, "SET search_path TO ", paste(mdb$schema, 'pg_temp', sep =","))
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
        mfdb_send(mdb, "CREATE SCHEMA ", mdb$schema)
        mfdb_send(mdb, "SET search_path TO ", paste(mdb$schema, 'pg_temp', sep =","))

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

    invisible(mdb)
}

# Create indexes if not already there
mfdb_finish_import <- function(mdb) {
    if (!exists('index_created', where = mdb$state)) {
        mfdb_create_indexes(mdb)
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

# Concatenate queries together and send to database
mfdb_send <- function(mdb, ..., result = "") {
    query <- paste0(c(...), collapse = "")

    if (class(mdb$db) == 'dbNull') {
        cat(query)
        cat("\n")
        if (is.function(result)) {
            result(mdb$ret_rows, 0)
            return(invisible(NULL))
        }
        if (result == "rowcount") return(mdb$ret_rowcount)
        if (result == "rows") return(mdb$ret_rows)
        return(mdb$ret_recordset)
    }

    res <- dbSendQuery(mdb$db, query)

    if (is.function(result)) {
        offset <- 0
        while (!DBI::dbHasCompleted(res)) {
            result(DBI::dbFetch(res, n = 1000), offset)
            offset <- offset + 1000
        }
        dbClearResult(res)
        return(invisible(NULL))
    }
    if (result == "rowcount") {
        out <- DBI::dbGetRowsAffected(res)
        dbClearResult(res)
        return(out)
    }
    if (result == "rows") {
        out <- DBI::dbFetch(res)
        dbClearResult(res)
        return(out)
    }
    return(res)
}
mfdb_fetch <- function(mdb, ...) mfdb_send(mdb, ..., result = "rows")

# Insert a vector row or data.frame of rows into table_name
mfdb_insert <- function(mdb, table_name, data_in, returning = "", extra = c()) {
    insert_row <- function (r) {
        if (class(mdb$db) == 'dbNull') mdb$ret_rowcount <- mdb$ret_rows <- if (is.null(nrow(r))) 1 else nrow(r)
        mfdb_send(mdb, "INSERT INTO ", paste(table_name, collapse = ""),
            " (", paste(c(names(r), names(extra)), collapse=","), ") VALUES ",
            if (is.null(nrow(r)))
                sql_quote(c(r, extra), always_bracket = TRUE)
            else
                paste0(vapply(seq_len(nrow(r)), function (i) { sql_quote(c(r[i,], extra), always_bracket = TRUE) }, ""), collapse = ","),
            (if (nzchar(returning)) paste0(c(" RETURNING ", returning), collapse = "") else ""),
            result = ifelse(nzchar(returning), "rows", "rowcount"))
    }
    if (!is.data.frame(data_in)) {
        # Insert single row
        return(insert_row(data_in))
    } else if (nrow(data_in) == 0) {
        # Nothing to insert
        return(0)
    } else {
        # Insert rows in ~1000 row chunks, combine results
        res <- do.call(rbind, lapply(
            split(data_in, seq_len(nrow(data_in)) %/% 1000),
            insert_row))
        return(if (nzchar(returning)) res else sum(res))
    }
}

# Update a vector row or data.frame of rows from table_name
mfdb_update <- function(mdb, table_name, data_in, returning = "", extra = c(), where = list()) {
    id_col <- paste0(table_name, '_id')

    update_row <- function (r) {
        if (class(mdb$db) == 'dbNull') mdb$ret_rowcount <- mdb$ret_rows <- if (is.null(nrow(r))) 1 else nrow(r)
        out <- mfdb_send(mdb,
            "UPDATE ", table_name,
            " SET ",
            paste0(c(
                vapply(names(r)[names(r) != id_col], function (n) paste0(n, "=", sql_quote(r[[n]])), ""),
                vapply(names(extra), function (n) paste0(n, "=", sql_quote(extra[[n]])), ""),
                NULL
            ), collapse = ","),
            " WHERE ", table_name, "_id = ", sql_quote(r[[id_col]]),
            vapply(names(where), function (n) paste0(" AND ", n, "=", sql_quote(where[[n]])), ""),
            result = "rowcount")
        if (out != 1) stop("update should affect one row not ", out)
        return(out)
    }

    if (!is.data.frame(data_in)) {
        # Update single row
        return(update_row(data_in))
    } else if (nrow(data_in) == 0) {
        # Nothing to update
        return(0)
    } else {
        # Update all rows in turn
        return(sum(vapply(seq(1, nrow(data_in)), function(i) update_row(data_in[i,]), 0)))
    }
}

# Pre-load data into temporary table, return name of temporary table
mfdb_bulk_copy <- function(mdb, target_table, data_in, fn) {
    temp_tbl <- basename(tempfile(pattern = "temp_insert_", tmpdir = ""))

    if (nrow(data_in) == 0) {
        # No data, so don't make a temporary table
        return(fn("VALUES (NULL)"))
    }

    # Fetch table definition from DB, so we can recreate for temporary table
    cols <- mfdb_fetch(mdb, "SELECT column_name, data_type",
        " FROM information_schema.columns",
        " WHERE table_schema = ", sql_quote(mdb$schema),
        " AND table_name = ", sql_quote(target_table),
        NULL
    )
    rownames(cols) <- cols$column_name
    if (nrow(cols) == 0) stop("Didn't find table ", target_table)

    mdb$logger$info("Writing rows to temporary table")
    if (mfdb_table_exists(mdb, temp_tbl)) mfdb_send(mdb, "DROP TABLE ", temp_tbl)
    mfdb_send(mdb, "SET CLIENT_ENCODING TO 'LATIN1'") # Not sure.
    dbWriteTable(mdb$db, temp_tbl, data_in, row.names = FALSE,
        field.types = structure(cols[names(data_in), 'data_type'], names = names(data_in)))
    mfdb_send(mdb, "SET CLIENT_ENCODING TO 'UTF8'")

    res <- tryCatch(fn(temp_tbl), error = function (e) {
        tryCatch(mfdb_send(mdb, "DROP TABLE ", temp_tbl), error = function (e) NULL)
        stop(e)
    })
    mfdb_send(mdb, "DROP TABLE ", temp_tbl)
    res
}

# Temporarily remove constraints from a table
mfdb_disable_constraints <- function(mdb, table_name, code_block) {
    # Based on http://blog.hagander.net/archives/131-Automatically-dropping-and-creating-constraints.html

    # Get a list of constraints and the order to recreate them
    constraints <- mfdb_fetch(mdb,
        "SELECT relname AS table_name, conname AS name",
        ", pg_get_constraintdef(pg_constraint.oid) AS definition",
        " FROM pg_constraint",
        " INNER JOIN pg_class ON conrelid=pg_class.oid",
        " INNER JOIN pg_namespace ON pg_namespace.oid=pg_class.relnamespace",
        " WHERE nspname IN ", sql_quote(mdb$schema, always_bracket = TRUE),
        " AND relname IN ", sql_quote(table_name, always_bracket = TRUE),
        " ORDER BY CASE WHEN contype='f' THEN 0 ELSE 1 END DESC,contype DESC,nspname DESC,relname DESC,conname DESC",
        NULL)

    tryCatch({
        for(i in rev(seq_len(nrow(constraints)))) {
            mdb$logger$info(paste0("Removing constraint ", constraints[i, "table_name"], ".", constraints[i, "name"]))
            mfdb_send(mdb,
                "ALTER TABLE ", mdb$schema, ".", constraints[i, "table_name"],
                " DROP CONSTRAINT ", constraints[i, "name"], "")
        }
        code_block
    }, finally = {
        for(i in seq_len(nrow(constraints))) {
            mdb$logger$info(paste0("Reinstating constraint ", constraints[i, "table_name"], ".", constraints[i, "name"]))
            mfdb_send(mdb,
                "ALTER TABLE ", mdb$schema, ".", constraints[i, "table_name"],
                " ADD CONSTRAINT ", constraints[i, "name"], " ", constraints[i, "definition"])
        }
    })
}

# Do the given tables already exist?
mfdb_table_exists <- function(mdb, table_name) {
    mfdb_fetch(mdb,
        "SELECT COUNT(*)",
        " FROM information_schema.tables",
        " WHERE (table_schema IN ", sql_quote(mdb$schema, always_bracket = TRUE),
        " OR table_schema = (SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema()))",
        " AND table_name IN ", sql_quote(table_name, always_bracket = TRUE))[, c(1)] > 0
}

mfdb_create_table <- function(mdb, name, desc, cols = c(), keys = c()) {
    items <- matrix(c(
        cols,
        unlist(lapply(keys, function (k) c(k, "", "")))
    ), nrow = 3)

    row_to_string <- function (i) {
        paste0("    ",
            items[1,i],
            (if (nzchar(items[2,i])) paste("\t", items[2,i])),
            (if (i == ncol(items)) "" else ","),
            (if (nzchar(items[3,i])) paste("\t--", items[3,i])),
            "\n")
    }

    mfdb_send(mdb,
        if (nzchar(desc)) paste0("-- ", desc, "\n", collapse = ""),
        "CREATE TABLE ", name, " (\n",
        vapply(1:ncol(items), row_to_string, ""),
        ")")
    if (nzchar(desc)) mfdb_send(mdb,
        "COMMENT ON TABLE ", name,
        " IS ", sql_quote(desc))
    for (i in 1:ncol(items)) {
        if (nzchar(items[3,i])) mfdb_send(mdb,
            "COMMENT ON COLUMN ", name, ".", items[1,i],
            " IS ", sql_quote(items[3,i]))
    }
}

# Create accum & final functions, turn into aggregate function
mfdb_create_aggregate <- function(mdb, func_name, accum_body, final_body,
        init_cond = '{0,0}',
        input_type = c("numeric", "numeric"),
        state_type = "numeric[2]",
        return_type = "numeric") {

    # Make sure all this happens in the selected schema
    func_name <- paste(mdb$schema, func_name, sep = ".")

    mfdb_send(mdb,
        "CREATE OR REPLACE FUNCTION ", func_name, "_accum(",
        "p ", state_type,
        ", ", paste0(
            c("n", "n"),
            seq_len(length(input_type)),
            c(" ", " "),
            input_type,
            collapse = ", "),
        ") RETURNS ", state_type, " AS ", accum_body, " IMMUTABLE;");

    mfdb_send(mdb,
        "CREATE OR REPLACE FUNCTION ", func_name, "_final(",
        "p ", state_type,
        ") RETURNS ", return_type, " AS ", final_body, " IMMUTABLE;");

    # (re)create aggregate function
    mfdb_send(mdb,
        "DROP AGGREGATE IF EXISTS ", func_name,
        "(", paste0(input_type, collapse = ","), ")",
        NULL)
    mfdb_send(mdb,
        "CREATE AGGREGATE ", func_name,
        "(", paste0(input_type, collapse = ","), ")",
        " (SFUNC=", func_name, "_accum",
        ", STYPE=", state_type,
        ", FINALFUNC=", func_name, "_final",
        ", INITCOND=", sql_quote(init_cond),
        ");")

    invisible(NULL)
}

# Execute code block within a DB transaction, roll back on error, commit otherwise
mfdb_transaction <- function(mdb, transaction) {
    if (class(mdb$db) == 'dbNull') {
        # Short-circuit when in tests
        transaction
        return(invisible(TRUE))
    }

    mdb$logger$info("Starting transaction...")
    dbSendQuery(mdb$db, "BEGIN TRANSACTION")
    ret <- tryCatch(transaction, error = function (e) e)
    if ("error" %in% class(ret)) {
        mdb$logger$warn("Rolling back transaction...")
        tryCatch(dbRollback(mdb$db), error = function (e) NULL)
        stop(ret)
    }
    mdb$logger$info("Committing transaction...")
    dbCommit(mdb$db)
    invisible(TRUE)
}
