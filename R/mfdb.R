# Init mfdb object and open connection to database
mfdb <- function(case_study_name,
                 db_params = list(),
                 destroy_schema = FALSE,
                 save_temp_tables = FALSE) {
    logger <- getLogger('mfdb')

    # Try a selection of host strings until we connect to something
    db_defaults = list(dbname = "mf", drv = PostgreSQL())
    db_guesses <- list(
        list(host = "/tmp"),
        list(host = "/var/tmp"),
        list(host = "/var/run/postgresql"),
        list(host = "localhost"),
        list(host = "localhost", user = "mf", password = "mf"),
        list(host = "/tmp/pg_mfdb"))
    for (guess in db_guesses) {
        db_combined <- c(db_params, guess, db_defaults)
        db_combined <- db_combined[!duplicated(names(db_combined))]
        logger$info(paste0(
            "Trying to connect to: ",
            paste(capture.output(str(db_combined)), collapse = "\n")))
        db_connection <- tryCatch(do.call(DBI::dbConnect, db_combined), error = function (e) NULL)
        if (!is.null(db_connection)) break
    }
    if (is.null(db_connection)) {
        stop("Could not find a local mf database")
    }

    # Create temporary mdb object and ensure we have a valid schema
    mdb <- structure(list(
            logger = logger,
            db = db_connection), class = "mfdb_temp")
    if (destroy_schema) {
        mfdb_destroy_schema(mdb)
        mdb$logger$info("Schema removed, connect again to repopulate DB.")
        dbDisconnect(mdb$db)
        return(invisible(NULL))
    }
    mfdb_update_schema(mdb)
    mfdb_update_taxonomy(mdb)

    # Look up case study ID
    res <- mfdb_fetch(mdb,
        "SELECT case_study_id",
        " FROM case_study",
        " WHERE name = ", sql_quote(case_study_name))
    if (length(res) == 1) {
        case_study_id <- res[1,1]
    } else {
        stop("Unknown case study ", case_study_name)
    }

    # Create full mdb object
    mdb <- structure(list(
            logger = logger,
            save_temp_tables = save_temp_tables,
            case_study_id = case_study_id,
            state = new.env(),
            db = db_connection), class = "mfdb")

    mfdb_update_cs_taxonomy(mdb)

    invisible(mdb)
}

# Create indexes if not already there
mfdb_finish_import <- function(mdb) {
    if (!exists('index_created', where = mdb$state)) {
        mfdb_create_indexes(mdb)
        mfdb_send(mdb, "ANALYZE")
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
        if (result == "rowcount") return(mdb$ret_rowcount)
        if (result == "rows") return(mdb$ret_rows)
        return(mdb$ret_recordset)
    }

    res <- dbSendQuery(mdb$db, query)

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

    # Fetch table definition from DB, so we can recreate for temporary table
    cols <- mfdb_fetch(mdb, "SELECT column_name, data_type",
        " FROM information_schema.columns",
        " WHERE table_schema = 'public'",
        " AND table_name = ", sql_quote(target_table),
        NULL
    )
    rownames(cols) <- cols$column_name
    if (nrow(cols) == 0) stop("Didn't find table ", target_table)

    mdb$logger$info("Writing rows to temporary table")
    tryCatch(mfdb_send(mdb, "DROP TABLE ", temp_tbl), error = function(e) {
        if(grepl("does not exist", e$message)) return();
        stop(e)
    })
    mfdb_send(mdb, "SET CLIENT_ENCODING TO 'LATIN1'") # Not sure.
    dbWriteTable(mdb$db, temp_tbl, data_in, row.names = FALSE,
        field.types = structure(cols[names(data_in), 'data_type'], names = names(data_in)))
    mfdb_send(mdb, "SET CLIENT_ENCODING TO 'UTF8'")

    tryCatch(fn(temp_tbl), finally = mfdb_send(mdb, "DROP TABLE ", temp_tbl))
}

# Temporarily remove constraints from a table
mfdb_disable_constraints <- function(mdb, table_name, code_block) {
    # Based on http://blog.hagander.net/archives/131-Automatically-dropping-and-creating-constraints.html

    # Get a list of constraints and the order to recreate them
    namespace <- "public"
    constraints <- mfdb_fetch(mdb,
        "SELECT conname AS name",
        ", pg_get_constraintdef(pg_constraint.oid) AS definition",
        " FROM pg_constraint",
        " INNER JOIN pg_class ON conrelid=pg_class.oid",
        " INNER JOIN pg_namespace ON pg_namespace.oid=pg_class.relnamespace",
        " WHERE nspname IN ", sql_quote(namespace, always_bracket = TRUE),
        " AND relname IN ", sql_quote(table_name, always_bracket = TRUE),
        " ORDER BY CASE WHEN contype='f' THEN 0 ELSE 1 END DESC,contype DESC,nspname DESC,relname DESC,conname DESC",
        NULL)

    tryCatch({
        mdb$logger$info(paste0("Removing constraints from ", table_name))
        for (cname in rev(constraints$name)) mfdb_send(mdb,
            "ALTER TABLE ", namespace, ".", table_name,
            " DROP CONSTRAINT ", cname, "")
        code_block
    }, finally = {
        mdb$logger$info(paste0("Reinstating constraints on ", table_name))
        for(i in 1:nrow(constraints)) mfdb_send(mdb,
            "ALTER TABLE ", namespace, ".", table_name,
            " ADD CONSTRAINT ", constraints[i, "name"], " ", constraints[i, "definition"])
    })
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

# Execute code block within a DB transaction, roll back on error, commit otherwise
mfdb_transaction <- function(mdb, transaction) {
    mdb$logger$info("Starting transaction...")
    dbSendQuery(mdb$db, "BEGIN TRANSACTION")
    ret <- tryCatch(transaction, error = function (e) { e })
    if ("error" %in% class(ret)) {
        mdb$logger$warn("Rolling back transaction...")
        dbRollback(mdb$db)
        stop(ret)
    }
    mdb$logger$info("Committing transaction...")
    dbCommit(mdb$db)
    invisible(TRUE)
}
