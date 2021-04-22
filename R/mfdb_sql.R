# Return v as a '-quoted SQL string
sql_quote <- function(v, always_bracket = FALSE, always_quote = FALSE, brackets = "()") {
    if (length(v) == 0) {
        stop("Cannot sql_quote empty vector")
    }
    if (!always_bracket && length(v) == 1) {
        if (is.na(v) || is.null(v)) {
            "NULL"
        } else if (!always_quote && is.numeric(v)) {
            as.character(v)
        } else {
            paste0("'", gsub("'", "''", v) ,"'")
        }
    } else {
        paste0(
            substr(brackets, 1, 1),
            paste0(sql_vquote(v, always_quote = always_quote), collapse = ","),
            substr(brackets, 2, 2))
    }
}
sql_vquote <- Vectorize(sql_quote)

sql_create_index <- function(table, cols) {
   index_name <- paste(c("idx", table, cols), collapse = "_")
   paste0(c("CREATE INDEX ", index_name, " ON ", table, " (", paste0(cols, collapse = ","), ")"), collapse = "")
}

# Return dbnull / pg / sqlite or unknown
mfdb_db_backend <- function(mdb) {
    if (class(mdb$db) == 'dbNull') return('dbnull')

    drv_package <- attr(class(mdb$db_args$drv), 'package')
    if (drv_package %in% c('RPostgres', 'RPostgreSQL')) return('pg')
    if (drv_package %in% c('RSQLite')) return('sqlite')
    return('unknown')
}
mfdb_is_postgres <- function (mdb) mfdb_db_backend(mdb) == 'pg'
mfdb_is_sqlite <- function (mdb) mfdb_db_backend(mdb) == 'sqlite'

# Concatenate queries together and send to database
mfdb_send <- function(mdb, ..., result = "") {
    query <- paste0(c(...), collapse = "")

    if (class(mdb$db) == 'dbNull') {
        if (is.list(mdb$ret_rows) && !is.data.frame(mdb$ret_rows)) {
            # Find the first of ret_rows where the regex name matches query
            ret_rows <- mdb$ret_rows[vapply(names(mdb$ret_rows), function (x) grepl(x, query), FALSE)]
            if (length(ret_rows) == 0) {
                ret_rows <- data.frame()
            } else {
                ret_rows <- ret_rows[[1]]
            }
        } else {
            ret_rows <- mdb$ret_rows
        }
        cat(query)
        cat("\n")
        if (is.function(result)) {
            result(ret_rows, 0)
            return(invisible(NULL))
        }
        if (result == "rowcount") return(mdb$ret_rowcount)
        if (result == "rows") return(ret_rows)
        return(mdb$ret_recordset)
    }

    # Log query, and optionally the explain plan, enable with mdb$explain_plan <- TRUE
    mdb$logger$finest(query)
    if (isTRUE(mdb$explain_plan) && result == 'rows') {
        writeLines("-----------------------------")
        writeLines(query)
        res <- dbSendQuery(mdb$db, paste("EXPLAIN ANALYSE", query))
        while (!DBI::dbHasCompleted(res)) {
            writeLines(DBI::dbFetch(res)[[1]])
        }
        writeLines("-----------------------------")
        dbClearResult(res)
    }

    res <- dbSendQuery(mdb$db, query)
    on.exit(dbClearResult(res))

    if (is.function(result)) {
        offset <- 0
        while (offset == 0 || !DBI::dbHasCompleted(res)) {
            result(DBI::dbFetch(res, n = 1000), offset)
            offset <- offset + 1000
        }
        return(invisible(NULL))
    }
    if (result == "rowcount") {
        out <- DBI::dbGetRowsAffected(res)
        return(out)
    }
    if (result == "rows") {
        out <- DBI::dbFetch(res)
        return(out)
    }
    return(invisible(NULL))
}
mfdb_fetch <- function(mdb, ...) mfdb_send(mdb, ..., result = "rows")

# Insert a vector row or data.frame of rows into table_name
mfdb_insert <- function(mdb, table_name, data_in, returning = "", extra = c()) {
    insert_row <- function (r) {
        if (class(mdb$db) == 'dbNull') mdb$ret_rowcount <- mdb$ret_rows <- if (is.null(nrow(r))) 1 else nrow(r)
        out <- mfdb_send(mdb, "INSERT INTO ", paste(table_name, collapse = ""),
            " (", paste(c(names(r), names(extra)), collapse=","), ") VALUES ",
            if (is.null(nrow(r)))
                sql_quote(c(r, extra), always_bracket = TRUE)
            else
                paste0(vapply(seq_len(nrow(r)), function (i) { sql_quote(c(r[i,], extra), always_bracket = TRUE) }, ""), collapse = ","),
            (if (!mfdb_is_sqlite(mdb) && nzchar(returning)) paste0(c(" RETURNING ", returning), collapse = "") else ""),
            result = ifelse(!mfdb_is_sqlite(mdb) && nzchar(returning), "rows", "rowcount"))
        if (mfdb_is_sqlite(mdb) && nzchar(returning)) {
            out <- mfdb_fetch(mdb, "SELECT ", returning, " FROM ", table_name, " WHERE ROWID = LAST_INSERT_ROWID()")
        }
        out
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

    mdb$logger$debug("Writing rows to temporary table")

    tryCatch({
        mfdb_send(mdb, "SET CLIENT_ENCODING TO 'LATIN1'") # Not sure.
        mfdb_send(mdb, "SET search_path TO pg_temp")
        if (mfdb_table_exists(mdb, temp_tbl)) mfdb_send(mdb, "DROP TABLE ", temp_tbl)

        dbWriteTable(mdb$db, temp_tbl, data_in, row.names = FALSE,
            field.types = structure(cols[names(data_in), 'data_type'], names = names(data_in)))
    }, finally = {
        mfdb_send(mdb, "SET CLIENT_ENCODING TO 'UTF8'")
        mfdb_send(mdb, "SET search_path TO ", paste(mdb$schema, 'pg_temp', sep =","))
    })
    temp_tbl <- paste(c('pg_temp', temp_tbl), collapse = ".")

    on.exit(mfdb_send(mdb, "DROP TABLE ", temp_tbl), add = TRUE)
    fn(temp_tbl)
}

# Temporarily remove constraints from a table, assumes it's been wrapped in a transaction
# NB: This *has* to be called within mfdb_transaction()
mfdb_disable_constraints <- function(mdb, table_name, code_block) {
    # If we're not the owner of these tables, just execute code_block
    owner <- mfdb_fetch(mdb,
        "SELECT COUNT(*)",
        " FROM pg_tables",
        " WHERE tableowner = current_user",
        " AND schemaname IN ", sql_quote(mdb$schema, always_bracket = TRUE),
        " AND tablename IN ", sql_quote(table_name, always_bracket = TRUE),
        NULL)[1,1]
    if (owner == 0) {
        mdb$logger$debug("Not owner of table, so can't drop constraints")
        return(code_block)
    }

    # Get a list of constraints and the order to recreate them
    # Based on http://blog.hagander.net/archives/131-Automatically-dropping-and-creating-constraints.html
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
            mfdb_send(mdb,
                "ALTER TABLE ", mdb$schema, ".", constraints[i, "table_name"],
                " DROP CONSTRAINT ", constraints[i, "name"], "")
        }
        code_block
    }, finally = {
        for(i in seq_len(nrow(constraints))) {
            mfdb_send(mdb,
                "ALTER TABLE ", mdb$schema, ".", constraints[i, "table_name"],
                " ADD CONSTRAINT ", constraints[i, "name"], " ", constraints[i, "definition"])
        }
    })
}

# Do the given tables already exist?
mfdb_table_exists <- function(mdb, table_name, schema_name = mdb$schema) {
    if (mfdb_is_postgres(mdb) || class(mdb$db) == 'dbNull') return(mfdb_fetch(mdb,
        "SELECT COUNT(*)",
        " FROM information_schema.tables",
        " WHERE (table_schema IN ", sql_quote(schema_name, always_bracket = TRUE),
        " OR table_schema = (SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema()))",
        " AND table_name IN ", sql_quote(table_name, always_bracket = TRUE))[, c(1)] > 0)
    if (mfdb_is_sqlite(mdb)) return(mfdb_fetch(mdb, "
        SELECT COUNT(*)
          FROM sqlite_schema
         WHERE type = 'table'
           AND name IN ", sql_quote(table_name, always_bracket = TRUE), "
        ")[, c(1)] > 0)
    stop("Unknown DB type")
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
        "CREATE TABLE ", (if (mfdb_is_postgres(mdb)) paste0(mdb$schema, ".", name) else name), " (\n",
        vapply(1:ncol(items), row_to_string, ""),
        ")")

    if (mfdb_is_postgres(mdb)) {
        if (nzchar(desc)) mfdb_send(mdb,
            "COMMENT ON TABLE ", name,
            " IS ", sql_quote(desc))
        for (i in 1:ncol(items)) {
            if (nzchar(items[3,i])) mfdb_send(mdb,
                "COMMENT ON COLUMN ", name, ".", items[1,i],
                " IS ", sql_quote(items[3,i]))
        }
    }
}

# Create accum & final functions, turn into aggregate function
mfdb_create_aggregate <- function(mdb, func_name, accum_body, final_body,
        init_cond = '{0,0}',
        input_type = c("numeric", "numeric"),
        state_type = "numeric[2]",
        return_type = "numeric") {

    # Make sure all this happens in the selected schema
    func_dotted_name <- paste(mdb$schema, func_name, sep = ".")

    mfdb_send(mdb,
        "CREATE OR REPLACE FUNCTION ", func_dotted_name, "_accum(",
        "p ", state_type,
        ", ", paste0(
            c("n", "n"),
            seq_len(length(input_type)),
            c(" ", " "),
            input_type,
            collapse = ", "),
        ") RETURNS ", state_type, " AS ", accum_body, " IMMUTABLE;");

    mfdb_send(mdb,
        "CREATE OR REPLACE FUNCTION ", func_dotted_name, "_final(",
        "p ", state_type,
        ") RETURNS ", return_type, " AS ", final_body, " IMMUTABLE;");

    # (re)create aggregate function
    agg_count <- mfdb_fetch(mdb,
        "SELECT COUNT(*) FROM pg_proc",
        " WHERE LOWER(proname) = LOWER(", sql_quote(func_name), ")",
        " AND pronamespace = (",
            "SELECT oid FROM pg_namespace",
            " WHERE LOWER(nspname) = LOWER(", sql_quote(mdb$schema), "));")
    if (agg_count[1][1] > 0) mfdb_send(mdb,
        "DROP AGGREGATE IF EXISTS ", func_dotted_name,
        "(", paste0(input_type, collapse = ","), ")",
        NULL)
    mfdb_send(mdb,
        "CREATE AGGREGATE ", func_dotted_name,
        "(", paste0(input_type, collapse = ","), ")",
        " (SFUNC=", func_dotted_name, "_accum",
        ", STYPE=", state_type,
        ", FINALFUNC=", func_dotted_name, "_final",
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

    mdb$logger$debug("Starting transaction...")
    tryCatch(dbBegin(mdb$db), error = function (e) {
        if (grepl('unable to find an inherited method for function.*dbBegin', e$message)) {
            # Old PostgreSQL DBI driver is missing dbBegin
            dbSendQuery(mdb$db, "BEGIN TRANSACTION")
        } else {
            stop(e)
        }
    })
    if (mfdb_is_postgres(mdb)) mfdb_send(mdb, "SET search_path TO ", paste(mdb$schema, 'pg_temp', sep =","))
    ret <- tryCatch(transaction, interrupt = function (e) e, error = function (e) e)
    if ("interrupt" %in% class(ret)) {
        mdb$logger$warn("Interrupted, rolling back transaction...")
        tryCatch(dbRollback(mdb$db), error = function (e) NULL)
        # NB: I can't see a way to re-throw ret, signalCondition(ret) would
        #     only work for another tryCatch block, not global handlers.
        #     rlang::interrupt() can do this properly, but the subtle difference
        #     doesn't seem worth the dependency.
        stop("Interrupted")
    }
    if ("error" %in% class(ret)) {
        mdb$logger$warn("Rolling back transaction...")
        tryCatch(dbRollback(mdb$db), error = function (e) NULL)
        stop(ret)
    }
    mdb$logger$debug("Committing transaction...")
    dbCommit(mdb$db)
    invisible(TRUE)
}
