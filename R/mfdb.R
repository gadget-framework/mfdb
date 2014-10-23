# Init mfdb object and open connection to database
mfdb <- function(case_study_name,
                 db_params = list(),
                 save_temp_tables = FALSE,
                 create_schema = FALSE,
                 defaultparams = list()) {
    logger <- getLogger('mfdb')

    # Try a selection of host strings until we connect to something
    db_params <- c(db_params, list(drv = PostgreSQL(), dbname = "mf"))
    db_guesses <- list(
        list(),
        list(host = "/tmp/pg_mfdb"),
        list(host = "/tmp"),
        list(host = "/var/tmp"),
        list(host = "localhost"))
    for (guess in db_guesses) {
        db_combined <- c(guess, db_params)[!duplicated(c(guess, db_params))]
        logger$info(paste0("Trying to connect to: ", capture.output(str(db_combined)), sep = ""))
        do.call(dbConnect, db_combined)
        db_connection <- tryCatch(do.call(dbConnect, db_combined), error = function (e) NULL)
        if (!is.null(db_connection)) break
    }
    if (is.null(db_connection)) {
        logger$error(paste0("Failed to connect, tried: ", capture.output(str(c(db_guesses, db_params))), sep = ""))
        stop("Could not find a local mf database")
    }

    # Look up case study ID via. data, since table might not be populated yet
    case_study_id <- case_study[case_study$name == case_study_name, 'id']
    if (length(case_study_id) != 1) {
        stop("Unknown case study ", case_study_name)
    }

    mdb <- structure(list(
            logger = logger,
            defaultparams = c(defaultparams, list(
                    timesteps = mfdb_group(year = c(1,2,3,4,5,6,7,8,9,10,11,12)))),
            save_temp_tables = save_temp_tables,
            case_study_id = case_study_id,
            state = new.env(),
            db = db_connection), class = "mfdb")

    mfdb_update_schema(mdb, read_only = !create_schema)
    mfdb_update_taxonomy(mdb)
    if (!create_schema) {
        # Assume indexes are already there if we shouldn't create schema
        assign('index_created', TRUE, pos = mdb$state)
    }

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

# Perform query and return all results
mfdb_fetch <- function(mdb, ...) {
    query <- paste0(c(...), collapse = "")
    mdb$logger$debug(query)
    res <- dbSendQuery(mdb$db, query)
    out <- dbFetch(res)
    dbClearResult(res)
    return(out)
}

# Send a query without the fetch
mfdb_send <- function(mdb, ...) {
    query <- paste0(c(...), collapse = "")
    mdb$logger$debug(query)
    res <- dbSendQuery(mdb$db, query)
}

# Insert a vector row or data.frame of rows into table_name
mfdb_insert <- function(mfdb, table_name, data_in, returning = "", extra = c()) {
    insert_row <- function (r) {
        res <- dbSendQuery(mfdb$db, paste0("INSERT INTO ", paste(table_name, collapse = ""),
            " (", paste(c(names(r), names(extra)), collapse=","), ") VALUES ",
            sql_quote(c(r, extra)),
            (if (nzchar(returning)) paste0(c(" RETURNING ", returning), collapse = "") else ""),
            "", collapse = ""))
        out <- if (nzchar(returning)) dbFetch(res) else dbGetRowsAffected(res)
        dbClearResult(res)
        return(out)
    }
    if (!is.data.frame(data_in)) {
        # Insert single row
        return(insert_row(data_in))
    } else {
        # Insert rows
        #TODO: Should be batching
        return(vapply(seq_len(nrow(data_in)), function (i) { insert_row(data_in[i,]) }, 0))
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
