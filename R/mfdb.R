#        areas = c(),	# c('101', '101:1001'), e.g. Will group at most granular
#        timesteps = mfdb_group("ts", c(1,2,3),c(4,5,6)), groupings of months,
#        todo = NULL) {
mfdb <- function(db_connection = NULL,
                 save_temp_tables = FALSE,
                 create_schema = FALSE,
                 defaultparams = list()) {
    if (is.null(db_connection)) {
        db_connection <- (function(hosts) {
            for (host in hosts) {
                conn <- tryCatch(dbConnect(PostgreSQL(), dbname = "mf", host = host), error = function (e) NULL)
                if (!is.null(conn)) return(conn)
            }
            stop("Could not find a local mf database")
        })(c("/tmp/pg_mfdb", "/tmp", "/var/tmp", "localhost"))
    }
    mdb <- structure(list(
            logger = getLogger('mfdb'),
            defaultparams = c(defaultparams, list(
                    timesteps = mfdb_group(year = c(1,2,3,4,5,6,7,8,9,10,11,12)))),
            save_temp_tables = save_temp_tables,
            db = db_connection), class = "mfdb")

    mfdb_update_schema(mdb, read_only = !create_schema)
    mfdb_update_taxonomy(mdb)

    invisible(mdb)
}

# Perform query and return all results
mfdb_fetch <- function(mfdb, ...) {
    res <- dbSendQuery(mfdb$db, paste0(..., collapse = ""))
    out <- dbFetch(res)
    dbClearResult(res)
    return(out)
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
