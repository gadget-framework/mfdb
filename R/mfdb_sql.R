# Return v as a '-quoted SQL string
sql_quote <- function(v, always_bracket = FALSE) {
    if (!always_bracket && length(v) == 1) {
        if (is.na(v) || is.null(v)) {
            "NULL"
        } else if (is.numeric(v)) {
            as.character(v)
        } else {
            paste0("'", gsub("'", "''", v) ,"'")
        }
    } else {
        paste0("(", paste0(Vectorize(sql_quote)(v), collapse = ","), ")")
    }
}

# Turn mfdb_group into a temporary table to join to
group_to_table <- function(mdb, table_name, group, datatype = "INT", save_temp_tables = FALSE) {
    if (is.null(group)) stop(paste("You must provide a mfdb_group for", table_name))
    #TODO: Assign random ID attribute to group, use this as table name or re-use table if it already has one
    # Remove the table if it exists, and recreate it
    tryCatch(mfdb_send(mdb,
        "DROP ",
        "TABLE ", table_name), error = function (e) {
            mdb$logger$debug(paste("Ignored", e))
        })
    mfdb_send(mdb, paste(
            "CREATE",
            (if (!save_temp_tables) "TEMPORARY"),
            "TABLE",
            table_name,
            "(sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value ", datatype,
            ")"))

    # Flatten out into multiple key:value rows, populate table in one hit
    mfdb_insert(mdb, table_name, denormalize(group))
}

sql_create_index <- function(table, cols) {
   paste0(c("CREATE INDEX ON ", table, " (", paste0(cols, collapse = ","), ")"), collapse = "")
}
