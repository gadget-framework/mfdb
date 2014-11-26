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

# Generic for turning "col == x" into a SQL SELECT condition
select_clause <- function(x, col, outputname) {
    if (is.null(x)) return("")
    UseMethod("select_clause")
}

# Generic for turning "col == x" into a SQL WHERE condition
where_clause <- function(x, col) {
    if (is.null(x)) return("")
    UseMethod("where_clause")
}

# Simple case for vectors, first checked to see if there's a lookup
select_clause.numeric <- function(x, col, outputname) {
    paste(col, "AS", outputname)
}
select_clause.character <- select_clause.numeric
where_clause.numeric <- function(x, col) {
    lookup <- gsub('(.*\\.)|_id', '', col)
    if (!(lookup %in% mfdb_taxonomy)) lookup <- NULL

    if (!is.vector(x)) return("")
    paste0(
        "(", col, " IN ",
        if (!is.null(lookup)) paste0("(SELECT ", lookup, "_id FROM ", lookup, " WHERE name IN "),
        sql_quote(x[!is.na(x)], always_bracket = TRUE),
        if (!is.null(lookup)) ")",
        if (NA %in% x) paste0(" OR ", col, " IS NULL"),
        ")")
}
where_clause.character <- where_clause.numeric

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
