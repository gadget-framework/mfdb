# Return v as a '-quoted SQL string
sql_quote <- function(v) {
    paste0("'", gsub("'", "''", v) ,"'")
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

# Turn vector into a SQL IN condition, NA = NULL, optionally go via a lookup table.
sql_col_condition <- function(col, v, lookup = NULL) {
    if (!is.vector(v)) return("")
    paste(
        "AND (", col, "IN (",
        if (!is.null(lookup)) paste(
            "SELECT", sub('^[a-z]+\\.', '', col), "FROM", lookup, "WHERE code IN ("),
        paste(
            sapply(v[!is.na(v)], function (x) { sql_quote(x) }),
            collapse = ","),
        if (!is.null(lookup)) ")",
        ")",
        if (NA %in% v) paste("OR", col, "IS NULL"),
        ")")
}

# Generate interval condition given 2 values
sql_interval_condition <- function(col, int_group, min_exclusive = FALSE, max_exclusive = FALSE) {
    if(is.null(int_group)) return("")
    paste("AND", col, if (min_exclusive) ">" else ">=", sql_quote(int_group$int_min),
          "AND", col, if (max_exclusive) "<" else "<=", sql_quote(int_group$int_max))
}

# Turn mfdb_group into a temporary table to join to
group_to_table <- function(db, table_name, group, datatype = "INT", save_tables = FALSE) {
    #TODO: This error message provides table name, not parameter
    if (is.null(group)) stop(paste("You must provide a mfdb_group for", table_name))
    #TODO: Assign random ID attribute to group, use this as table name or re-use table if it already has one
    # Remove the table if it exists, and recreate it
    tryCatch(dbSendQuery(db, paste("DROP TABLE", table_name)), error = function (e) {})
    dbSendQuery(db, paste(
            "CREATE",
            (if (!save_tables) "TEMPORARY"),
            "TABLE",
            table_name,
            "(sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value ", datatype,
            ")"))

    # Flatten out into multiple key:value rows, populate table in one hit
    group_table <- denormalize(group)
    by(group_table, 1:nrow(group_table), function(v) {
        #NB: Once we upgrade postgresql can use multi-row insert form
        dbSendQuery(db, paste0("INSERT INTO ", table_name, " (sample, name, value) VALUES (", sql_quote(v[[1]]), ",", sql_quote(v[[2]]), ",", sql_quote(v[[3]]), ")"))
    })
}

# Return SQL to create a table, arguments of form
#  * Name of table
#  * Description of table
#  * Column definition
#  * Description of column
#  (extra columns as required)
sql_create_table <- function(name, desc, ...) {
    cols <- matrix(c(...), nrow = 2)
    row_to_string <- function (i) {
        paste0("    ",
            cols[1,i],
            (if (i == ncol(cols)) "" else ","),
            (if (nzchar(cols[2,i])) paste("\t--", cols[2,i])),
            "\n")
    }

    paste0(
        if (nzchar(desc)) paste0("-- ", desc, "\n", collapse = ""),
        "CREATE TABLE ", name, " (\n",
        paste(sapply(1:ncol(cols), row_to_string), collapse = ""),
        ")")
}
