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

sql_create_index <- function(table, cols) {
   paste0(c("CREATE INDEX ON ", table, " (", paste0(cols, collapse = ","), ")"), collapse = "")
}
