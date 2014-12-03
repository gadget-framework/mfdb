# Return v as a '-quoted SQL string
sql_quote <- function(v, always_bracket = FALSE, always_quote = FALSE) {
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
        paste0("(", paste0(sql_vquote(v, always_quote = always_quote), collapse = ","), ")")
    }
}
sql_vquote <- Vectorize(sql_quote)

sql_create_index <- function(table, cols) {
   paste0(c("CREATE INDEX ON ", table, " (", paste0(cols, collapse = ","), ")"), collapse = "")
}
