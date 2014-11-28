# Generics for mfdb_aggregates, called for each portion of the query
# Handle NULL here, grouping everything together
pre_query <- function(mdb, x, outputname) {
    if (is.null(x)) return(NULL)
    UseMethod("pre_query", x)
}

sample_clause <- function(x, col, outputname) {
    if (is.null(x)) return("0")
    UseMethod("sample_clause", x)
}

select_clause <- function(x, col, outputname) {
    if (is.null(x)) return(paste0("'all' AS ", outputname))
    UseMethod("select_clause", x)
}

from_clause <- function(x, col, outputname) {
    if (is.null(x)) return(c())
    UseMethod("from_clause", x)
}

where_clause <- function(x, col, outputname) {
    if (is.null(x)) return(c())
    UseMethod("where_clause", x)
}

# Add some do-nothing cases where definining the function is optional
pre_query.mfdb_aggregate <- function(mdb, x, outputname) NULL
sample_clause.mfdb_aggregate <- function(x, col, outputname) "0"
from_clause.mfdb_aggregate <- function(x, col, outputname) c()

# Numeric vectors, first checked to see if there's a lookup
pre_query.numeric <- pre_query.mfdb_aggregate
sample_clause.numeric <- sample_clause.mfdb_aggregate
select_clause.numeric <- function(x, col, outputname) {
    paste(col, "AS", outputname)
}
from_clause.numeric <- from_clause.mfdb_aggregate
where_clause.numeric <- function(x, col, outputname) {
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

# Character vectors work the same as numeric vector
pre_query.character     <- pre_query.numeric
sample_clause.character <- sample_clause.numeric
select_clause.character <- select_clause.numeric
from_clause.character   <- from_clause.numeric
where_clause.character  <- where_clause.numeric
