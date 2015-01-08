mfdb_unaggregated <- function (omitNA = FALSE) {
    group <- structure(list(omitNA = omitNA),
            class = c("mfdb_unaggregated", "mfdb_aggregate"))
    group
}

select_clause.mfdb_unaggregated <- function(mdb, x, col, outputname) {
    paste0(col, " AS ", outputname)
}

where_clause.mfdb_unaggregated <- function(mdb, x, col, outputname) {
    if (x$omitNA) paste0(col, " IS NOT NULL") else c()
}

agg_summary.mfdb_unaggregated <- function(mdb, x, col, data) {
    if (is.null(data[[col]])) {
        stop("Column ", col, " missing from data")
    }
    vals <- unique(data[[col]])
    as.list(structure(vals, names = vals))
}
