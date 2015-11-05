mfdb_unaggregated <- function (omitNA = FALSE) {
    group <- structure(list(omitNA = omitNA),
            class = c("mfdb_unaggregated", "mfdb_aggregate"))
    group
}

select_clause.mfdb_unaggregated <- select_clause.numeric

where_clause.mfdb_unaggregated <- function(mdb, x, col, outputname) {
    if (x$omitNA) paste0(col, " IS NOT NULL") else c()
}

agg_summary.mfdb_unaggregated <- function(mdb, x, col, outputname, data, sample_num) {
    if (is.null(data[[outputname]])) {
        if (identical(names(data), c('bssample')) || nrow(data) == 0) {
            # No data anyway, so nothing to return
            return(list())
        }
        stop("Column ", outputname, " missing from data")
    }
    vals <- unique(data[[outputname]])
    as.list(structure(vals, names = vals))
}
