mfdb_unaggregated <- function (omitNA = FALSE) {
    group <- structure(list(omitNA = omitNA),
            class = c("mfdb_unaggregated", "mfdb_aggregate"))
}

select_clause.mfdb_unaggregated <- function(x, col, outputname) {
    paste0(col, " AS ", outputname)
}

where_clause.mfdb_unaggregated <- function(x, col, outputname) {
    if (x$omitNA) paste0(col, " IS NOT NULL") else c()
}
