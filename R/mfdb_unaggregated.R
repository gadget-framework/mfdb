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

as.list.mfdb_unaggregated <- function(x, ...) {
    # Put out placeholder for now, need to restructure to get something useful
    list(X='X')
}
