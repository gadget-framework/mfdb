mfdb_unaggregated <- function (omitNA = FALSE, like = c(), not_like = c()) {
    group <- structure(list(omitNA = omitNA, like = like, not_like = not_like),
            class = c("mfdb_unaggregated", "mfdb_aggregate"))
    group
}

select_clause.mfdb_unaggregated <- select_clause.numeric

gen_likes <- function(col, conditions, operator) {
    if (length(conditions) == 0) return(c())

    # Look up in taxonomy
    lookup <- gsub('(.*\\.)|_id', '', col)
    if ((lookup %in% c(mfdb_taxonomy, mfdb_cs_taxonomy))) {
        return(paste0(
            "(", col, " IN ",
            "(SELECT ", lookup, "_id FROM ", lookup, " WHERE ",
            paste0("name ", operator, sql_vquote(conditions), collapse = " OR "),
            "))"))
    }

    # No taxonomy
    return(paste0("(", paste0(col, operator, sql_vquote(conditions), collapse = " OR "), ")"))
}

where_clause.mfdb_unaggregated <- function(mdb, x, col, outputname, group_disabled = FALSE) {
    lookup <- gsub('(.*\\.)|_id', '', col)

    c(
        if (x$omitNA) paste0(col, " IS NOT NULL") else c(),
        gen_likes(col, x$like, " LIKE "),
        gen_likes(col, x$not_like, " NOT LIKE "),
        NULL
    )
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
