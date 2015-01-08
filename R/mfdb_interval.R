# Max exclusive
# mfdb_interval("length", c(10, 400, 1000))
# mfdb_interval("length", seq(10, 1000, by=10))
mfdb_interval <- function (prefix, vect, open_ended = FALSE) {
    if (length(vect) < 2) {
        stop("vect must at least be 2 items long (min & max)")
    }
    return(structure(vect,
        names = paste0(prefix, vect),
        open_ended = open_ended,
        class = c("mfdb_interval", "mfdb_aggregate")))
}

# Generate CASE statement to pick correct group for value
select_clause.mfdb_interval <- function(mdb, x, col, outputname) {
    sorted <- sort(x, decreasing = TRUE)
    if (!attr(x, 'open_ended')) {
        # Assign stuff outside highest group to NULL
        names(sorted)[[1]] <- NA
    }
    paste("CASE",
        paste("WHEN",
            col, ">=", sorted, "THEN",
            vapply(names(sorted), sql_quote, ""), collapse = " ")
        , "END AS", outputname)
}

# Ensure value is within range specified
where_clause.mfdb_interval <- function(mdb, x, col, outputname) {
    c(
        paste(col, ">=", sql_quote(min(x))),
        if (!attr(x, 'open_ended')) paste(col, "<", sql_quote(max(x))),
        NULL)
}

# Return a list of the form "group" = c("min", "max"), as required by gadget_file
agg_summary.mfdb_interval <- function(mdb, x, data) {
    mapply(c, x[1:length(x) - 1], x[2:length(x)], SIMPLIFY = FALSE)
}
