# Max exclusive
# mfdb_interval("length", c(10, 400, 1000))
# mfdb_interval("length", seq(10, 1000, by=10))
mfdb_interval <- function (prefix, vect, open_ended = FALSE) {
    if (length(vect) < 2) {
        stop("vect must at least be 2 items long (min & max)")
    }

    # Convert old T/F form into a list of upper/lower
    if (identical(TRUE, open_ended)) {
        open_ended = c('upper')
    } else if (identical(FALSE, open_ended)) {
        open_ended = c()
    }

    return(structure(vect,
        names = paste0(prefix, vect),
        open_ended = open_ended,
        class = c("mfdb_interval", "mfdb_aggregate")))
}

# Cast value to something that matches column
col_cast <- function (col) {
    lookup <- gsub('(.*\\.)|_id', '', col)
    # NB: Not exhaustive, but main thing to avoid is comparing REAL to NUMERIC types
    ifelse(lookup == "species", "::BIGINT", ifelse(lookup == "year", "::INT", "::REAL"))
}

# Generate CASE statement to pick correct group for value
select_clause.mfdb_interval <- function(mdb, x, col, outputname, group_disabled = FALSE) {
    sorted <- sort(x, decreasing = FALSE)

    # Shift names up by one, ignore minimum value (only relevant for filters)
    sorted <- structure(
        sorted[2:length(sorted)],
        names = names(sorted)[1:length(sorted) - 1])

    paste("CASE",
        paste("WHEN",
            col, "<", paste0(sorted, col_cast(col)), "THEN",
            vapply(names(sorted), sql_quote, ""), collapse = " "),
        "ELSE", sql_quote(tail(names(sorted), 1)),  # If upper is open ended, remainder is bunged in with final group
        "END AS", outputname)
}

# Ensure value is within range specified
where_clause.mfdb_interval <- function(mdb, x, col, outputname, group_disabled = FALSE) {
    c(
        if (!('lower' %in% attr(x, 'open_ended'))) paste(col, ">=", paste0(sql_quote(min(x)), col_cast(col))),
        if (!('upper' %in% attr(x, 'open_ended'))) paste(col, "<", paste0(sql_quote(max(x)), col_cast(col))),
        NULL)
}

# Return a list of the form "group" = c("min", "max"), as required by gadget_file
agg_summary.mfdb_interval <- function(mdb, x, col, outputname, data, sample_num) {
    out <- mapply(function (curVal, nextVal) {
        structure(
            call("seq", curVal, nextVal - 1),
            min = curVal,
            max = nextVal)
    }, x[1:length(x) - 1], x[2:length(x)], SIMPLIFY = FALSE)

    # Annotate top/bottom groups if they're open_ended
    if ('lower' %in% attr(x, 'open_ended')) attr(out[[1]], 'min_open_ended') <- TRUE
    if ('upper' %in% attr(x, 'open_ended')) attr(out[[length(out)]], 'max_open_ended') <- TRUE

    return(out)
}
