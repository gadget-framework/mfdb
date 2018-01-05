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

# Generate CASE statement to pick correct group for value
select_clause.mfdb_interval <- function(mdb, x, col, outputname, group_disabled = FALSE) {
    sorted <- sort(x, decreasing = FALSE)

    # Shift names up by one, ignore minimum value (only relevant for filters)
    sorted <- structure(
        sorted[2:length(sorted)],
        names = names(sorted)[1:length(sorted) - 1])

    paste("CASE",
        paste("WHEN",
            col, "<", sorted, "THEN",
            vapply(names(sorted), sql_quote, ""), collapse = " "),
        "ELSE", sql_quote(tail(names(sorted), 1)),  # If upper is open ended, remainder is bunged in with final group
        "END AS", outputname)
}

# Ensure value is within range specified
where_clause.mfdb_interval <- function(mdb, x, col, outputname, group_disabled = FALSE) {
    c(
        if (!('lower' %in% attr(x, 'open_ended'))) paste(col, ">=", sql_quote(min(x))),
        if (!('upper' %in% attr(x, 'open_ended'))) paste(col, "<", sql_quote(max(x))),
        NULL)
}

# Return a list of the form "group" = c("min", "max"), as required by gadget_file
agg_summary.mfdb_interval <- function(mdb, x, col, outputname, data, sample_num) {
    return(mapply(function (curVal, nextVal) {
        structure(
            call("seq", curVal, nextVal - 1),
            min = curVal,
            max = nextVal)
    }, x[1:length(x) - 1], x[2:length(x)], SIMPLIFY = FALSE))
}
