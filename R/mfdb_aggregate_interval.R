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
    sorted <- sort(x, decreasing = TRUE)
    final <- c()

    if ('upper' %in% attr(x, 'open_ended')) {
        # assign everything outside the highest group to the highest interval
        names(sorted)[1]<-names(sorted[2])
    } else {
        # Assign stuff outside highest group to NULL
        names(sorted)[[1]] <- NA
    }

    if ('lower' %in% attr(x, 'open_ended')) {
        # Final item should be a less-than instead
        final <- tail(sorted, 1)
        sorted <- head(sorted, -1)

        final <- paste("WHEN",
            col, "<", tail(sorted, 1), "THEN",
            sql_quote(names(final)), collapse = " ")
    }

    paste("CASE",
        paste("WHEN",
            col, ">=", sorted, "THEN",
            vapply(names(sorted), sql_quote, ""), collapse = " "),
        final,
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
