# Create interval based on step value, rather than explicit boundaries
mfdb_step_interval <- function (prefix, by, from = 0, to = NULL, open_ended = FALSE) {
        return(structure(
            list(prefix = prefix, by = by, from = from, to = to),
            open_ended = open_ended,
            class = c("mfdb_step_interval", "mfdb_aggregate")))
}

# Use integer division to pick correct group
select_clause.mfdb_step_interval <- function(mdb, x, col, outputname) {
    #TODO: add x$from somewhere to fudge steps
    val <- paste0("greatest(floor(", col, ")::integer, ", sql_quote(x$from), ")")
    if (!is.null(x$to)) val <- paste0("least(", val, ", ", sql_quote(x$to), ")")

    paste0(sql_quote(x$prefix),
        " || (", val, " / ", sql_quote(x$by), ") * ", sql_quote(x$by),
        " AS ", outputname)
}

# Ensure value is within range specified
where_clause.mfdb_step_interval <- function(mdb, x, col, outputname) {
    c(
        paste(col, ">=", sql_quote(x$from)),
        if (!is.null(x$to) && !attr(x, 'open_ended')) paste(col, "<", sql_quote(x$to)),
        NULL)
}

# Return a list of the form "group" = c("min", "max"), as required by gadget_file
agg_summary.mfdb_step_interval <- function(mdb, x, col, outputname, data) {
    if (!is.null(x$to)) {
        to <- x$to
    } else {
        # Work out the smallest value on sequence bigger than the largest value returned
        if (is.null(data[[outputname]])) {
            stop("Column ", outputname, " missing from data")
        }
        to <- max(data[[outputname]])
        to <- to - to %% x$by + x$by
    }
    out <- seq(from = x$from, to = to, by = x$by)
    names(out) <- paste0(x$prefix, out)
    mapply(c, out[1:length(out) - 1], out[2:length(out)], SIMPLIFY = FALSE)
}
