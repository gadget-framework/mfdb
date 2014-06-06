# Max exclusive
# mfdb_interval("length", c(10, 400, 1000))
# mfdb_interval("length", seq(10, 1000, by=10))
mfdb_interval <- function (prefix, vect) {
    if (length(vect) < 2) {
        stop("vect must at least be 2 items long (min & max)")
    }
    group <- structure(vect,
            names = paste0(prefix, vect),
            class = "mfdb_interval")
    invisible(group)
}

# Generate CASE statement to pick correct group for value
select_clause.mfdb_interval <- function(x, col, outputname) {
    sorted <- sort(x, decreasing = TRUE)
    paste(", CASE",
        paste("WHEN",
            col, ">=", sorted, "THEN",
            sql_quote(names(sorted)), collapse = " ")
        , "END AS", outputname)
}

# Ensure value is within range specified
where_clause.mfdb_interval <- function(x, col) {
    paste("AND", col, ">=", sql_quote(min(x)),
        "AND", col, "<", sql_quote(max(x)))
}

# Return a list of the form "group" = c("min", "max"), as required by gadget_file
as.list.mfdb_interval <- function(x, ...) {
    mapply(c, x[1:length(x) - 1], x[2:length(x)], SIMPLIFY = FALSE)
}
