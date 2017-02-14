is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# Create interval based on step value, rather than explicit boundaries
mfdb_step_interval <- function (prefix, by, from = 0, to = NULL, open_ended = FALSE) {
    if(!is.wholenumber(by)) {
        stop("Non-integer intervals not supported---by must be a whole number")
    }

    # Convert old T/F form into a list of upper/lower
    if (identical(TRUE, open_ended)) {
        open_ended = c('upper')
    } else if (identical(FALSE, open_ended)) {
        open_ended = c()
    }

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
        if (!is.null(x$from) && !('lower' %in% attr(x, 'open_ended'))) paste(col, ">=", sql_quote(x$from)),
        if (!is.null(x$to) && !('upper' %in% attr(x, 'open_ended'))) paste(col, "<", sql_quote(x$to)),
        NULL)
}

# Return a list of the form "group" = c("min", "max"), as required by gadget_file
agg_summary.mfdb_step_interval <- function(mdb, x, col, outputname, data, sample_num) {
  if (!is.null(x$to)) {
    to <- x$to
  } else {
    # Find the biggest grouping in the data, go one up
    if (is.null(data[[outputname]])) {
      stop("Column ", outputname, " missing from data")
    }
    to <- max(as.integer(gsub(paste0("^", x$prefix), "", data[[outputname]]))) + x$by
  }
  
  if('upper' %in% attr(x, 'open_ended')){
    ## go one up
    to <- to + x$by
  }
  
  # Vector of groupings
  out <- seq(from = x$from, to = to, by = x$by)
  names(out) <- paste0(x$prefix, out)
  
  # Expand this to have min/max values
  return(mapply(function (curVal, nextVal) {
    structure(
      call("seq", curVal, nextVal - 1),
      min = curVal,
      max = nextVal)
  }, out[1:length(out) - 1], out[2:length(out)], SIMPLIFY = FALSE))
}
