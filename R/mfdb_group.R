mfdb_group <- function (prefix, ...) {
    group <- structure(list(...),
            prefix = prefix,
            class = "mfdb_group")

    # If elements of our list don't have a label, set one
    if (is.null(names(group))) names(group) <- rep("", length(group))
    names(group) <- mapply(function (i, x) {
        if (nchar(x) > 0) x else paste0(prefix, i)
    }, seq_along(group), names(group))
    group
}

# Denormalise the nesting and convert into a list of strings
denormalize <- function(group, bootstrap = 0) UseMethod("denormalize")
denormalize.mfdb_group <- function (group, bootstrap = 0) {
    # Apply function to each members of list, return as a combined list
    moosh <- function(x, fun) {
        unlist(lapply(x, fun), recursive = FALSE)
    }

    moosh(if (bootstrap > 0) 1:bootstrap else 0, function (bs) {
        moosh(1:length(group), function (i) {
            lapply(
                if (bs > 0 & length(group[[i]]) > 1) sample(group[[i]], replace = TRUE) else group[[i]],
                function (value) { c(bs, names(group)[[i]], value) })
        })
    })
}

# Numeric intervals, for length e.g.
mfdb_group_interval <- function (prefix, int_min, int_max, int_step = 20) {
    group <- structure(list(
                int_min = int_min,
                int_max = int_max,
                int_step = int_step),
            prefix = prefix,
            class = "mfdb_group_interval")
    group
}
