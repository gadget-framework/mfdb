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
denormalize <- function(group) UseMethod("denormalize")
denormalize.mfdb_group <- function (group) {
    unlist(lapply(1:length(group), function (i) {
        lapply(group[[i]], function (value) { c(names(group)[[i]], value) })
    }), recursive = FALSE)
}
