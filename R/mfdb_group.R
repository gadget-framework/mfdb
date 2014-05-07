mfdb_group <- function (...) {
    structure(list(...),
            class = "mfdb_group")
}

# Denormalise the nesting and convert into a list of strings
denormalize <- function(group, prefix = "", collapse = "=") UseMethod("denormalize")
denormalize.mfdb_group <- function (group, prefix = "", collapse = "=") {
    unlist(lapply(1:length(group), function (i) {
        labels <- if (is.null(names(group))) rep("", length(group)) else names(group)
        lapply(group[[i]], function (value) {
            paste0(c(
                if (nchar(labels[[i]]) > 0) labels[[i]] else paste0(prefix, i),
                value), collapse = collapse)
        })
    }))
}
