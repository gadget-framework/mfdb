mfdb_group <- function (...) {
    structure(list(...),
            class = "mfdb_group")
}

# Denormalise the nesting and convert into a list of strings
denormalize <- function(group, prefix = "") UseMethod("denormalize")
denormalize.mfdb_group <- function (group, prefix = "") {
    labels <- if (is.null(names(group))) rep("", length(group)) else names(group)
    unlist(lapply(1:length(group), function (i) {
        lapply(group[[i]], function (value) {
            c(
                if (nchar(labels[[i]]) > 0) labels[[i]] else paste0(prefix, i),
                value)
        })
    }), recursive = FALSE)
}
