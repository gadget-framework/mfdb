# list(
#     list(camels = c(1,2), aardvarks = c(3,4)),
#     list(camels = c(2,2), aardvarks = c(4,4))
# )
mfdb_group <- function (...) {
    #TODO: Check each value is a vector
    group <- structure(list(...),
            class = "mfdb_group")

    invisible(group)
}

# Shortcut, return group of form prefix1 = c(1,2)
mfdb_group_numbered <- function (prefix, ...) {
    # Set label for items based on prefix
    items <- list(...)
    names(items) <- mapply(function (i) {
        paste0(prefix, i)
    }, seq_along(items))

    do.call(mfdb_group, items)
}

mfdb_bootstrap_group <- function (count, group) {
    if (class(group) != 'mfdb_group') {
        stop("Second argument should be a mfdb_group")
    }
    if (count < 1) {
        stop("Count should be equal or greater than 1")
    }

    bs_group <- structure(
            lapply(1:count, function(i) { lapply(group, function (g) { if (length(g) == 1) g else sample(g, replace = TRUE) }) }),
            class = "mfdb_bootstrap_group")
    invisible(bs_group)
}

# Denormalise the nesting and convert into a list of strings
denormalize <- function(group, samp_count = 0) UseMethod("denormalize")
# Break down each sample
denormalize.mfdb_bootstrap_group <- function (group, samp_count = 0) {
    do.call(rbind, lapply(1:length(group), function (i) {
        denormalize.mfdb_group(group[[i]], samp_count = i)
    }))
}
# Break down nested vectors into data.frame
denormalize.mfdb_group <- function (group, samp_count = 0) {
    do.call(rbind, lapply(1:length(group), function (i) {
        data.frame(
            samp = samp_count,
            key = names(group)[[i]],
            value = I(group[[i]]))
    }))
}
