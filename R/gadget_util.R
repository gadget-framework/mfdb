fname <- function (dir, ...) {
    file.path(dir, paste0(c(...), collapse = ""))
}

component_replace <- function(gfile, newcomponent, namefn, component_name = "component") {
    # Iterate through, stop when we either get to the end or find a match
    newname <- namefn(newcomponent)
    for (i in seq_len(length(gfile$components) + 1)) {
        if (i > length(gfile$components)) break;
        if (namefn(gfile$components[[i]]) == newname) break;
    }

    gfile$components[[i]] <- newcomponent
    names(gfile$components)[[i]] <- component_name
    return(gfile)
}

# Make sure the data frame colums match what is required
compare_cols <- function (actual, expected) {
    if (is.null(expected)) return(invisible(NULL))

    # Fill NAs in expected with whatever we did get
    expected[is.na(expected)] <- actual[is.na(expected)]

    if (!isTRUE(all.equal(actual, expected))) {
        stop(paste(c(
            "Expected data to have columns '",
            paste(expected, collapse=","),
            "', not '",
            paste(actual, collapse=","),
            "'",
            NULL
            ), collapse = ""))
    }
    return(invisible(NULL))
}

