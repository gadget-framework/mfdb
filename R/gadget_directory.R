# Initalise a gadget output directory, including creating it if necessary
gadget_directory <- function (dir) {
    if(!utils::file_test("-d", dir)) tryCatch(dir.create(dir), warning = function(w) {
        stop(paste("Could not create output directory:", conditionMessage(w)))
    })
    if(!utils::file_test("-d", dir)) stop(paste("Could not create output directory", dir))
    structure(list(
        dir = dir), class = c("gadget_directory"))
}

gadget_dir_write <- function(gd, obj) UseMethod("gadget_dir_write", obj)
gadget_dir_write.gadget_file <- function(gd, obj) {
    fh = file(file.path(gd$dir, obj$filename), "w")
    tryCatch(
        capture.output(print(obj), file = fh),
        finally = close(fh))
}

gadget_dir_write.gadget_likelihood_component <- function(gd, obj) {
    # Either replace component with matching name, or add to end
    insert_component <- function(comps, cname, n) {
        for (i in 1:(length(comps) + 1)) {
            if (i > length(comps)) break;
            if (length(comps[[i]]) == 0) next;  # e.g. empty initial component
            if (names(comps)[[i]] == cname
                & comps[[i]]$type == n$type
                & comps[[i]]$name == n$name) break;
        }
        comps[[i]] <- n
        names(comps)[[i]] <- cname
        return(comps)
    }

    # Insert / Update component in likelihood file
    likelihood <- gadget_dir_read(gd, 'likelihood')
    likelihood$components <- insert_component(
        as.list(likelihood$components),
        "component",
        lapply(obj, function (x) {
            # Don't let gadget_file's leak out into lists for export
            if ("gadget_file" %in% class(x)) x$filename else x
        }))
    gadget_dir_write(gd, likelihood)

    # Write out each file-based component
    for (x in obj) {
        if ("gadget_file" %in% class(x)) {
            gadget_dir_write(gd, x)
        }
    }
}

gadget_dir_read <- function(gd, file_name, missing_okay = TRUE) UseMethod("gadget_dir_read", gd)
gadget_dir_read.gadget_directory <- function(gd, file_name, missing_okay = TRUE) {
    path <- file.path(gd$dir, file_name)

    if (missing_okay && !file.exists(path)) {
        # Return empty file to read later
        gadget_file(file_name)
    } else {
        read.gadget_file(path)
    }
}
