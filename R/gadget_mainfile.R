# For each option, make sure values contained are in main file
gadget_mainfile_update <- function (gd,
        timefile = NULL,
        areafile = NULL,
        printfiles = NULL,
        stockfiles = NULL,
        tagfiles = NULL,
        otherfoodfiles = NULL,
        fleetfiles = NULL,
        likelihoodfiles = NULL) {
    made_change <- FALSE
    swap <- function (old_val, repl_val, single = FALSE) {
        # NULL means leave alone
        if (is.null(repl_val)) return(old_val);

        # Merge vectors
        new_val <- if (single) repl_val else unique(c(old_val, repl_val))
        if (!identical(all.equal(old_val, new_val), TRUE)) {
            made_change <<- TRUE
        }
        return(new_val);
    }

    # Read file, create basic outline if doesn't exist
    mfile <- gadget_dir_read(gd, gd$mainfile, missing_okay = TRUE)
    if (length(mfile$components) == 0) mfile$components <- list(
        list(timefile = "", areafile = "", printfiles = structure(list(), comment = "Required comment")),
        stock = list(),
        tagging = list(),
        otherfood = list(),
        fleet = list(),
        likelihood = list())

    # Do simple swaps first
    mfile$components[[1]]$timefile <- swap(mfile$components[[1]]$timefile, timefile, single = TRUE)
    mfile$components[[1]]$areafile <- swap(mfile$components[[1]]$areafile, areafile, single = TRUE)

    # Printfiles is mandatory, but can specify empty by adding a comment
    mfile$components[[1]]$printfiles <- swap(mfile$components[[1]]$printfiles, printfiles)

    # Rest are in their own component
    mfile$components$stock$stockfiles <- swap(mfile$components$stock$stockfiles, stockfiles)
    mfile$components$tagging$tagfiles <- swap(mfile$components$tagging$tagfiles, tagfiles)
    mfile$components$otherfood$otherfoodfiles <- swap(mfile$components$otherfood$otherfoodfiles, otherfoodfiles)
    mfile$components$fleet$fleetfiles <- swap(mfile$components$fleet$fleetfiles, fleetfiles)
    mfile$components$likelihood$likelihoodfiles <- swap(mfile$components$likelihood$likelihoodfiles, likelihoodfiles)

    # Write file back out again
    if (made_change) gadget_dir_write(gd, mfile)
}
