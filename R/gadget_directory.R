# Initalise a gadget output directory, including creating it if necessary
gadget_directory <- function (dir, mainfile = 'main') {
    if(!utils::file_test("-d", dir)) tryCatch(dir.create(dir), warning = function(w) {
        stop(paste("Could not create output directory:", conditionMessage(w)))
    })
    if(!utils::file_test("-d", dir)) stop(paste("Could not create output directory", dir))
    structure(list(
        mainfile = mainfile,
        dir = dir), class = c("gadget_directory"))
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

# Generic to write gadget objects out
gadget_dir_write <- function(gd, obj) UseMethod("gadget_dir_write", obj)
