# Initalise a gadget output directory, including creating it if necessary
gadget_directory <- function (dir) {
    # TODO: Create directory if it doesn't exist
    # TODO: Option to empty directory first?
    structure(list(
        dir = dir), class = c("gadget_directory"))
}

# Create a mainfile to go in the gadget directory
gadget_mainfile <- function (gd) {
    # TODO: These shouldn't be here, they're specialisms of gadget_file
}

# Create the main areafile
gadget_areafile <- function (gd, sizes = NULL, temperatures = NULL) {
    # TODO: These shouldn't be here, they're specialisms of gadget_file
}

gadget_dir_write <- function(gd, obj) UseMethod("gadget_dir_write", obj)
gadget_dir_write.gadget_file <- function(gd, obj) {
    fh = file(file.path(gd$dir, obj$filename), "w")
    tryCatch(
        capture.output(print(obj), file = fh),
        finally = close(fh))
}

gadget_dir_write.gadget_likelihood_component <- function(gd, obj) {
    #TODO: Should parse the file, and add it in
    # Append the component to the likelihood file
    fname <- file.path(gd$dir, "likelihood")
    fh = file(fname, if (file.exists(fname)) "a" else "w")
    tryCatch(
        capture.output(print(obj), file = fh),
        finally = close(fh))

    # Write out each file-based component
    for (x in obj) {
        if ("gadget_file" %in% class(x)) {
            gadget_dir_write(gd, x)
        }
    }
}

gadget_dir_read <- function(gd, file_name, missing_okay = TRUE) {
    path <- file.path(gd$dir, file_name)

    if (missing_okay && !file.exists(path)) {
        # Return empty file to read later
        gadget_file(file_name)
    } else {
        read.gadget_file(path)
    }
}
