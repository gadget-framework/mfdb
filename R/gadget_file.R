gadget_file <- function (filename, properties = NULL, data = NULL) {
    structure(list(
        filename = filename,
        props = properties,
        data = data), class = "gadget_file")
}

# Print character representation of the gadget file to stdout
print.gadget_file <- function (x, ...) {
    #TODO: Output commented version

    # properties are in key\tvalue form
    if (is.list(x$props)) {
        for (k in names(x$props)) {
            cat(paste0(k, "\t", paste(x$props[[k]], collapse = "\t"), "\n"))
        }
    }

    if (!is.null(x$data)) {
        cat("; ")
        write.table(x$data,
                file = "",
                quote = FALSE,
                sep = "\t",
                col.names = TRUE,
                row.names = FALSE,
                fileEncoding = "utf-8")
    }
}

# Return a character representation of the gadget file
as.character.gadget_file <- function (x, ...) {
    capture.output(print.gadget_file(x))
}

# Load gadget file into memory
read.gadget_file <- function(file = "") {
    stop("Not implemented")
}
