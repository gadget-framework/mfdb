gadgetfile <- function (filename, properties = NULL, data = NULL) {
    structure(list(
        filename = filename,
        props = properties,
        data = data), class = "gadgetfile")
}

print.gadgetfile <- function (x, ...) {
    #TODO: Output commented version
    # properties are in key\tvalue form
    if (is.list(x$props)) {
        for (k in names(x$props)) {
            v = x$props[[k]]
            cat(paste0(k, "\t", v, "\n"))
        }
    }
}

as.character.gadgetfile <- function (object) {
    capture.output(print.gadgetfile(object))
}

write.gadgetfile <- function (dir) {
}
