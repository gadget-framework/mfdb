# Initalise a gadget output directory, including creating it if necessary
gadget_directory <- function (dir) {
    # TODO: Create directory if it doesn't exist
    # TODO: Option to empty directory first?
    structure(list(
        dir = dir), class = c("gadget_directory"))
}

# Create a mainfile to go in the gadget directory
gadget_mainfile <- function (gd) {
    # TODO:
}

# Create the main areafile
gadget_areafile <- function (gd, sizes = NULL, temperatures = NULL) {
    # TODO:
}

# Add a likelihood component to the likelihood file, and write data files the
# component requires
gadget_likelihood_component <- function (gd, type, ...) {
    # TODO:
}
