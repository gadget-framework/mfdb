gadget_areafile <- function (size, temperature) {
    structure(list(
        size = size,
        temperature = temperature
        ), class = "gadget_areafile")
}

gadget_dir_write.gadget_areafile <- function(gd, obj) {
    # Write out areafile
    gadget_dir_write(gd, gadget_file("area", components = list(list(
        areas = obj$size$area,
        size = obj$size$size,
        temperature = ""  # NB: Hack in a line with just "temperature"
    )), data = obj$temperature))

    # Add to mainfile
    gadget_mainfile_update(gd, areafile = "area")
}
