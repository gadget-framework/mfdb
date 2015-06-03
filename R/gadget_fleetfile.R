# Create/update a fleet component with name
gadget_fleet_component <- function (type,
        name = type,
        livesonareas = unique(data$area),
        multiplicative = 1,
        suitability = NULL,
        fleetfile = 'fleet',
        data = stop("data not provided"),
        ...) {
    type <- tolower(type)

    if (type == 'effortfleet') {
        # Catchability is list of stocks
        extras <- c(
            list(catchability = NULL),
            list(...)[['catchability']])
    } else if (type == 'quotafleet') {
        extras <- list(...)[c(
            'quotafunction',
            'biomasslevel',
            'quotalevel',
            NULL)]
    } else {
        extras <- list()
    }

    # No aggfile, so make a map and use this internally
    area_map <- structure(
        seq_len(length(attr(data, 'area'))),
        names = names(attr(data, 'area')))

    if (ncol(data) == 5) {
        # Fleetfile is provided. Ideally we'd remove this
        compare_cols(names(data), c(
            "year",
            "step",
            "area",
            NA,  # fleet name
            NA,  # amount for total/number fleet, else scaling factor
            NULL))

        data$area <- area_map[data$area]
    } else {
        compare_cols(names(data), c(
            "year",
            "step",
            "area",
            NA,  # amount for total/number fleet, else scaling factor
            NULL))

        # Map areas to integers, add in constant fleetname column
        data <- cbind(
            data[c(1,2)],
            data.frame(
                area = area_map[data$area],
                fleetname = c(name)),
            data[c(4)])
    }

    amountfile <- gadget_file(
        file.path('Data', paste(fleetfile, name, 'data', sep = ".")),
        data = data)

    structure(c(
        structure(list(name), names = c(type)),
        list(
            livesonareas = area_map[livesonareas],
            multiplicative = multiplicative,
            suitability = suitability),
        extras,
        list(amount = amountfile),
        NULL), fleetfile = fleetfile, class = c(
            paste0("gadget_", type, "_component"),
            "gadget_fleet_component",
            NULL))
}

gadget_dir_write.gadget_fleet_component <- function(gd, obj) {
    fname <- file.path('Modelfiles', paste(attr(obj, 'fleetfile'), 'fleet', sep = "."))

    # Update mainfile with pointer to fleetfile
    gadget_mainfile_update(gd, fleetfiles = fname)

    # Update component in fleetfile
    if (is.null(attr(obj, "preamble"))) {
        attr(obj, "preamble") <- ""
    }
    fleetfile <- gadget_dir_read(gd, fname)
    fleetfile <- component_replace(fleetfile, obj, function(comp) {
        if (length(comp) == 0) "" else comp[[1]]
    }, component_name = "fleetcomponent")
    gadget_dir_write(gd, fleetfile)
}
