gadget_likelihood_component <- function (type, weight = 0, name = type, likelihoodfile = 'likelihood', ...) {
    obj <- structure(c(
        list(name = name, weight = weight, type = type),
        switch(type,
            penalty = gadget_penalty_component(name, ...),
            understocking = gadget_understocking_component(name, ...),
            catchdistribution = gadget_catchdistribution_component(name, ...),
            catchstatistics = gadget_catchstatistics_component(name, ...),
            stockdistribution = gadget_stockdistribution_component(name, ...),
            surveyindicies = gadget_surveyindicies_component(name, ...),
            surveydistribution = gadget_surveydistribution_component(name, ...),
            stomachcontent = gadget_stomachcontent_component(name, ...),
            recaptures = gadget_recaptures_component(name, ...),
            recstatistics = gadget_recstatistics_component(name, ...),
            migrationpenalty = gadget_migrationpenalty_component(name, ...),
            catchinkilos = gadget_catchinkilos_component(name, ...),
            stop(paste("Unknown likelihood component", type)))
    ), likelihoodfile = likelihoodfile, class = c(
        paste0("gadget_", type, "_component"),
        "gadget_likelihood_component"))
}

fname <- function (dir, ...) {
    file.path(dir, paste0(c(...), collapse = ""))
}

gadget_dir_write.gadget_likelihood_component <- function(gd, obj) {
    # Either replace component with matching name, or add to end
    gadget_likelihoodfile_update <- function(gd, fname, obj) {
        likelihood <- gadget_dir_read(gd, fname)

        n <- lapply(obj, function (x) {
            # Don't let gadget_file's leak out into lists for export
            if ("gadget_file" %in% class(x)) x$filename else x
        })

        # Find component with matching name and type
        for (i in 1:(length(likelihood$components) + 1)) {
            if (i > length(likelihood$components)) break;
            if (length(likelihood$components[[i]]) == 0) next;  # e.g. empty initial component
            if (names(likelihood$components)[[i]] == "component"
                & likelihood$components[[i]]$type == n$type
                & likelihood$components[[i]]$name == n$name) break;
        }
        likelihood$components[[i]] <- n
        names(likelihood$components)[[i]] <- "component"
        if (is.null(attr(likelihood$components[[i]], "preamble"))) {
            attr(likelihood$components[[i]], "preamble") <- ""
        }

        gadget_dir_write(gd, likelihood)
    }

    # Update mainfile and likelihood file
    likelihoodfile <- if (is.null(attr(obj, 'likelihoodfile'))) 'likelihood' else attr(obj, 'likelihoodfile')
    gadget_mainfile_update(gd, likelihoodfiles = likelihoodfile)
    gadget_likelihoodfile_update(gd, likelihoodfile, obj)

    # Write out each file-based component
    for (x in obj) {
        if ("gadget_file" %in% class(x)) {
            gadget_dir_write(gd, x)
        }
    }
}

### Internal constructors for each component type

gadget_penalty_component <- function (name, data = NULL) {
    list(datafile = gadget_file(
        fname('Data', name, '.penaltyfile'),
        data = if (length(data) > 0) data else data.frame(
            switch = c("default"),
            power = c(2),
            stringsAsFactors = FALSE)))
}

gadget_understocking_component <- function (name) {
    list()
}

gadget_catchstatistics_component <- function (
        name,
        data_function = NULL,
        data = NULL, area = NULL, age = NULL,
        fleetnames = c(), stocknames = c()) {

    prefix <- paste0('catchstatistics.', name, '.')

    if (is.null(data)) {
        stop("No data provided")
    }

    # Work out data_function based how data was generated
    if (!is.null(data_function)) {
        # It's already set, so nothing to do
    } else if (is.null(attr(data, "generator"))) {
        stop("Cannot work out the required function, and data_function not provided")
    } else if (attr(data, "generator") == "mfdb_sample_meanlength_stddev") {
        data_function <- 'lengthgivenstddev'
    } else if (attr(data, "generator") == "mfdb_sample_meanlength") {
        data_function <- 'lengthnostddev'
    } else if (attr(data, "generator") == "mfdb_sample_meanweight_stddev") {
        data_function <- 'weightgivenstddev'
    } else if (attr(data, "generator") == "mfdb_sample_meanweight") {
        data_function <- 'weightnostddev'
    } else {
        stop(paste("Unknown generator function", attr(data, "generator")))
    }

    # Make sure we have the columns we need
    compare_cols(names(data), list(
        lengthcalcstddev = c("year", "step", "area", "age", "number", "mean"),
        lengthgivenstddev = c("year", "step", "area", "age", "number", "mean", "stddev"),
        weightgivenstddev = c("year", "step", "area", "age", "number", "mean", "stddev"),
        weightnostddev = c("year", "step", "area", "age", "number", "mean"),
        lengthnostddev = c("year", "step", "area", "age", "number", "mean"),
        null = NULL)[[data_function]])

    list(
        datafile = gadget_file(fname('Data', prefix, data_function), data=data),
        "function" = data_function,
        areaaggfile = agg_file('area', prefix, if(is.null(area)) attr(data, "area") else area),
        ageaggfile  = agg_file('age', prefix, if(is.null(age)) attr(data, "age") else age),
        fleetnames = fleetnames,
        stocknames = stocknames)
}

gadget_catchdistribution_component <- function (
        name,
        data_function = 'sumofsquares',
        data_function_params = list(),
        aggregationlevel = FALSE,
        overconsumption = FALSE,
        epsilon = 10,
        data = NULL, area = NULL, age = NULL, length = NULL,
        fleetnames = c(), stocknames = c()) {

    prefix <- paste0('catchdistribution.', name, '.')

    # Make sure we have the columns we need
    compare_cols(names(data), c("year", "step", "area", "age", "length", "number"))

    c(
        list(
            datafile = gadget_file(fname('Data', prefix, data_function), data=data),
            "function" = data_function
        ),
        data_function_params,
        list(
            aggregationlevel = if (aggregationlevel) 1 else 0,
            overconsumption = if (overconsumption) 1 else 0,
            epsilon = epsilon,
            areaaggfile = agg_file('area', prefix, if(is.null(area)) attr(data, "area") else area),
            ageaggfile  = agg_file('age', prefix, if(is.null(age)) attr(data, "age") else age),
            lenaggfile  = agg_file('len', prefix, if(is.null(length)) attr(data, "length") else length),
            fleetnames = fleetnames,
            stocknames = stocknames))
}

gadget_stockdistribution_component <- function (
        name,
        data_function = 'sumofsquares',
        overconsumption = FALSE,
        epsilon = 10,
        data = NULL, area = NULL, age = NULL, length = NULL,
        fleetnames = c(), stocknames = c()) {
    prefix <- paste0('stockdistribution.', name, '.')

    # Make sure we have the columns we need
    compare_cols(names(data), c("year", "step", "area", NA, "length", "number"))

    # For stock distribution, anything in column 4 should be called stock
    if (length(names(data)) > 4) {
        names(data)[4] <- 'stock'
    }

    list(
        datafile = gadget_file(fname('Data', prefix, data_function), data=data),
        "function" = data_function,
        overconsumption = if (overconsumption) 1 else 0,
        epsilon = epsilon,
        areaaggfile = agg_file('area', prefix, if(is.null(area)) attr(data, "area") else area),
        ageaggfile  = agg_file('age', prefix, if(is.null(age)) attr(data, "age") else age),
        lenaggfile  = agg_file('len', prefix, if(is.null(length)) attr(data, "length") else length),
        fleetnames = fleetnames,
        stocknames = stocknames)
}

# http://www.hafro.is/gadget/userguide/userguide.html#x1-1090008.6
gadget_surveyindicies_component <- function (
        name,
        sitype = 'lengths',
        biomass = 0,
        data = NULL,
        area = NULL,
        ...) {
    prefix <- paste0('surveyindicies.', name, '.')

    if (!('fittype' %in% names(c(...)))) {
        stop("fittype missing. It is a required parameter")
    }

    if (sitype == 'lengths') {
        compare_cols(names(data), c("year", "step", "area", "length", "number"))
        length <- c(...)['length']
        si_cols <- list(
            lenaggfile  = agg_file('len', prefix, if(is.null(length)) attr(data, "length") else length))

    } else if (sitype == 'ages') {
        compare_cols(names(data), c("year", "step", "area", "age", "number"))
        age <- c(...)['age']
        si_cols <- list(
            ageaggfile  = agg_file('age', prefix, if(is.null(age)) attr(data, "age") else age))

    } else if (sitype == 'fleets') {
        compare_cols(names(data), c("year", "step", "area", "length", "number"))
        length <- c(...)['length']
        si_cols <- list(
            lenaggfile  = agg_file('len', prefix, if(is.null(length)) attr(data, "length") else length),
            fleetnames = c(...)['fleetnames'])

    } else if (sitype == 'acoustic') {
        compare_cols(names(data), c("year", "step", "area", "survey", "acoustic"))
        si_cols <- list(surveynames = c(...)['surveynames'])

    } else if (sitype == 'effort') {
        compare_cols(names(data), c("year", "step", "area", "fleet", "effort"))
        si_cols <- list(fleetnames = c(...)['fleetnames'])

    } else {
        stop("Unknown sitype", sitype)
    }

    # Mix in other default columns
    return(c(
        list(
            datafile = gadget_file(fname('Data', prefix, sitype), data=data),
            sitype = sitype,
            biomass = biomass,
            areaaggfile = agg_file('area', prefix, if(is.null(area)) attr(data, "area") else area)),
        si_cols,
        list(stocknames = c(...)['stocknames']),
        na.omit(c(...)[c('fittype', 'slope', 'intercept')]),
        NULL))
}

gadget_surveydistribution_component <- function (
        name,
        data = NULL) {
    stop("Not implemented")
}

gadget_stomachcontent_component <- function (
        name,
        data = NULL) {
    stop("Not implemented")
}

gadget_recaptures_component <- function (
        name,
        data = NULL) {
    stop("Not implemented")
}

gadget_recstatistics_component <- function (
        name,
        data = NULL) {
    stop("Not implemented")
}

# http://www.hafro.is/gadget/userguide/userguide.html#x1-1380008.11
gadget_migrationpenalty_component <- function (
        name,
        stockname = c(),
        powercoeffs = c()) {

    list(
        stockname = stockname,
        powercoeffs = powercoeffs)
}

gadget_catchinkilos_component <- function (
        name,
        data = NULL) {
    stop("Not implemented")
}

agg_file <- function (type, prefix, data) {
    if (is.null(data)) {
        # Data isn't aggregated, so leave a placeholder for now
        data <- list(all = 'X')
    } else if (class(data) == 'integer' || class(data) == 'character') {
        # Convert 1:5 to a list of 1 = 1, 2 = 2, ...
        data <- structure(
            lapply(data, function (x) x),
            names = data)
    }

    if (type == 'area') {
        # Areas should just be a => 1, b => 2, ...
        comp <- structure(
            as.list(seq_len(length(data))),
            names = names(data))
    } else if (type == 'len') {
        # Lengths should be min/max
        comp <- lapply(as.list(data), function (x) c(min(x), max(x)))
    } else {
        # Convert to list
        comp <- as.list(data)
    }

    return(gadget_file(
        fname('Aggfiles', prefix, type, '.agg'),
        components=list(comp)))
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
