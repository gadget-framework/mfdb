gadget_likelihood_component <- function (type, weight = 0, name = type, likelihoodfile = 'likelihood', ...) {
    # Formulate arguments to hand down to next function
    args <- list(name = name, ...)
    if(!is.null(args$data) && !is.data.frame(args$data)) {
        if (length(args$data) == 1 && is.data.frame(args$data[[1]])) {
            # List-wrapped data.frame from mfdb_*, be nice and unwrap it.
            args$data <- args$data[[1]]
        } else {
            stop("data supplied for ", name, " is a ", class(args$data), ", not a data.frame.")
        }
    }

    # Call appropriate function
    if(gsub("[a-z]", "", type) != "") stop("Malformed component type ", type)
    obj <- do.call(paste0(c("gadget", type, "component"), collapse = "_"), args)

    # Wrap up with common bits of class
    obj <- structure(c(
        list(name = name, weight = weight, type = type),
        obj
    ), likelihoodfile = likelihoodfile, class = c(
        paste0("gadget_", type, "_component"),
        "gadget_likelihood_component"))
}

gadget_dir_write.gadget_likelihood_component <- function(gd, obj) {
    fname <- if (is.null(attr(obj, 'likelihoodfile'))) 'likelihood' else attr(obj, 'likelihoodfile')

    # Update mainfile
    gadget_mainfile_update(gd, likelihoodfiles = fname)

    # Update component in likelihood file
    if (is.null(attr(obj, "preamble"))) {
        attr(obj, "preamble") <- ""
    }
    likelihoodfile <- gadget_dir_read(gd, fname)
    likelihoodfile <- component_replace(likelihoodfile, obj, function(comp) {
        if (length(comp) == 0) return("")
        return(paste(comp$type, comp$name, sep = ":", collapse = "."))
    })
    gadget_dir_write(gd, likelihoodfile)
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
        datafile = gadget_file(fname('Data', fname_prefix(sys.call(0), name), data_function), data=data),
        "function" = data_function,
        areaaggfile = agg_file('area', fname_prefix(sys.call(0), name), if(is.null(area)) attr(data, "area") else area),
        ageaggfile  = agg_file('age', fname_prefix(sys.call(0), name), if(is.null(age)) attr(data, "age") else age),
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

    # Make sure we have the columns we need
    compare_cols(names(data), c("year", "step", "area", "age", "length", "number"))

    c(
        list(
            datafile = gadget_file(fname('Data', fname_prefix(sys.call(0), name), data_function), data=data),
            "function" = data_function
        ),
        data_function_params,
        list(
            aggregationlevel = if (aggregationlevel) 1 else 0,
            overconsumption = if (overconsumption) 1 else 0,
            epsilon = epsilon,
            areaaggfile = agg_file('area', fname_prefix(sys.call(0), name), if(is.null(area)) attr(data, "area") else area),
            ageaggfile  = agg_file('age', fname_prefix(sys.call(0), name), if(is.null(age)) attr(data, "age") else age),
            lenaggfile  = agg_file('len', fname_prefix(sys.call(0), name), if(is.null(length)) attr(data, "length") else length),
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
    # Make sure we have the columns we need
    compare_cols(names(data), c("year", "step", "area", NA, "age", "length", "number"))

    # For stock distribution, anything in column 4 should be called stock
    if (length(names(data)) > 4) {
        names(data)[4] <- 'stock'
    }

    list(
        datafile = gadget_file(fname('Data', fname_prefix(sys.call(0), name), data_function), data=data),
        "function" = data_function,
        overconsumption = if (overconsumption) 1 else 0,
        epsilon = epsilon,
        areaaggfile = agg_file('area', fname_prefix(sys.call(0), name), if(is.null(area)) attr(data, "area") else area),
        ageaggfile  = agg_file('age', fname_prefix(sys.call(0), name), if(is.null(age)) attr(data, "age") else age),
        lenaggfile  = agg_file('len', fname_prefix(sys.call(0), name), if(is.null(length)) attr(data, "length") else length),
        fleetnames = fleetnames,
        stocknames = stocknames)
}

# http://www.hafro.is/gadget/userguide/userguide.html#x1-1090008.6
gadget_surveyindices_component <- function (
        name,
        sitype = 'lengths',
        biomass = 0,
        data = NULL,
        area = NULL,
        fittype = NULL,
        length = NULL,
        age = NULL,
        fleetnames = NULL,
        surveynames = NULL,
        stocknames = NULL,
        slope = NULL,
        intercept = NULL) {
    if (is.null(fittype)) {
        stop("fittype missing. It is a required parameter")
    }

    if (sitype == 'lengths') {
        compare_cols(names(data), c("year", "step", "area", "length", "number"))
        si_cols <- list(
            lenaggfile  = agg_file(
                'len',
                fname_prefix(sys.call(0), name),
                if(is.null(length)) attr(data, "length") else length))

    } else if (sitype == 'ages') {
        compare_cols(names(data), c("year", "step", "area", "age", "number"))
        si_cols <- list(
            ageaggfile  = agg_file(
                'age',
                fname_prefix(sys.call(0), name),
                if(is.null(age)) attr(data, "age") else age))

    } else if (sitype == 'fleets') {
        compare_cols(names(data), c("year", "step", "area", "length", "number"))
        if (is.null(fleetnames)) {
            stop("Expected vector of fleetnames for effort surveyindices")
        }
        si_cols <- list(
            lenaggfile  = agg_file(
                'len',
                fname_prefix(sys.call(0), name),
                if(is.null(length)) attr(data, "length") else length),
            fleetnames = fleetnames)

    } else if (sitype == 'acoustic') {
        compare_cols(names(data), c("year", "step", "area", NA, NA))
        if (is.null(surveynames)) {
            stop("Expected vector of surveynames for acoustic surveyindices")
        }
        si_cols <- list(surveynames = surveynames)

    } else if (sitype == 'effort') {
        compare_cols(names(data), c("year", "step", "area", NA, NA))
        if (is.null(fleetnames)) {
            stop("Expected vector of fleetnames for effort surveyindices")
        }
        si_cols <- list(fleetnames = fleetnames)

    } else {
        stop("Unknown sitype", sitype)
    }

    # Mix in other default columns
    return(c(
        list(
            datafile = gadget_file(fname('Data', fname_prefix(sys.call(0), name), sitype), data=data),
            sitype = sitype,
            biomass = biomass,
            areaaggfile = agg_file('area', fname_prefix(sys.call(0), name), if(is.null(area)) attr(data, "area") else area)),
        si_cols,
        if (is.null(stocknames)) c() else list(stocknames = stocknames),
        if (is.null(fittype)) c() else list(fittype = fittype),
        if (is.null(slope)) c() else list(slope = slope),
        if (is.null(intercept)) c() else list(intercept = intercept),
        NULL))
}

gadget_surveydistribution_component <- function (
        name,
        data = NULL,
        area = NULL,
        length = NULL,
        age = NULL,
        stocknames = c(),
        fittype = 'linearfit',
        slope = NULL,
        intercept = NULL,
        epsilon = 10,
        likelihoodtype = 'multinomial') {

    compare_cols(names(data), c("year", "step", "area", "age", "length", "number"))

    # Combine standard columns with fit type parameters
    return(c(
        list(
            datafile = gadget_file(fname('Data', fname_prefix(sys.call(0), name)), data=data),
            areaaggfile = agg_file('area', fname_prefix(sys.call(0), name), if(is.null(area)) attr(data, "area") else area),
            lenaggfile  = agg_file('len', fname_prefix(sys.call(0), name), if(is.null(length)) attr(data, "length") else length),
            ageaggfile  = agg_file('age', fname_prefix(sys.call(0), name), if(is.null(age)) attr(data, "age") else age),
            stocknames = stocknames,
            fittype = fittype,
            parameters = ""),
        if (is.null(slope)) c() else list(slope = slope),
        if (is.null(intercept)) c() else list(intercept = intercept),
        list(
            epsilon = epsilon,
            likelihoodtype = likelihoodtype),
        NULL))
}

# http://www.hafro.is/gadget/userguide/userguide.html#x1-1300008.8
gadget_stomachcontent_component <- function (
        name,
        data_function = 'scsimple',
        epsilon = 10,
        area = NULL,
        predator_length = NULL,
        prey_length = NULL,
        prey_labels = c(""),
        prey_digestion_coefficients = c(1,0,0),
        predator_names = c(),
        data = NULL) {
    # Make sure we have the columns we need
    compare_cols(names(data), c("year", "step", "area", "predator_length", NA, "ratio"))

    # Generate prey file
    if(is.null(prey_length)) prey_length <- attr(data, "prey_length")
    if(is.null(prey_length)) stop("Data should group by prey_length to build prey aggregation file")
    prey_minmax <- agg_prop(prey_length, "min/max")
    prey_components <- lapply(names(prey_length), function (name) {
        structure(
            list(
                name = NULL,
                lbls = (if (length(prey_labels) > 1) prey_labels[seq(2,length(prey_labels))]),
                lengths = prey_minmax[[name]],
                digestioncoefficients = prey_digestion_coefficients),
            names = c(name, prey_labels[[1]], 'lengths', 'digestioncoefficients'),
            preamble = "")
    })

    list(
        "function" = data_function,
        datafile = gadget_file(fname('Data', fname_prefix(sys.call(0), name), data_function), data=data),
        epsilon = epsilon,
        areaaggfile = agg_file('area', fname_prefix(sys.call(0), name), if(is.null(area)) attr(data, "area") else area),
        predatornames = predator_names,
        predatorlengths = NULL,
        lenaggfile  = agg_file('len', fname_prefix(sys.call(0), name), if(is.null(predator_length)) attr(data, "predator_length") else predator_length),
        preyaggfile = gadget_file(fname('Aggfiles', fname_prefix(sys.call(0), name), 'prey.agg'),components=prey_components))
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
        data = NULL,
        data_function = 'sumofsquares',
        epsilon = 10,
        area = NULL,
        fleetnames = c(), stocknames = c()) {
    # Make sure we have the columns we need
    compare_cols(names(data), c("year", "step", "area", NA, "total_weight"))

    # If aggregated yearly, then switch to aggregationlevel 1 and drop step column
    if (isTRUE(identical(attr(data, 'step'), mfdb_timestep_yearly))) {
        aggregationlevel <- 1
        data <- data[,names(data) != 'step', drop = FALSE]
    } else {
        aggregationlevel <- 0
    }

    list(
        datafile = gadget_file(fname('Data', fname_prefix(sys.call(0), name), data_function), data=data),
        "function" = data_function,
        aggregationlevel = aggregationlevel,
        epsilon = epsilon,
        areaaggfile = agg_file('area', fname_prefix(sys.call(0), name), if(is.null(area)) attr(data, "area") else area),
        fleetnames = fleetnames,
        stocknames = stocknames)
}

# Transform agg summary by either applying func_name, or fishing out pre-baked values
agg_prop <- function (data, func_name) {
    get_prop <- function (x, func_name) {
        if (func_name == "diff") {
            return(diff(get_prop(x, "min/max")))
        }
        if (func_name == "min/max") {
            return(c(get_prop(x, "min"), get_prop(x, "max")))
        }
        if (!is.null(attr(x, func_name))) {
            return(attr(x, func_name))
        }
        # No shortcut attribute, eval x properly
        do.call(func_name, list(eval(x)))
    }

    lapply(as.list(data), function (d) get_prop(d, func_name))
}

agg_file <- function (type, prefix, data) {
    if (type == 'area') {
        # Areas should just be a => 1, b => 2, ...
        comp <- structure(
            as.list(seq_len(length(data))),
            names = names(data))
    } else if (type == 'len') {
        # Lengths should be min/max
        comp <- agg_prop(data, "min/max")
    } else {
        # Convert to list
        comp <- agg_prop(data, "c")
    }

    return(gadget_file(
        fname('Aggfiles', prefix, type, '.agg'),
        components=list(comp)))
}

# Prefix for filenames based on callee and likelihood name
fname_prefix <- function (fn, name) {
    paste0(
        gsub(".*gadget_([^_]+)_component.*", "\\1", fn)[[1]],
        '.',
        name,
        '.')
}
