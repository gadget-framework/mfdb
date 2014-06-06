gadget_likelihood_component <- function (type, ...) {
    switch(type,
        penalty = gadget_penalty_component(...),
        understocking = gadget_understocking_component(...),
        catchstatistics = gadget_catchstatistics_component(...),
        catchdistribution = gadget_catchdistribution_component(...),
        stockdistribution = gadget_stockdistribution_component(...),
        stop(paste("Unknown likelihood component", type)))
}

print.gadget_likelihood_component <- function(x, ...) {
    #TODO: Output commented version
    cat(";\n;\n[component]\n")
    for (k in names(x)) {
        v = x[[k]]
        if ("gadget_file" %in% class(v)) {
            # Don't paste the gadget object in, leave the filename
            v = v[['filename']]
        }
        cat(paste0(k, "\t", paste(v, collapse = "\t"), "\n"))
    }
}

as.character.gadget_likelihood_component <- function(x, ...) {
    capture.output(print.gadget_likelihood_component(x))
}

### Internal constructors for each component type

gadget_penalty_component <- function (weight = 0, name = "penalty", data = NULL) {
    if (!length(data)) {
        data = data.frame(
            switch = c("default"),
            power = c(2),
            stringsAsFactors = FALSE)
    }
    structure(list(
        name = name,
        weight = weight,
        type = "penalty",
        datafile = gadget_file(
            filename = paste0(name, '.penaltyfile'),
            data = data)), class = c("gadget_penalty_component", "gadget_likelihood_component"))
}

gadget_understocking_component <- function (weight = 0, name = "understocking") {
    structure(list(
        name = name,
        weight = weight,
        type = "understocking"), class = c("gadget_understocking_component", "gadget_likelihood_component"))
}

gadget_catchstatistics_component <- function (weight = 0,
        name = "catchstatistics",
        data_function = NULL,
        data = NULL, areas = NULL, ages = NULL,
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
    } else if (attr(data, "generator") == "mfdb_meanlength_stddev") {
        data_function <- 'lengthgivenstddev'
    } else if (attr(data, "generator") == "mfdb_meanlength") {
        data_function <- 'lengthnostddev'
    } else if (attr(data, "generator") == "mfdb_meanweight_stddev") {
        data_function <- 'weightgivenstddev'
    } else if (attr(data, "generator") == "mfdb_meanweight") {
        data_function <- 'weightnostddev'
    } else {
        stop(paste("Unknown generator function", attr(data, "generator")))
    }

    structure(list(
        name = name,
        weight = weight,
        type = "catchstatistics",
        datafile = gadget_file(paste0(prefix, data_function), data=data),
        "function" = data_function,
        areaaggfile = gadget_file(paste0(prefix, 'area.agg'), properties=if(is.null(areas))   attr(data, "areas") else areas),
        ageaggfile  = gadget_file(paste0(prefix, 'age.agg'),  properties=if(is.null(ages))    attr(data, "ages") else ages),
        fleetnames = fleetnames,
        stocknames = stocknames), class = c("gadget_catchstatistics_component", "gadget_likelihood_component"))
}

gadget_catchdistribution_component <- function (weight = 0,
        name = "catchdistribution",
        data_function = 'sumofsquares',
        data_function_params = list(),
        aggregationlevel = FALSE,
        overconsumption = FALSE,
        epsilon = 10,
        data = NULL, areas = NULL, ages = NULL, lengths = NULL,
        fleetnames = c(), stocknames = c()) {

    prefix <- paste0('catchdistribution.', name, '.')

    structure(c(list(
        name = name,
        weight = weight,
        type = "catchdistribution",
        datafile = gadget_file(paste0(prefix, data_function), data=data),
        "function" = data_function),
        data_function_params, list(
        aggregationlevel = if (aggregationlevel) 1 else 0,
        overconsumption = if (overconsumption) 1 else 0,
        epsilon = epsilon,
        areaaggfile = gadget_file(paste0(prefix, 'area.agg'), properties=if(is.null(areas))   attr(data, "areas") else areas),
        ageaggfile  = gadget_file(paste0(prefix, 'age.agg'),  properties=if(is.null(ages))    attr(data, "ages") else ages),
        lenaggfile  = gadget_file(paste0(prefix, 'len.agg'),  properties=if(is.null(lengths)) attr(data, "lengths") else lengths),
        fleetnames = fleetnames,
        stocknames = stocknames)), class = c("gadget_catchdistribution_component", "gadget_likelihood_component"))
}

gadget_stockdistribution_component <- function (weight = 0,
        name = "stockdistribution",
        data_function = 'sumofsquares',
        overconsumption = FALSE,
        epsilon = 10,
        data = NULL, areas = NULL, ages = NULL, lengths = NULL,
        fleetnames = c(), stocknames = c()) {
    prefix <- paste0('stockdistribution.', name, '.')

    structure(c(list(
        name = name,
        weight = weight,
        type = "stockdistribution",
        datafile = gadget_file(paste0(prefix, data_function), data=data),
        "function" = data_function,
        overconsumption = if (overconsumption) 1 else 0,
        epsilon = epsilon,
        areaaggfile = gadget_file(paste0(prefix, 'area.agg'), properties=if(is.null(areas))   attr(data, "areas") else areas),
        ageaggfile  = gadget_file(paste0(prefix, 'age.agg'),  properties=if(is.null(ages))    attr(data, "ages") else ages),
        lenaggfile  = gadget_file(paste0(prefix, 'len.agg'),  properties=if(is.null(lengths)) attr(data, "lengths") else lengths),
        fleetnames = fleetnames,
        stocknames = stocknames)), class = c("gadget_stockdistribution_component", "gadget_likelihood_component"))
}
