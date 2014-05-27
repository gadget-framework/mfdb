gadget_likelihood_component <- function (type, ...) {
    if (type == 'penalty') {
        return(gadgetpenaltycomponent(...))
    }
    if (type == 'understocking') {
        return(gadgetunderstockingcomponent(...))
    }
    if (type == 'catchstatistics') {
        return(gadgetcatchstatisticscomponent(...))
    }
    stop(paste("Unknown likelihood component", type))
}

print.gadget_likelihood_component <- function(x, ...) {
    #TODO: Output commented version
    cat(";\n;\n[component]\n")
    cat(paste0("type\t", componenttype(x), "\n"))
    for (k in names(x)) {
        v = x[[k]]
        if ("gadget_file" %in% class(v)) {
            # Don't paste the gadget object in, leave the filename
            v = v[['filename']]
        }
        cat(paste0(k, "\t", paste(v, collapse = "\t"), "\n"))
    }
}

as.character.gadget_likelihood_component <- function(object, ...) {
    capture.output(print.gadget_likelihood_component(object))
}

componenttype <- function(x) UseMethod("componenttype")

### Penalty / bounds component

gadgetpenaltycomponent <- function (weight = 0, name = "bounds", data = NULL) {
    if (!length(data)) {
        data = data.frame(
            switch = c("default"),
            power = c(2),
            stringsAsFactors = FALSE)
    }
    structure(list(
        name = name,
        datafile = gadget_file(
            filename = paste0(name, '.penaltyfile'),
            data = data),
        weight = weight), class = c("gadgetpenaltycomponent", "gadget_likelihood_component"))
}
componenttype.gadgetpenaltycomponent <- function (x) { "penalty" }

### Understocking component

gadgetunderstockingcomponent <- function (weight = 0, name = "understocking") {
    structure(list(
        name = name,
        weight = weight), class = c("gadgetunderstockingcomponent", "gadget_likelihood_component"))
}
componenttype.gadgetunderstockingcomponent <- function (x) { "understocking" }

### Catch statistics component
#   GAVL: age vs length
#   GAVW: age vs weight

# function:		"lengthgivensd", etc. Gadget function.
gadgetcatchstatisticscomponent <- function (weight = 0,
        name = "catchstatistics",
        data_function = NULL,
        data = NULL, area = NULL, age = NULL,
        fleetnames = c(), stocknames = c()) {

    prefix <- paste0('catchstatistics.', name, '.')

    # Work out data_function based how data was generated
    if (!is.null(data_function)) {
        # It's already set, so nothing to do
    } else if (attr(data, "generator") == "mfdb_meanlength_stddev") {
        data_function <- 'lengthgivenstddev'
    } else if (attr(data, "generator") == "mfdb_meanlength"){
        data_function <- 'lengthnostddev'
    } else if (attr(data, "generator") == "mfdb_meanweight_stddev") {
        data_function <- 'weightgivenstddev'
    } else if (attr(data, "generator") == "mfdb_meanweight"){
        data_function <- 'weightnostddev'
    } else {
        stop("Cannot work out the required function, and data_function not provided")
    }

    structure(list(
        name = name,
        weight = weight,
        datafile = gadget_file(paste0(prefix, data_function), data=data),
        "function" = data_function,
        areaaggfile = gadget_file(paste0(prefix, 'area.agg'), properties=area),
        ageaggfile = gadget_file(paste0(prefix, 'age.agg'), properties=age),
        fleetnames = fleetnames,
        stocknames = stocknames), class = c("gadgetcatchstatisticscomponent", "gadget_likelihood_component"))
}
componenttype.gadgetcatchstatisticscomponent <- function (x) { "catchstatistics" }
