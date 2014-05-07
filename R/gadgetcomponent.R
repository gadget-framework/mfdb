gadgetcomponent <- function (type, ...) {
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

print.gadgetcomponent <- function(x, ...) {
    #TODO: Output commented version
    cat("[component]\n")
    cat(paste0("type\t", componenttype(x), "\n"))
    for (k in names(x)) {
        v = x[[k]]
        if ("gadgetfile" %in% class(v)) {
            # Don't paste the gadget object in, leave the filename
            v = v[['filename']]
        }
        cat(paste0(k, "\t", v, "\n"))
    }
}

as.character.gadgetcomponent <- function(object, ...) {
    capture.output(print.gadgetcomponent(object))
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
        datafile = gadgetfile(
            filename = paste0(name, '.penaltyfile'),
            data = data),
        weight = weight), class = c("gadgetpenaltycomponent", "gadgetcomponent"))
}
componenttype.gadgetpenaltycomponent <- function (x) { "penalty" }

### Understocking component

gadgetunderstockingcomponent <- function (weight = 0, name = "understocking") {
    structure(list(
        name = name,
        weight = weight), class = c("gadgetunderstockingcomponent", "gadgetcomponent"))
}
componenttype.gadgetunderstockingcomponent <- function (x) { "understocking" }

### Catch statistics component
#   GAVL: age vs length
#   GAVW: age vs weight

# function:		"lengthgivensd", etc. Gadget function.
gadgetcatchstatisticscomponent <- function (weight = 0,
        name = "catchstatistics",
        function = 'lengthgivensd',
        meanlength = NULL) {
    # Break out aggregates
    meanlength <- 
    structure(list(
        name = name,
        weight = weight,
        datafile = 'TODO:',
        "function" = 'lengthgivensd',
        areaaggfile = NULL,
        ageaggfile = NULL,
        fleetnames = if (is_survey) surveynames[[0]] else fleetnames[[0]],
        stocknames = stocknames), class = c("gadgetcatchstatisticscomponent", "gadgetcomponent"))
}
componenttype.gadgetcatchstatisticscomponent <- function (x) { "catchstatistics" }
