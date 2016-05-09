# Fetch the agg_summary off a data.frame
fetch_agg_summary <- function (data, name) {
   out <- attr(data, name)
   if (is.null(out)) stop("Can't find ", data, " grouping data for catch_data")
   return(out)
}

# stanza_group.csv: functional group definition.
ewe_tbl_stanza_group <- function (stanza_structure) {
    # TODO: Summary of the first, top-level group
    data.frame(
        StGroupNum = 1:length(stanza_structure),
        Stanza_Group = names(stanza_structure),
        nstanzas = vapply(stanza_structure, length, 0),  # Number of stanzas in group
        VBGF_Ksp = NA, #  The K in the von Bartalanffy
        VBGF_d = 0.66667,  # Constant.
        Wmat = NA,  # Weight at maturity
        RecPower = 1,  # Constant.
        stringsAsFactors = FALSE)
}

# stanzas.csv: 
ewe_tbl_stanzas <- function(stanza_structure, all_stanzas, catch_data) {
    # Get the min and max values for all age groups
    age_groups <- fetch_agg_summary(catch_data, 'age')
    stanzas_min <- unlist(mfdb:::agg_prop(age_groups, "min"))
    stanzas_max <- unlist(mfdb:::agg_prop(age_groups, "max"))

    data.frame(
        StGroupNum = unlist(lapply(seq_len(length(stanza_structure)), function(i) rep(i, times = length(stanza_structure[[i]])))),  # Number of stanza group
        Stanza = unlist(lapply(stanza_structure, function(x) seq_len(length(x)))),  # number of stanza within group
        GroupNum = seq_len(length(all_stanzas)) + 3,  # Number of stanza outside group TODO: 3 is number of fisheries, presumably
        Group = all_stanzas,  # Group name
        First = stanzas_min[all_stanzas],  # First month of age group
        Last = stanzas_max[all_stanzas] - 1,  # Last month of age group(non-inclusive)
        Z = NA, # total mortality (Z) = fishing mortality + natural mortality (TODO: Generate & hardcode to 0.2?)
        Leading = NA,
        stringsAsFactors = FALSE)
}

# pedigree.csv: 1 for functional/vessel groups
ewe_tbl_pedigree <- function(all_stanzas, catch_data) {
    vessel_groups <- fetch_agg_summary(catch_data, 'vessel')

    tbl_pedigree <- data.frame(
        Group = c(all_stanzas, names(vessel_groups)),
        B = 1,
        PB = 1,
        QB = 1,
        Diet = 1,
        stringsAsFactors = FALSE)
    tbl_pedigree[names(vessel_groups)] <- 1
    return(tbl_pedigree)
}

# model.csv: Catch biomass in t/km^2 for starting year, for each fleet
ewe_tbl_model <- function (all_stanzas, catch_data, area_data) {
    vessel_groups <- fetch_agg_summary(catch_data, 'vessel')

    # Run function for each vessel, returned a named list of the outputs
    foreach_vessel <- function(fn, discards = FALSE) {
        out <- lapply(names(vessel_groups), fn)
        if (discards) {
            names(out) <- paste(names(vessel_groups), "disc", sep = '.')
        } else {
            names(out) <- names(vessel_groups)
        }
        return(out)
    }

    # Join with all_stanzas, assume missing values are 0
    stanza_list <- function(df) {
        df <- merge(data.frame(age = all_stanzas), df, by = c('age'), all.x = TRUE)

        # Replace NAs (i.e. no catch for this age) with zero
        #TODO: This scales by the entire area, should this be swept area for vessels?
        return(replace(df$total_weight, which(is.na(df$total_weight)), 0) / area_data$size)
    }

    # Combine each list argument, turn the whole lot into a data.frame
    df_from_lists <- function (...) {
        do.call(data.frame, c(...))
    }

    rbind(
        df_from_lists(
            list( # Consumers
                Group = all_stanzas,
                Type = 0,  # 0 = consumer, 1 = primary producer, 2 = detritus and 3 = fleets
                Biomass = stanza_list(aggregate(total_weight ~ age, catch_data, sum)),  # Biomass/km^2, from GADGET normally TODO: generate something anyway, in the hope we can apply a translation?
                PB = NA,  # PB: biomass production ratio (Z = total mortality)
                QB = NA,  # QB: consumption biomass ratio. How many times they consume their own biomass in a year (around 3 times for most)
                EE = NA,  # EE: Ecotrophic efficiency. Left as NA unless some of the other parameters is not know then put to 0.8-0.95.
                ProdCons = NA,  # ProdCons: Production consumption ratio
                BioAcc = 0,
                Unassim = 0.2,  # Unassim: unassimilated food
                DetInput = NA,  # DetInput:
                Detritus = 1,  # Detritus: how much goes to detritus
                Discards = 0,  # Discards:
                stringsAsFactors = FALSE),
            foreach_vessel(function (vessel) stanza_list(catch_data[catch_data$vessel == vessel, c('age', 'total_weight')])),
            foreach_vessel(function (vessel) 0, discards = TRUE),
            NULL),
        # NB: Primary producers (type 1) not generated
        df_from_lists(
            list(
                Group = c("Detritus", "Discards"),
                Type = 2,
                Biomass = NA,
                PB = NA,
                QB = NA,
                EE = NA,
                ProdCons = NA,
                BioAcc = 0,
                Unassim = 0.0,
                DetInput = NA,
                Detritus = 0,
                Discards = 0,
                stringsAsFactors = FALSE),
            foreach_vessel(function (vessel) 0),
            foreach_vessel(function (vessel) 0, discards = TRUE),
            NULL),
        df_from_lists(
            list(
                Group = names(vessel_groups),
                Type = 3,
                Biomass = NA,
                PB = NA,
                QB = NA,
                EE = NA,
                ProdCons = NA,
                BioAcc = NA,
                Unassim = NA,
                DetInput = NA,
                Detritus = 0,
                Discards = 1,
                stringsAsFactors = FALSE),
            foreach_vessel(function (vessel) NA),
            foreach_vessel(function (vessel) NA, discards = TRUE),
            NULL))
}

# diet.csv: Proportions for all functional group combinations
ewe_tbl_diet <- function (all_stanzas, catch_data, consumption_data) {
    if (is.null(consumption_data)) {
        return(NULL)
    }
    vessel_groups <- fetch_agg_summary(catch_data, 'vessel')

    age_map <- structure(seq_len(length(all_stanzas)), names = names(all_stanzas))
    list(  # TODO: should be a sparseMatrix
        dims = c(length(all_stanzas), length(all_stanzas)),
        dimnames = list(all_stanzas, all_stanzas),
        i = age_map[consumption_data$predator_age],
        j = age_map[consumption_data$prey_age],
        x = consumption_data$ratio)
}

# Run ewe_tbl_* with given data, return a list with each output
ewe_generate_model <- function(stanza_groups, area_data, catch_data, consumption_data = NULL) {
    if (nrow(area_data) != 1) stop("Should only be 1 row in area_data, not ", nrow(area_data))

    age_groups <- fetch_agg_summary(catch_data, 'age')
    stanza_structure <- lapply(stanza_groups, function (re) grep(re, names(age_groups), value = TRUE))
    all_stanzas <- unname(unlist(stanza_structure))

    # Return a list of files
    return(list(
        stanzas = ewe_tbl_stanzas(stanza_structure, all_stanzas, catch_data),
        stanza_group = ewe_tbl_stanza_group(stanza_structure),
        pedigree = ewe_tbl_pedigree(all_stanzas, catch_data),
        model = ewe_tbl_model(all_stanzas, catch_data, area_data),
        diet = ewe_tbl_diet(all_stanzas, catch_data, consumption_data)))
}
