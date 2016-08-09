# Fetch the agg_summary off a data.frame
fetch_agg_summary <- function (data, name) {
   out <- attr(data, name)
   if (is.null(out)) stop("Can't find ", data, " grouping data for survey_data")
   return(out)
}

# List of character lists of all the grouping columns in the df
stanza_list <- function (df, ignore_re = c(), include_re = c()) {
    # Generate boolean vector of what to include
    if (length(include_re) > 0) {
        cols <- grepl(paste(include_re, collapse="|"), names(df))
    } else {
        cols <- rep(TRUE, times = ncol(df))
    }
    cols <- cols & !grepl(paste(c("^total_weight$", "^ratio$", ignore_re), collapse="|"), names(df))

    apply(
        df[, cols, drop = FALSE],
        1,
        # Strip anything "all", return list so apply doesn't apply simplification
        function (row) as.list(row[row != 'all']))
}

# Fetch the value at position i from each of the lists
stanza_col <- function (group_stanzas, i) {
    vapply(group_stanzas, function (s) s[[i]], "")
}

# Generate labels for list-of-list stanzas
stanza_labels <- function (group_stanzas) {
    vapply(group_stanzas, function (x) paste(x, collapse="."), "")
}

# stanza_group.csv: functional group definition.
ewe_stanza_group <- function (survey_data) {
    stanzas <- stanza_list(survey_data)

    # Ignore stanzas that don't have at least 2 labels
    group_stanzas <- stanzas[vapply(stanzas, length, 0) >= 2]
    parent_stanzas <- table(stanza_col(group_stanzas, 1))

    data.frame(
        StGroupNum = 1:length(parent_stanzas),
        Stanza_Group = names(parent_stanzas),
        nstanzas = as.vector(parent_stanzas),  # Number of stanzas in group
        VBGF_Ksp = NA, #  The K in the von Bartalanffy
        VBGF_d = 0.66667,  # Constant.
        Wmat = NA,  # Weight at maturity
        RecPower = 1,  # Constant.
        stringsAsFactors = FALSE)
}

# stanzas.csv: List of all functional groups, min/max ages
ewe_stanzas <- function(survey_data) {
    stanzas <- stanza_list(survey_data)

    # Ignore stanzas that don't have at least 2 labels
    group_stanzas <- stanzas[vapply(stanzas, length, 0) >= 2]
    parent_stanzas <- table(stanza_col(group_stanzas, 1))

    # Get the agg_prop value relating to each of the stanzas
    get_age_prop <- function (group_stanzas, prop_name) {
        if (is.null(survey_data)) return(NULL);

        values_for_prop <- unlist(agg_prop(fetch_agg_summary(survey_data, 'age'), prop_name))
        return(values_for_prop[stanza_col(group_stanzas, 'age')])
    }

    st_group_num <- function (parent_stanzas) {
        # Return s repeated as many times as parent_stanzas thinks is required
        st_rep <- function (s) {
            rep(s, times=parent_stanzas[s])
        }

        # For each index in parent_stanzas, generate a list of indexes
        unlist(lapply(seq_len(length(parent_stanzas)), st_rep))
    }

    data.frame(
        StGroupNum = st_group_num(parent_stanzas),  # Number of stanza group
        Stanza = unlist(lapply(parent_stanzas, seq_len)),  # number of stanza within group
        GroupNum = seq_len(length(group_stanzas)) + 3,  # Number of stanza outside group TODO: 3 is number of fisheries, presumably
        Group = stanza_labels(group_stanzas), # Group name, i.e. each list collapsed
        First = get_age_prop(group_stanzas, 'min'),  # First month of age group
        Last = get_age_prop(group_stanzas, 'max'),  # Last month of age group(non-inclusive)
        Z = NA, # total mortality (Z) = fishing mortality + natural mortality (TODO: Generate & hardcode to 0.2?)
        Leading = NA,
        stringsAsFactors = FALSE)
}

# model.csv: Catch biomass in t/km^2 for starting year, for each fleet
ewe_model <- function (area_data, survey_data, catch_data = NULL) {
    stanzas <- stanza_list(survey_data)

    if (nrow(area_data) != 1) stop("Should only be 1 row in area_data, not ", nrow(area_data))

    consumer_names <- stanza_labels(stanzas)
    detritus_names <- c("Detritus", "Discards")
    pproducer_names <- c()
    fleet_names <- if (is.null(catch_data)) c() else names(attr(catch_data, 'vessel'))

    out <- data.frame(
        Group = c(
            consumer_names,
            detritus_names,
            pproducer_names,
            fleet_names),
        Type = c(  # 0 = consumer, 1 = primary producer, 2 = detritus and 3 = fleets
            rep(0, length(consumer_names)),
            rep(1, length(pproducer_names)),
            rep(2, length(detritus_names)),
            rep(3, length(fleet_names))),
        Biomass = c(  # Biomass/km^2, from GADGET normally
            survey_data$total_weight / area_data$size,
            rep(NA, length(pproducer_names)),
            rep(NA, length(detritus_names)),
            rep(NA, length(fleet_names))),
        PB = NA,  # PB: biomass production ratio (Z = total mortality)
        QB = NA,  # QB: consumption biomass ratio. How many times they consume their own biomass in a year (around 3 times for most)
        EE = NA,  # EE: Ecotrophic efficiency. Left as NA unless some of the other parameters is not know then put to 0.8-0.95.
        ProdCons = NA,  # ProdCons: Production consumption ratio
        BioAcc = c(
            rep(0, length(consumer_names)),
            rep(0, length(pproducer_names)),
            rep(0, length(detritus_names)),
            rep(NA, length(fleet_names))),
        Unassim = c(  # Unassim: unassimilated food
            rep(0.2, length(consumer_names)),
            rep(0,2, length(pproducer_names)),
            rep(0.2, length(detritus_names)),
            rep(NA, length(fleet_names))),
        DetInput = c(  # DetInput:
            rep(NA, length(consumer_names)),
            rep(NA, length(pproducer_names)),
            rep(0, length(detritus_names)),
            rep(NA, length(fleet_names))),
        Detritus = c(  # Detritus: how much goes to detritus
            rep(1, length(consumer_names)),
            rep(0, length(pproducer_names)),
            rep(0, length(detritus_names)),
            rep(0, length(fleet_names))),
        Discards = c(  # Discards:
            rep(0, length(consumer_names)),
            rep(0, length(pproducer_names)),
            rep(0, length(detritus_names)),
            rep(1, length(fleet_names))),
        stringsAsFactors = FALSE)

     # Add vessel columns
     for (v in fleet_names) {
         vessel_data <- catch_data[catch_data$vessel==v, names(catch_data) != 'vessel']
         vessel_out <- data.frame(
             Group = stanza_labels(stanza_list(vessel_data)),
             total_weight = vessel_data$total_weight,
             stringsAsFactors = FALSE)

         # Create new column with either the corresponding vessel value, 0 or NA
         out[,v] <- vapply(out$Group, function (g) {
             if (g %in% fleet_names) return(NA)

             tw <- vessel_out[vessel_out$Group == g, 'total_weight']
             if(length(tw) == 0) 0 else tw[[1]] / area_data$size  # TODO: Should we be scaling by swept area?
         }, 0)
     }

     # Add vessel discard placeholders
     for (v in unique(catch_data$vessel)) {
         # Create new column with either the corresponding vessel value, 0 or NA
         out[,paste0(v, '.disc')] <- vapply(out$Group, function (g) {
             if (g %in% fleet_names) return(NA)

             return(0)
         }, 0)
     }

     return(out[with(out, order(Type, Group)), ])
}

# diet.csv: Proportions for all functional group combinations
ewe_diet <- function (consumption_data) {
    if (is.null(consumption_data)) {
        return(NULL)
    }

    # Make data.frame of labels to values
    df <- data.frame(
        predator = stanza_labels(stanza_list(consumption_data, ignore_re = '^prey_')),
        prey = stanza_labels(stanza_list(consumption_data, include_re = '^prey_')),
        ratio = consumption_data$ratio,
        stringsAsFactors = FALSE)
    # Make a combinatoral explosion of all predator/preys with NAs
    empty_df <- expand.grid(
        predator = unique(c(df$predator, df$prey)),
        prey = unique(c(df$predator, df$prey)),
        ratio = NA)
    # Combine, take the first value (i.e. the actual value if it exists, fall back to NA)
    full_df <- aggregate(
        ratio ~ predator + prey,
        rbind(df, empty_df),
        function (x) x[[1]],
        na.action = na.pass)
    # Turn this into a matrix
    matrix(
        nrow = length(unique(full_df$predator)),
        ncol = length(unique(full_df$prey)),
        dimnames = list(unique(full_df$predator), unique(full_df$prey)),
        data = full_df$ratio,
        byrow = TRUE)
}

# pedigree.csv: Lots of 1
ewe_pedigree <- function (survey_data, catch_data = NULL) {
    stanzas <- stanza_list(survey_data)

    consumer_names <- stanza_labels(stanzas)
    detritus_names <- c("Detritus", "Discards")
    pproducer_names <- c()
    fleet_names <- if (is.null(catch_data)) c() else names(attr(catch_data, 'vessel'))

    out <- data.frame(
        Group = c(
            consumer_names,
            detritus_names,
            pproducer_names,
            fleet_names),
        B = 1,
        PB = 1,
        QB = 1,
        Diet = 1,
        stringsAsFactors = FALSE)

     # Add vessel columns
     for (v in fleet_names) {
         out[,v] <- 1
     }

     return(out[with(out, order(Group)),])
}
