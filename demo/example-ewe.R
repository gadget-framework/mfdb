# Prototype EwE support, see bottom of file for usage
library(mfdb)
library(Matrix)

ewe_generate_model <- function(catch_data, consumption_data, stanza_groups) {
    age_groups <- attr(catch_data, 'age')
    if (is.null(age_groups)) stop("Can't find age grouping data for catch_data")
    stanza_structure <- lapply(stanza_groups, function (re) grep(re, names(age_groups), value = TRUE))
    all_stanzas <- unlist(stanza_structure)

    vessel_groups <- attr(catch_data, 'vessel')
    if (is.null(vessel_groups)) stop("Can't find age grouping data for catch_data")

    tbl_stanza_group <- data.frame(
        StGroupNum = 1:length(stanza_structure),
        Stanza_Group = names(stanza_structure),
        nstanzas = vapply(stanza_structure, length, 0),  # Number of stanzas in group
        VBGF_Ksp = NA, #  The K in the von Bartalanffy
        VBGF_d = 0.66667,  # Constant.
        Wmat = NA,  # Weight at maturity
        RecPower = 1,  # Constant.
        stringsAsFactors = FALSE)

    # Get the min and max values for all age groups
    stanzas_min <- unlist(mfdb:::agg_prop(age_groups, "min"))
    stanzas_max <- unlist(mfdb:::agg_prop(age_groups, "max"))
    tbl_stanzas <- data.frame(
        StGroupNum = unlist(lapply(seq_len(length(stanza_structure)), function(i) rep(i, times = length(stanza_structure[[i]])))),  # Number of stanza group
        Stanza = unlist(lapply(stanza_structure, function(x) seq_len(length(x)))),  # number of stanza within group
        GroupNum = seq_len(length(all_stanzas)) + 3,  # Number of stanza outside group TODO: 3 is number of fisheries, presumably
        Group = all_stanzas,  # Group name
        First = stanzas_min[all_stanzas],  # First month of age group
        Last = stanzas_max[all_stanzas] - 1,  # Last month of age group(non-inclusive)
        Z = NA, # total mortality (Z) = fishing mortality + natural mortality (TODO: Generate & hardcode to 0.2?)
        Leading = NA,
        stringsAsFactors = FALSE)

    tbl_pedigree <- data.frame(
        Group = all_stanzas,
        B = 1,
        PB = 1,
        QB = 1,
        Diet = 1,
        stringsAsFactors = FALSE)
    # TODO: Need some data tbl_pedigree[names(vessel_groups)] <- 1

    # model.csv: Catch biomass in t/km^2 for starting year, for each fleet
    tbl_model <- data.frame(
        Group = all_stanzas,
        Type = 0,  # 0 = consumer, 1 = primary producer, 2 = detritus and 3 = fleets
        Biomass = NA,  # Biomass/km^2, from GADGET normally TODO: generate something anyway, in the hope we can apply a translation?
        PB = NA,  # PB: biomass production ratio (Z = total mortality)
        QB = NA,  # QB: consumption biomass ratio. How many times they consume their own biomass in a year (around 3 times for most)
        EE = NA,  # EE: Ecotrophic efficiency. Left as NA unless some of the other parameters is not know then put to 0.8-0.95.
        ProdCons = NA,  # ProdCons: Production consumption ratio
        Unassim = NA,  # Unassim: unassimilated food
        DetInput = NA,  # DetInput:
        Detritus = NA,  # Detritus: how much goes to detritus
        Discards = NA,  # Discards:
        # TODO: Per fishery biomass
        # TODO: Per fishery biomass discards
        stringsAsFactors = FALSE)

    # diet.csv: Proportions for all functional group combinations
    age_map <- structure(seq_len(length(all_stanzas)), names = names(all_stanzas))
    tbl_diet <- list(  # TODO: should be a sparseMatrix
        dims = c(length(all_stanzas), length(all_stanzas)),
        dimnames = list(all_stanzas, all_stanzas),
        i = age_map[consumption_data$predator_age],
        j = age_map[consumption_data$prey_age],
        x = consumption_data$ratio)

    # Return a list of models
    return(list(
        stanzas = tbl_stanzas,
        stanza_group = tbl_stanza_group,
        pedigree = tbl_pedigree,
        model = tbl_model,
        diet = tbl_diet))
}

# Steal the data that example-datras generates
mdb <- mfdb('Baltic')

# Configure functional groups
start_year <- mfdb_group(decade = 1990:2000)  # Would normally be 1 year, but we cheat to get data
grouping_cod <- list(
    species = 'COD',
    age = mfdb_step_interval('cod', 2, to = 10))
grouping_had <- list(
    species = 'COD',  # It's haddock, honest
    age = mfdb_step_interval('had', 10, from = 10, to = 40))
grouping_vessel = list(
    vessel = mfdb_unaggregated())
stanza_groups <- list(
    'all_cod' = '^cod',
    'all_haddock' = '^had')

# Query data and group together
catch_data <- mfdb_concatenate_results(
    mfdb_sample_totalweight(mdb, c('age', 'vessel'), c(grouping_cod, grouping_vessel))[[1]],
    mfdb_sample_totalweight(mdb, c('age', 'vessel'), c(grouping_had, grouping_vessel))[[1]])
consumption_data <- mfdb_concatenate_results(
    mfdb_stomach_presenceratio(mdb, c('age'), grouping_cod)[[1]],
    mfdb_stomach_presenceratio(mdb, c('age'), grouping_had)[[1]])

# Generate CSVs and print out each
model_files <- ewe_generate_model(catch_data, consumption_data, stanza_groups)
for (x in names(model_files)) {
    cat("== ", x, " ==============\n")
    print(model_files[[x]])
}

# Example output from script:-
# ==  stanzas  ==============
#              StGroupNum Stanza GroupNum Group First Last  Z Leading
# all_cod1              1      1        4  cod0     0    1 NA      NA
# all_cod2              1      2        5  cod2     2    3 NA      NA
# all_cod3              1      3        6  cod4     4    5 NA      NA
# all_cod4              1      4        7  cod6     6    7 NA      NA
# all_cod5              1      5        8  cod8     8    9 NA      NA
# all_haddock1          2      1        9 had10    10   19 NA      NA
# all_haddock2          2      2       10 had20    20   29 NA      NA
# all_haddock3          2      3       11 had30    30   39 NA      NA
# ==  stanza_group  ==============
#             StGroupNum Stanza_Group nstanzas VBGF_Ksp  VBGF_d Wmat RecPower
# all_cod              1      all_cod        5       NA 0.66667   NA        1
# all_haddock          2  all_haddock        3       NA 0.66667   NA        1
# ==  pedigree  ==============
#              Group B PB QB Diet
# all_cod1      cod0 1  1  1    1
# all_cod2      cod2 1  1  1    1
# all_cod3      cod4 1  1  1    1
# all_cod4      cod6 1  1  1    1
# all_cod5      cod8 1  1  1    1
# all_haddock1 had10 1  1  1    1
# all_haddock2 had20 1  1  1    1
# all_haddock3 had30 1  1  1    1
# ==  model  ==============
#              Group Type Biomass PB QB EE ProdCons Unassim DetInput Detritus Discards
# all_cod1      cod0    0      NA NA NA NA       NA      NA       NA       NA       NA
# all_cod2      cod2    0      NA NA NA NA       NA      NA       NA       NA       NA
# all_cod3      cod4    0      NA NA NA NA       NA      NA       NA       NA       NA
# all_cod4      cod6    0      NA NA NA NA       NA      NA       NA       NA       NA
# all_cod5      cod8    0      NA NA NA NA       NA      NA       NA       NA       NA
# all_haddock1 had10    0      NA NA NA NA       NA      NA       NA       NA       NA
# all_haddock2 had20    0      NA NA NA NA       NA      NA       NA       NA       NA
# all_haddock3 had30    0      NA NA NA NA       NA      NA       NA       NA       NA
# ==  diet  ==============
# $dimnames
# $dimnames[[1]]
#     all_cod1     all_cod2     all_cod3     all_cod4     all_cod5 all_haddock1 all_haddock2 all_haddock3 
#       "cod0"       "cod2"       "cod4"       "cod6"       "cod8"      "had10"      "had20"      "had30" 
# 
# $dimnames[[2]]
#     all_cod1     all_cod2     all_cod3     all_cod4     all_cod5 all_haddock1 all_haddock2 all_haddock3 
#       "cod0"       "cod2"       "cod4"       "cod6"       "cod8"      "had10"      "had20"      "had30" 
