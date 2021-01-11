# Given survey_data & catch_data, call create.rpath.params()
mfdb_rpath_params <- function (area_data,
                               survey_data,
                               catch_data,
                               consumption_data,
                               create_rpath_params = stop("Set create_rpath_params = Rpath::create.rpath.params"),
                               living_groups = character(0),
                               detritus_groups = c("Detritus")) {
    if (nrow(area_data) != 1) stop("Should only be 1 row in area_data, not ", nrow(area_data))

    # Strip off result columns and year/time columns, build rpath group out of what's left
    df_to_rpath_groups <- function(df, ignore_cols = c()) {
        ignore_re <- paste(c(
            '^year$', '^step$', '^area$',
            '^vessel$', '^gear$',
            '^prey_species$',
            '^total_weight$', '^ratio$',
            ignore_cols), collapse = '|')
        cols <- !grepl(ignore_re, names(df))

        # Combine remaining columns with "." to make our group name
        group_names <- do.call(paste, c(list(sep = "."), df[,cols, drop = F]))
        return(group_names)
    }

    # split name by ., return first half if there is one, NA otherwise
    to_stgroup <- function (stanza_names) {
        out <- strsplit(stanza_names, '.', fixed = TRUE)
        out <- vapply(out, function (x) {
            if (length(x) > 1) x[[1]] else as.character(NA)
        }, character(1))
        return(out)
    }

    # Primary groups are everything mentioned in survey/catch/prey
    primary_groups <- unique(c(
        df_to_rpath_groups(survey_data),
        df_to_rpath_groups(catch_data),
        consumption_data[,'prey_species'],
        NULL))
    fleet_groups <- unique(catch_data[,'vessel'])

    stgroups <- c(
        to_stgroup(living_groups),
        to_stgroup(primary_groups),
        to_stgroup(detritus_groups),
        rep(NA, length(fleet_groups)))  # No stanza groups for fisheries
    # If there aren't any, Rpath expects a single NA
    if (all(is.na(stgroups))) stgroups <- NA
    rp <- create_rpath_params(
        group = c(
            living_groups,
            primary_groups,
            detritus_groups,
            fleet_groups),
        type = c(
            rep(0, length(living_groups)),
            rep(1, length(primary_groups)),
            rep(2, length(detritus_groups)),
            rep(3, length(fleet_groups))),
        stgroup = stgroups)

    # TODO: These are all really data.tables, and should be treated accordingly,
    #       but using both data.table and rlang in the same package appears to be
    #       a recipe for disaster.

    # Populate biomass
    rp$model$Biomass <- survey_data[match(
        rp$model$Group,
        df_to_rpath_groups(survey_data)), 'total_weight'] / area_data$size

    # Populate catch
    for (vessel in fleet_groups) {
        v_catch_data <- catch_data[catch_data$vessel == vessel, ]
        rp$model[[vessel]] <- v_catch_data[match(
            rp$model$Group,
            df_to_rpath_groups(v_catch_data)), 'total_weight']
    }

    # Populate diet matrix
    for (predator in df_to_rpath_groups(consumption_data)) {
        p_consumption_data <- consumption_data[df_to_rpath_groups(consumption_data) == predator,]
        rp$diet[[predator]] <- p_consumption_data[match(
            rp$diet$Group,
            p_consumption_data$prey_species), 'ratio']
    }

    return(rp)
}
