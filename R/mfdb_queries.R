# Return area, size
mfdb_area_size <- function (mdb, params) {
    if (is.null(params$area)) stop("Must specify 'area' in params")
    mfdb_sample_grouping(mdb,
        group_cols = c("area"),
        calc_cols = c("SUM(c.size) AS size"),
        core_table = "areacell",
        col_defs = list(area = "c.areacell_id"),
        params = params)
}

# Return year, step, area, temperature (mean)
mfdb_temperature <- function (mdb, params = list()) {
    mfdb_survey_index_mean(mdb, c(), c(list(index_type = 'temperature'), params))
}

# Return year, step, area, mean value
mfdb_survey_index_mean <- function (mdb, cols, params) {
    if (!('index_type' %in% names(params))) {
        stop("Need to supply 'index_type' in params. Querying all indicies makes little sense")
    }
    mfdb_sample_grouping(mdb,
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c("AVG(c.value) AS mean"),
        col_defs = list(
            data_source = 'c.data_source_id',
            index_type = "c.index_type_id",

            area = "c.areacell_id",
            year = "c.year",
            step = "c.month"),
        core_table = "survey_index",
        params = params)
}

# Return year, step, area, total value
mfdb_survey_index_total <- function (mdb, cols, params) {
    if (!('index_type' %in% names(params))) {
        stop("Need to supply 'index_type' in params. Querying all indicies makes little sense")
    }
    mfdb_sample_grouping(mdb,
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c("SUM(c.value) AS total"),
        col_defs = list(
            data_source = 'c.data_source_id',
            index_type = "c.index_type_id",

            area = "c.areacell_id",
            year = "c.year",
            step = "c.month"),
        core_table = "survey_index",
        params = params)
}

abundance_core_table <- function (mdb, scale_index) {
    if (is.null(scale_index)) {
        return(c("sample", "c.count"))
    } else if (scale_index == 'tow_length') {
        return(c(
            "(SELECT sam.*, t.length tow_length FROM sample sam, tow t WHERE sam.tow_id = t.tow_id)",
            "c.count / c.tow_length"))
    } else {
        return(c(paste0(
            "(SELECT sam.*, AVG(si.value) abundance",
            " FROM sample sam, survey_index si",
            " WHERE si.index_type_id = ",
                "(SELECT index_type_id",
                " FROM index_type",
                " WHERE case_study_id = ", sql_quote(mdb$case_study_id),
                " AND name = ", sql_quote(scale_index),
                ")",
            " AND sam.case_study_id = si.case_study_id",
            " AND sam.areacell_id = si.areacell_id",
            " AND sam.year = si.year",
            " AND sam.month = si.month",
            " GROUP BY 1",
            ")"), "c.count * c.abundance"))
    }
}

# Return year, step, area, ... , number (of samples)
mfdb_sample_count <- function (mdb, cols, params, scale_index = NULL) {
    abundance <- abundance_core_table(mdb, scale_index)
    mfdb_sample_grouping(mdb,
        core_table = abundance[[1]],
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            paste0("SUM(", abundance[[2]], ") AS number"),
            NULL),
        params = params)
}

# Return year,step,area,age,number (# of samples),mean (length)
mfdb_sample_meanlength <- function (mdb, cols, params, scale_index = NULL) {
    abundance <- abundance_core_table(mdb, scale_index)
    out <- mfdb_sample_grouping(mdb,
        core_table = abundance[[1]],
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            paste0("SUM(", abundance[[2]], ") AS number"),
            paste0("WEIGHTED_MEAN(c.length::numeric, (", abundance[[2]], ")::numeric) AS mean"),
            NULL),
        params = params)
    out
}

# Return year,step,area,age,number (# of samples),mean (length), stddev (length)
mfdb_sample_meanlength_stddev <- function (mdb, cols, params, scale_index = NULL) {
    # TODO: Do we need to know the resolution of the input data to avoid oversampling?
    abundance <- abundance_core_table(mdb, scale_index)
    out <- mfdb_sample_grouping(mdb,
        core_table = abundance[[1]],
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            paste0("SUM(", abundance[[2]], ") AS number"),
            paste0("WEIGHTED_MEAN(c.length::numeric, (", abundance[[2]], ")::numeric) AS mean"),
            paste0("WEIGHTED_STDDEV(c.length::numeric, (", abundance[[2]], ")::numeric) AS stddev"), # TODO: Should take length_var into account
            NULL),
        params = params)
    out
}

# Return year,step,area, ... , total (weight)
mfdb_sample_totalweight <- function (mdb, cols, params) {
    out <- mfdb_sample_grouping(mdb,
        core_table = "sample",
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            paste0("SUM(CASE WHEN count IS NULL THEN weight ELSE weight * count END) AS total_weight"),
            NULL),
        params = params)
    out
}

# Return year,step,area,age,number (# of samples),mean (weight)
mfdb_sample_meanweight <- function (mdb, cols, params, scale_index = NULL) {
    abundance <- abundance_core_table(mdb, scale_index)
    out <- mfdb_sample_grouping(mdb,
        core_table = abundance[[1]],
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            paste0("SUM(", abundance[[2]], ") AS number"),
            paste0("WEIGHTED_MEAN(c.weight::numeric, (", abundance[[2]], ")::numeric) AS mean"),
            NULL),
        params = params)
    out
}

# Return year,step,area,age,number (# of samples),mean (weight), stddev (weight)
mfdb_sample_meanweight_stddev <- function (mdb, cols, params, scale_index = NULL) {
    abundance <- abundance_core_table(mdb, scale_index)
    out <- mfdb_sample_grouping(mdb,
        core_table = abundance[[1]],
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            paste0("SUM(", abundance[[2]], ") AS number"),
            paste0("WEIGHTED_MEAN(c.weight::numeric, (", abundance[[2]], ")::numeric) AS mean"),
            paste0("WEIGHTED_STDDEV(c.weight::numeric, (", abundance[[2]], ")::numeric) AS stddev"), # TODO: Should take weight_var into account
            NULL),
        params = params)
    out
}

# Common definitions for stomach columns
pred_col_defs <- c(
    data_source = 'c.data_source_id',

    institute = 'c.institute_id',
    gear = 'c.gear_id',
    vessel = 'c.vessel_id',
    sampling_type = 'c.sampling_type_id',

    year = 'c.year',
    step = 'c.month',
    area = 'c.areacell_id',

    predator_species = 'c.species_id',
    age = 'c.age',
    sex = 'c.sex_id',
    maturity_stage = 'c.maturity_stage_id',
    stomach_state = 'c.stomach_state_id',
    predator_length = 'c.length',
    predator_weight = 'c.weight',
    NULL)
prey_col_defs <- c(
    prey_species = 'prey.species_id',
    digestion_stage = 'prey.digestion_stage_id',
    prey_length = 'prey.length',
    prey_weight = 'prey.weight',
    prey_count = 'prey.count',
    prey_ratio = 'prey.ratio',
    NULL)

# Returns year, step, area, (cols), number (of prey found in stomach)
mfdb_stomach_preycount <- function (mdb, cols, params) {
    mfdb_sample_grouping(mdb,
        core_table = "predator",
        join_tables = "INNER JOIN prey ON c.predator_id = prey.predator_id",
        col_defs = as.list(c(pred_col_defs, prey_col_defs)),
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            paste0("SUM(prey.count) AS number"),
            NULL),
        params = params)
}

# Returns year, step, area, (cols), number, mean_length (of prey found in stomach)
mfdb_stomach_preymeanlength <- function (mdb, cols, params) {
    mfdb_sample_grouping(mdb,
        core_table = "predator",
        join_tables = "INNER JOIN prey ON c.predator_id = prey.predator_id",
        col_defs = as.list(c(pred_col_defs, prey_col_defs)),
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            paste0("SUM(prey.count) AS number"),
            paste0("WEIGHTED_MEAN(prey.length::numeric, prey.count::numeric) AS mean_length"),
            NULL),
        params = params)
}

# Returns year, step, area, (cols), number, mean_weight (of prey found in stomach)
mfdb_stomach_preymeanweight <- function (mdb, cols, params) {
    mfdb_sample_grouping(mdb,
        core_table = "predator",
        join_tables = "INNER JOIN prey ON c.predator_id = prey.predator_id",
        col_defs = as.list(c(pred_col_defs, prey_col_defs)),
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            paste0("SUM(prey.count) AS number"),
            paste0("WEIGHTED_MEAN(prey.weight::numeric, prey.count::numeric) AS mean_weight"),
            NULL),
        params = params)
}

# Returns year, step, area, (cols), ratio (of selected prey in stomach to all prey by count)
mfdb_stomach_presenceratio <- function (mdb, cols, params) {
    # Group without prey options first
    without_prey <- mfdb_sample_grouping(mdb,
        core_table = "predator",
        col_defs = as.list(c(pred_col_defs)),
        group_cols = c("year", "timestep", "area", intersect(cols, names(pred_col_defs))),
        calc_cols = c(
            "COUNT(DISTINCT c.predator_id) AS stomachs_total",
            NULL),
        params = params)

    # Group with prey restrictions
    with_prey <- mfdb_sample_grouping(mdb,
        core_table = "predator",
        join_tables = c(
            paste0("INNER JOIN prey ON c.predator_id = prey.predator_id"),
            NULL),
        col_defs = as.list(c(pred_col_defs, prey_col_defs)),
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            "COUNT(DISTINCT c.predator_id) AS stomachs_present",
            NULL),
        params = params)

    if (length(with_prey) == 0) return(list())
    # TODO: Bootstrapping is very likely broken
    if (length(with_prey) != length(without_prey)) stop("Don't support bootstrapping for stomachs")

    # If there's nothing to merge, don't bother
    if (nrow(without_prey[[1]]) == 0) return(without_prey)

    # Merge data frames together, return with ratio of present / total
    mapply(function (w, wo) {
        merged <- merge(w, wo)
        merged$ratio <- merged$stomachs_present / merged$stomachs_total
        do.call(structure, c(
            list(merged[, c("year", "step", "area", cols, "ratio"), drop = FALSE]),
            attributes(w)[c("year", "step", "area", cols, "generator")],
            NULL))
    }, with_prey, without_prey, SIMPLIFY = FALSE)
}

# Group up sample data by area, age or length
mfdb_sample_grouping <- function (mdb,
        params = list(),
        group_cols = c("year", "timestep", "area", "age"),
        col_defs = list(
            data_source = "c.data_source_id",
            year = "c.year", step = "c.month", area = "c.areacell_id", age = "c.age",
            maturity_stage = "c.maturity_stage_id", length = "c.length",
            institute = "c.institute_id", gear = "c.gear_id", vessel = "c.vessel_id",
            sampling_type = "c.sampling_type_id", species ="c.species_id", sex = "c.sex_id",
            vessel_type = "v.vessel_type_id", vessel_full_name = "v.full_name",
            vessel_length = "v.length", vessel_power = "v.power", vessel_tonnage = "v.tonnage",
            tow = "c.tow_id", tow_latitude = "t.latitude", tow_longitude = "t.longitude", tow_depth = "t.depth", tow_length = "t.length",
        NULL),
        calc_cols = c(),
        core_table = "sample",
        # join_tables: JOIN statements to attach to the main table
        join_tables = c(),
        generator = as.character(sys.call(-1)[[1]])) {

    if (!is.list(params)) {
        stop("params argument must be a list")
    }

    # Bodge timestep -> step inconsistency
    group_cols <- gsub("^timestep$", "step", group_cols)
    if ('step' %in% group_cols && 'timestep' %in% names(params)) {
        params$step <- params$timestep
    }

    # Call relevant clause function for all group_cols
    clauses <- function (cols, fn) {
        unlist(lapply(cols, function (col) fn(
                mdb,
                params[[col]],
                col_defs[[col]],
                col)))
    }

    # Importing is probably done, so create indexes if we need to
    mfdb_finish_import(mdb)

    # All group_cols should be valid
    for (col in group_cols) {
        if (is.null(col_defs[[col]])) {
            stop("Unknown column ", col)
        }
    }

    # If we need tow or vessel, join these tables
    if (length(grep("^v\\.", col_defs[names(params)])) > 0) {
        join_tables = c("JOIN vessel v ON c.case_study_id = v.case_study_id AND c.vessel_id = v.vessel_id", join_tables)
    }
    if (length(grep("^t\\.", col_defs[names(params)])) > 0) {
        join_tables = c("JOIN tow t ON c.case_study_id = t.case_study_id AND c.tow_id = t.tow_id", join_tables)
    }

    # Pick out any extra params we can make use of, ignore rest
    filter_cols <- intersect(names(params), names(col_defs))

    # Call pre-query for all groups
    for (col in union(group_cols, filter_cols)) {
        x <- pre_query(mdb, params[[col]], col_defs[[col]])
    }

    out <- mfdb_fetch(mdb,
        "SELECT ", paste(c(
            paste(paste(
                clauses(group_cols, sample_clause),
                NULL, collapse = "|| '.' ||"), "AS bssample"),
            gsub("AS ([a-zA-Z0-9_]+)$", "AS grp_\\1", clauses(group_cols, select_clause)),
            calc_cols,
            NULL), collapse = ","),
        " FROM ", paste(c(
            paste(c(core_table, "c", join_tables), collapse = " "),
            clauses(union(group_cols, filter_cols), from_clause),
            NULL), collapse = ","),
        " WHERE ", paste(c(
            paste("c.case_study_id =", sql_quote(mdb$case_study_id)),
            clauses(union(group_cols, filter_cols), where_clause),
            NULL), collapse = " AND "),
        " GROUP BY ", paste(c(
            "bssample",
            paste0("grp_", group_cols),
            NULL), collapse=","),
        " ORDER BY ", paste(c(
            "bssample",
            paste0("grp_", group_cols),
            NULL), collapse=","),
        NULL)
    names(out) <- gsub("grp_(.*)", "\\1", names(out))

    # No data, so fake enough to generate list of empty frames
    if (nrow(out) == 0) {
        # Get list of probable sample names
        samples <- lapply(group_cols, function(col) {
            if("mfdb_bootstrap_group" %in% class(params[[col]])) {
                s <- seq(0, length(params[[col]]) - 1)
                return(vapply(s, as.character, ""))
            }
            return(c("0"))
        })
        # Flatten this into a vector of concatenated sample names
        denormalised <- apply(
            do.call(data.frame, samples),
            1,
            function(x) paste(x, collapse="."))
        out <- data.frame(bssample = denormalised)
    }

    # Break data up by sample and annotate each with the value for group_cols
    lapply(split(out, list(out$bssample)), function (sample) {
        if (identical(names(sample), c('bssample'))) {
            # No data, just generating dummy groups
            subset <- data.frame()
            sample_num <- rep(NA, length(group_cols))
        } else {
            # Select our final output columns
            subset <- sample[,c(
                group_cols,
                gsub(".*\\s+AS\\s+(\\w+)", "\\1", calc_cols),
                NULL), drop = FALSE]
            rownames(subset) <- NULL
            sample_num <- as.integer(strsplit(as.character(sample$bssample[[1]]), "\\.")[[1]])
        }
        do.call(structure, c(
            list(subset),
            structure(
                lapply(seq_len(length(group_cols)), function(i) agg_summary(
                    mdb,
                    params[[group_cols[[i]]]],
                    col_defs[[group_cols[[i]]]],
                    group_cols[[i]],
                    sample,
                    sample_num[[i]])),
                names = group_cols),
            list(generator = generator)
        ))
    })
}
