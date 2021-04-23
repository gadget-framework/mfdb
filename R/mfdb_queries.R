# Extract foreign key table from definition
col_def_get_foreign_key <- function (x) {
    m <- regexec("REFERENCES (\\w+)\\((\\w+)\\)", x)
    m <- regmatches(x, m)[[1]]
    if (length(m) == 0) list(table = NULL, column = NULL) else list(table = m[2], column = m[3])
}

# Generate col_defs for a table and all it's foreign keys
col_defs_for <- function (table_name, prefix = "", referencing_col = NULL) {
    # Add prefix only when it's not ""
    to_external_name <- function(x) {
        # External names don't mention id
        x <- gsub("_id$", "", x)
        if (prefix == "") return(x)

        out <- paste(prefix, x, sep = "_")
        # Don't want to call the column vessel_vessel_type_id
        out <- gsub(paste0("^", prefix, "_", prefix), prefix, out)
        return(out)
    }

    # Find column definitions, turn into matrix for easy access
    if (table_name %in% names(mfdb_measurement_table_defs)) {
        col_defs <- mfdb_measurement_table_defs[[table_name]]$cols
    } else if (table_name %in% names(mfdb_taxonomy_table_defs)) {
        col_defs <- mfdb_taxonomy_table_defs[[table_name]]$cols
    } else {
        stop("Unknown table ", table_name)
    }
    col_defs <- t(matrix(col_defs, nrow = 3))

    # Don't care about primary keys or description
    col_defs <- col_defs[!grepl("PRIMARY KEY", col_defs[,2]),,drop = F]
    col_defs <- col_defs[col_defs[,1] != 'description',,drop = F]
    if (nrow(col_defs) == 0) return(character(0))  # Nothing to do

    # Generate SQL that will join appropriately
    join_sql <- if (is.null(referencing_col)) NULL else paste0(
        "JOIN ", table_name, " AS ", prefix,
        " ON ", referencing_col, " = ", prefix, ".", table_name, "_id",
        "")
    sql_prefix <- if (prefix == "") "c" else prefix

    # Build list of all tables and their columns
    out <- c(
        # Recurse over foreign key relationships, one list entry for each
        lapply(grep("REFERENCES", col_defs[,2]), function (i) {
            col_defs_for(
                col_def_get_foreign_key(col_defs[i,2])$table,
                prefix = paste0(prefix, if(prefix != "") "_", gsub("_id$", "", col_defs[i,1])),
                referencing_col = paste(sql_prefix, col_defs[i,1], sep = "."))
        }),
        # Build keys for this table
        list(structure(
            lapply(  # i.e turn paste(...) into a list, adding join_sql as we go
                seq_len(nrow(col_defs)),
                function (i) structure(
                    paste(sql_prefix, col_defs[i,1], sep = "."),
                    lookup = col_def_get_foreign_key(col_defs[i,2])$table,
                    join_sql = join_sql)),
            names = to_external_name(col_defs[,1]))))

    # Flatten list of lists
    return(do.call(c, out))
}

# Return area, size
mfdb_area_size <- function (mdb, params) {
    mfdb_sample_grouping(mdb,
        group_cols = c("area"),
        calc_cols = c("SUM(c.size) AS size"),
        core_table = "areacell",
        col_defs = list(area = "c.areacell_id"),
        params = params)
}
# Return area, size, mean_depth
mfdb_area_size_depth <- function (mdb, params) {
    mfdb_sample_grouping(mdb,
        group_cols = c("area"),
        calc_cols = c(
            "SUM(c.size) AS size",
            paste0("SUM(CAST(c.depth AS numeric) * CAST(c.size AS numeric)) AS mean_depth__sum"),
            paste0("SUM(CAST(c.size AS numeric)) AS mean_depth__count"),
            NULL),
        core_table = "areacell",
        col_defs = list(area = "c.areacell_id"),
        params = params)
}

# Return year, step, area, temperature (mean)
mfdb_temperature <- function (mdb, params = list()) {
    mfdb_survey_index_mean(mdb, c(), c(list(index_type = 'temperature'), params))
}

abundance_survey_index_table <- function (mdb, scale_index) {
    if (is.null(scale_index)) {
        return(c("survey_index", 1))
    } else if (scale_index == 'tow_length') {
        stop("A survey index isn't associated with a tow")
    } else if (scale_index == 'area_size') {
        return(c(
            "(SELECT si.*, a.size area_size FROM survey_index si, areacell a WHERE si.areacell_id = a.areacell_id)",
            "c.area_size"))
    } else {
        return(c(paste0(
            "(SELECT sam.*, AVG(si.value) abundance",
            " FROM survey_index sam, survey_index si",
            " WHERE si.index_type_id = ",
                "(SELECT index_type_id",
                " FROM index_type",
                " WHERE name = ", sql_quote(scale_index),
                ")",
            " AND sam.areacell_id = si.areacell_id",
            " AND sam.year = si.year",
            " AND sam.month = si.month",
            " GROUP BY 1",
            ")"), "c.abundance"))
    }
}

# Return year, step, area, mean value
mfdb_survey_index_mean <- function (mdb, cols, params, scale_index = NULL) {
    if (!('index_type' %in% names(params))) {
        stop("Need to supply 'index_type' in params. Querying all indicies makes little sense")
    }
    abundance <- abundance_survey_index_table(mdb, scale_index)
    mfdb_sample_grouping(mdb,
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            if (abundance[[2]] == 1)
                paste0("AVG(c.value) AS mean")
            else c(
                paste0("SUM(CAST(c.value AS numeric) * CAST(", abundance[[2]], " AS numeric)) AS mean__sum"),
                paste0("SUM(CAST(", abundance[[2]], " AS numeric)) AS mean__count"))),
        col_defs = list(
            data_source = 'c.data_source_id',
            index_type = "c.index_type_id",

            area = "c.areacell_id",
            year = "c.year",
            step = "c.month"),
        core_table = abundance[[1]],
        params = params)
}

# Return year, step, area, total value
mfdb_survey_index_total <- function (mdb, cols, params, scale_index = NULL) {
    if (!('index_type' %in% names(params))) {
        stop("Need to supply 'index_type' in params. Querying all indicies makes little sense")
    }
    abundance <- abundance_survey_index_table(mdb, scale_index)
    mfdb_sample_grouping(mdb,
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            if (abundance[[2]] == 1)
                paste0("SUM(c.value) AS total")
            else
                paste0("SUM(c.value * ", abundance[[2]], ") AS total")),
        col_defs = list(
            data_source = 'c.data_source_id',
            index_type = "c.index_type_id",

            area = "c.areacell_id",
            year = "c.year",
            step = "c.month"),
        core_table = abundance[[1]],
        params = params)
}

abundance_core_table <- function (mdb, scale_index) {
    if (is.null(scale_index)) {
        return(c("sample", "c.count"))
    } else if (scale_index == 'tow_length') {
        return(c(
            "(SELECT sam.*, t.length tow_length FROM sample sam, tow t WHERE sam.tow_id = t.tow_id)",
            "c.count / c.tow_length"))
    } else if (scale_index == 'area_size') {
        return(c(
            "(SELECT sam.*, a.size area_size FROM sample sam, areacell a WHERE sam.areacell_id = a.areacell_id)",
            "c.count * c.area_size"))
    } else {
        return(c(paste0(
            "(SELECT sam.*, AVG(si.value) abundance",
            " FROM sample sam, survey_index si",
            " WHERE si.index_type_id = ",
                "(SELECT index_type_id",
                " FROM index_type",
                " WHERE name = ", sql_quote(scale_index),
                ")",
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
            paste0("SUM(CAST(c.length AS numeric) * CAST(", abundance[[2]], " AS numeric)) AS mean__sum"),
            paste0("SUM(CAST(", abundance[[2]], " AS numeric)) AS mean__count"),
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
            paste0("SUM(CAST(c.length AS numeric) * CAST(", abundance[[2]], " AS numeric)) AS mean__sum"),
            paste0("SUM(CAST(", abundance[[2]], " AS numeric)) AS mean__count"),
            paste0("SUM(CAST(c.length AS numeric) * CAST(c.length AS numeric) * CAST(", abundance[[2]], " AS numeric)) AS mean__sqsum"),   # TODO: Should take length_var into account
            NULL),
        params = params)
    out
}

# Helper to translate measurements argument
measurements_to_sql <- function (measurements) {
    measurement_cols <- c(
        overall = 'weight',
        liver = 'liver_weight',
        gonad = 'gonad_weight',
        stomach = 'stomach_weight')

    if (!all(measurements %in% names(measurement_cols))) {
        stop("Measurement values should be one of ",
            paste(names(measurement_cols), collapse = ","),
            " not: ",
            paste(measurements, collapse = ","))
    }
    return(measurement_cols[measurements])
}

# Return year,step,area, ... , total (weight)
mfdb_sample_totalweight <- function (mdb, cols, params, measurements = c('overall')) {
    col_sql <- measurements_to_sql(measurements)
    col_totallabel <- paste0('total_', col_sql)

    out <- mfdb_sample_grouping(mdb,
        core_table = "sample",
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            paste0("SUM(CASE WHEN count IS NULL THEN ", col_sql, " ELSE ", col_sql, " * count END) AS ", col_totallabel),
            NULL),
        params = params)
    out
}

# Return year,step,area,age,number (# of samples),mean (weight)
mfdb_sample_meanweight <- function (mdb, cols, params, scale_index = NULL, measurements = c('overall')) {
    col_sql <- measurements_to_sql(measurements)
    col_meanlabel <- ifelse(col_sql == 'weight', 'mean', paste0('mean_', col_sql))

    abundance <- abundance_core_table(mdb, scale_index)
    out <- mfdb_sample_grouping(mdb,
        core_table = abundance[[1]],
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            paste0("SUM(", abundance[[2]], ") AS number"),
            paste0("SUM(CAST(c.", col_sql, " AS numeric) * CAST(", abundance[[2]], " AS numeric)) AS ", col_meanlabel, "__sum"),
            paste0("SUM(CAST(", abundance[[2]], " AS numeric)) AS ", col_meanlabel, "__count"),
            NULL),
        params = params)
    out
}

# Return year,step,area,age,number (# of samples),mean (weight), stddev (weight)
mfdb_sample_meanweight_stddev <- function (mdb, cols, params, scale_index = NULL, measurements = c('overall')) {
    col_sql <- measurements_to_sql(measurements)
    col_meanlabel <- ifelse(col_sql == 'weight', 'mean', paste0('mean_', col_sql))

    abundance <- abundance_core_table(mdb, scale_index)
    out <- mfdb_sample_grouping(mdb,
        core_table = abundance[[1]],
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            paste0("SUM(", abundance[[2]], ") AS number"),
            paste0("SUM(CAST(c.", col_sql, " AS numeric) * CAST(", abundance[[2]], " AS numeric)) AS ", col_meanlabel, "__sum"),
            paste0("SUM(CAST(", abundance[[2]], " AS numeric)) AS ", col_meanlabel, "__count"),
            paste0("SUM(CAST(c.", col_sql, " AS numeric) * CAST(c.", col_sql, " AS numeric) * CAST(", abundance[[2]], " AS numeric)) AS ", col_meanlabel, "__sqsum"),
            # TODO: Should take length_var into account
            NULL),
        params = params)
    out
}

mfdb_sample_rawdata <- function (mdb, cols, params, scale_index = NULL) {
    abundance <- abundance_core_table(mdb, scale_index)
    out <- mfdb_sample_grouping(mdb,
        core_table = abundance[[1]],
        disable_group_by = TRUE,
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            paste0(abundance[[2]], " AS number"),
            "c.length AS raw_length",
            "c.weight AS raw_weight",
            NULL),
        params = params)
    out
}

# Return year, step, area, ... , number (of samples), mean weight -- scaled by tow_length
mfdb_sample_scaled <- function (mdb, cols, params, abundance_scale = NULL, scale = 'tow_length') {
    if (scale == 'tow_length') {
        scale_fn <- "1/SUM(t_scale.length)"
        scale_tables <- c("JOIN tow t_scale ON c.tow_id = t_scale.tow_id")
    } else {
        stop("Don't know how to scale by column ", scale)
    }
    abundance <- abundance_core_table(mdb, abundance_scale)
    mfdb_sample_grouping(mdb,
        core_table = abundance[[1]],
        join_tables = scale_tables,
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            paste0("SUM(", abundance[[2]], ") * ", scale_fn, " AS number"),
            paste0("SUM(CAST(c.length AS numeric) * CAST(", abundance[[2]], " AS numeric)) * ", scale_fn, " AS mean_weight__sum"),
            paste0("SUM(CAST(", abundance[[2]], " AS numeric)) AS mean_weight__count"),
            NULL),
        params = params)
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
            paste0("SUM(COALESCE(prey.count, 0)) AS number"),
            paste0("SUM(CAST(prey.length AS numeric) * CAST(COALESCE(prey.count, 0) AS numeric)) AS mean_length__sum"),
            paste0("SUM(CAST(COALESCE(prey.count, 0) AS numeric)) AS mean_length__count"),
            NULL),
        params = params)
}

# Returns year, step, area, (cols), number, mean_weight (of prey found in stomach)
# stomach count weight
#   A      01     49 \
#   A      04     24  -- Sum
#   A      08     30 /
#   B      NA     34  ==> Total
#   C      NA     74  ==> Total
# ==> ( 49*1 + 24*4 + 30*8 + 34 + 74 ) / 3 ( unique stomachs )
mfdb_stomach_preymeanweight <- function (mdb, cols, params) {
    # Group without prey options first
    without_prey <- mfdb_sample_grouping(mdb,
        core_table = "predator",
        join_tables = c(
            NULL),
        col_defs = as.list(c(pred_col_defs)),
        group_cols = c("year", "timestep", "area", intersect(cols, names(pred_col_defs))),
        calc_cols = c(
            paste0("COUNT(DISTINCT c.predator_id) AS predator_count"),
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
            paste0("SUM(CAST(prey.weight AS numeric) * COALESCE(prey.count, 1)) AS weight_total"),
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
        merged$mean_weight <- merged$weight_total / as.numeric(merged$predator_count)
        do.call(structure, c(
            list(merged[, c("year", "step", "area", cols, "predator_count", "mean_weight"), drop = FALSE]),
            attributes(w)[c("year", "step", "area", cols, "generator")],
            NULL))
    }, with_prey, without_prey, SIMPLIFY = FALSE)
}

# percentage of stomach weight is prey
mfdb_stomach_preyweightratio <- function (mdb, cols, params) {
    # Group without prey options first
    without_prey <- mfdb_sample_grouping(mdb,
        core_table = "predator",
        join_tables = c(
            paste0("INNER JOIN prey ON c.predator_id = prey.predator_id"),
            NULL),
        col_defs = as.list(c(pred_col_defs)),
        group_cols = c("year", "timestep", "area", intersect(cols, names(pred_col_defs))),
        calc_cols = c(
            "SUM(CAST(prey.weight AS numeric) * COALESCE(prey.count, 1)) AS weight_total",
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
            "SUM(CAST(prey.weight AS numeric) * COALESCE(prey.count, 1)) AS weight_present",
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
        merged$ratio <- merged$weight_present / merged$weight_total
        do.call(structure, c(
            list(merged[, c("year", "step", "area", cols, "ratio"), drop = FALSE]),
            attributes(w)[c("year", "step", "area", cols, "generator")],
            NULL))
    }, with_prey, without_prey, SIMPLIFY = FALSE)
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
        col_defs = c(list(step = "c.month", area = "c.areacell_id"), col_defs_for('sample')),
        calc_cols = c(),
        core_table = "sample",
        # join_tables: JOIN statements to attach to the main table
        join_tables = c(),
        disable_group_by = FALSE,
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
                col,
                group_disabled = disable_group_by)))
    }

    # Importing is probably done, so create indexes if we need to
    mfdb_finish_import(mdb)

    # All group_cols should be valid
    for (col in group_cols) {
        if (!(col %in% names(col_defs))) {
            stop("Unknown column ", col, "\nAvailable columns: ", paste(names(col_defs), collapse = ", "))
        }
    }

    # If we need any joins for selected columns, add them
    extra_joins <- unique(unlist(lapply(
        col_defs[c(names(params), group_cols)],
        function(x) attr(x, "join_sql"))))
    if (!is.null(extra_joins)) join_tables <- c(extra_joins, join_tables)

    # Pick out any extra params we can make use of, ignore rest
    filter_cols <- intersect(names(params), names(col_defs))

    # Call pre-query for all groups
    for (col in union(group_cols, filter_cols)) {
        x <- pre_query(mdb, params[[col]], col_defs[[col]])
        # Replace group if a new one was given
        if (!is.null(x)) params[[col]] <- x
    }

    out <- mfdb_fetch(mdb,
        "SELECT ", paste(c(
            paste(paste(
                clauses(group_cols, sample_clause),
                NULL, collapse = "|| '.' ||"), "AS bssample"),
            gsub("AS ([a-zA-Z0-9_]+)$", "AS grp_\\1", clauses(group_cols, select_clause)),
            calc_cols,
            NULL), collapse = "\n     , "),
        "\n FROM ", paste(c(
            paste(c(core_table, "c", join_tables), collapse = " "),
            clauses(union(group_cols, filter_cols), from_clause),
            NULL), collapse = ","),
        "\n WHERE ", paste(c(
            "TRUE",
            clauses(union(group_cols, filter_cols), where_clause),
            NULL), collapse = "\n   AND "),
        if (disable_group_by) c() else c(
            "\n GROUP BY ", paste(c("bssample", paste0("grp_", group_cols)), collapse=",")),
        "\n ORDER BY ", paste(c(
            "bssample",
            if (length(group_cols) > 0) paste0("grp_", group_cols) else c(),
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

    calc_col_names <- gsub(".*\\s+AS\\s+(\\w+)", "\\1", calc_cols)
    if (any(grepl('__sum$', names(out)))) {
        # Finalise the weighted mean / stddev
        colname_sum <- names(out)[grepl('__sum$', names(out))]
        colname_count <- names(out)[grepl('__count$', names(out))]
        colname_sqsum <- names(out)[grepl('__sqsum$', names(out))]
        colname_base <- gsub('__sum$', '', colname_sum)

        calc_col_names <- c(calc_col_names, colname_base)
        out[colname_base] <- out[,colname_sum] / out[,colname_count]
        if (length(colname_sqsum) > 0) {
            # Generate stddev col if sqsum is also available
            colname_base <- gsub('mean', 'stddev', colname_base)
            calc_col_names <- c(calc_col_names, colname_base)
            out[colname_base] <- ifelse(
                out[,colname_count] < 2,
                NA,
                sqrt((out[,colname_sqsum] - out[,colname_sum]**2 / out[,colname_count]) / (out[,colname_count] - 1)))
        }

        calc_col_names <- calc_col_names[!grepl('__', calc_col_names)]
        out <- out[!grepl('__', names(out))]
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
                calc_col_names,
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
