# Return area, size
mfdb_area_size <- function (mdb, params) {
    if (is.null(params$area)) stop("Must specify 'area' in params")
    mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("area"),
        filter_cols = c(),
        calc_cols = c("SUM(c.size) size"),
        core_table = "areacell",
        generator = "mfdb_area_size")
}

# Return year, step, area, temperature (mean)
mfdb_temperature <- function (mdb, params = list()) {
    mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("year", "timestep", "area"),
        filter_cols = c(),
        calc_cols = c("AVG(c.temperature) temperature"),
        core_table = "temperature",
        generator = "mfdb_temperature")
}

# Return year, step, area, ... , number (of samples)
mfdb_sample_count <- function (mdb, cols, params) {
    mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            "SUM(c.count) AS number"),
        generator = "mfdb_sample_count")
}

# Return year,step,area,age,number (# of samples),mean (length)
mfdb_sample_meanlength <- function (mdb, cols, params) {
    out <- mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            "SUM(c.count) AS number",
            "AVG(c.count * c.length) * (COUNT(*)::float / SUM(c.count)) AS mean"),
        generator = "mfdb_sample_meanlength")
    out
}

# Return year,step,area,age,number (# of samples),mean (length), stddev (length)
mfdb_sample_meanlength_stddev <- function (mdb, cols, params) {
    # SCHEMA: Need a weighted stddev function
    # TODO: Do we need to know the resolution of the input data to avoid oversampling?
    out <- mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            "SUM(c.count) AS number",
            "AVG(c.count * c.length) * (COUNT(*)::float / SUM(c.count)) AS mean",
            "0 AS stddev"),
        generator = "mfdb_sample_meanlength_stddev")
    out
}

# Return year,step,area,age,number (# of samples),mean (weight)
mfdb_sample_meanweight <- function (mdb, cols, params) {
    out <- mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            "SUM(c.count) AS number",
            "AVG(c.count * c.weight) * (COUNT(*)::float / SUM(c.count)) AS mean"),
        generator = "mfdb_sample_meanweight")
    out
}

# Return year,step,area,age,number (# of samples),mean (weight), stddev (weight)
mfdb_sample_meanweight_stddev <- function (mdb, cols, params) {
    # SCHEMA: Don't have weight_stddev, aggregation function
    out <- mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            "SUM(c.count) AS number",
            "AVG(c.count * c.weight) * (COUNT(*)::float / SUM(c.count)) AS mean",
            "0 AS stddev"),
        generator = "mfdb_sample_meanweight_stddev")
    out
}

# Group up sample data by area, age or length
mfdb_sample_grouping <- function (mdb,
        params = list(),
        group_cols = c("year", "timestep", "area", "age"),
        filter_cols = c("length", "institute", "gear", "vessel", "sampling_type", "species", "sex"),
        calc_cols = c(),
        core_table = "(SELECT sur.institute_id, sur.gear_id, sur.vessel_id, sur.sampling_type_id, sam.* FROM sample sam INNER JOIN survey sur ON sam.survey_id = sur.survey_id)",
        generator = "mfdb_sample_grouping") {

    # If grouping by, the a setting *must* be in params
    grouping_by <- function(str, if_true = TRUE, if_false = NULL) {
        if(!(str %in% group_cols)) return(if_false)
        return(if_true)
    }
    # If filtering, then do it if possible
    filtering_by <- function(str, if_true = TRUE, if_false = NULL) {
        if (str %in% filter_cols && !is.null(params[[str]])) if_true else if_false
    }

    # Importing is probably done, so create indexes if we need to
    mfdb_finish_import(mdb)

    # Group names to the columns that define them
    col_defs <- list(
        year = "c.year",
        timestep = "c.month",
        area = "c.areacell_id",
        age = "c.age",
        maturity_stage = "c.maturity_stage_id",
        length = "c.length")

    for (col in group_cols) {
        if (is.null(col_defs[[col]])) {
            stop("Unknown column ", col)
        }
        x <- pre_query(mdb, params[[col]], ifelse(col == 'timestep', 'step', col))
    }

    # Call relevant clause function for all group_cols
    group_clauses <- function (fn) {
        unlist(lapply(group_cols, function (col) fn(
                mdb,
                params[[col]],
                col_defs[[col]],
                ifelse(col == 'timestep', 'step', col))))
    }

    out <- mfdb_fetch(mdb,
        "SELECT ", paste(c(
            paste(paste(
                group_clauses(sample_clause),
                NULL, collapse = "|| '.' ||"), "AS sample"),
            group_clauses(select_clause),
            calc_cols,
            NULL), collapse = ","),
        " FROM ", paste(c(
            paste(core_table, "c"),
            group_clauses(from_clause),
            NULL), collapse = ","),
        " WHERE ", paste(c(
            paste("c.case_study_id =", sql_quote(mdb$case_study_id)),
            group_clauses(where_clause),
            filtering_by("length", where_clause(mdb, params$length, "c.length")),
            filtering_by("institute", where_clause(mdb, params$institute, "c.institute_id")),
            filtering_by("gear", where_clause(mdb, params$gear, "c.gear_id")),
            filtering_by("vessel", where_clause(mdb, params$vessel, "c.vessel_id")),
            filtering_by("sampling_type", where_clause(mdb, params$sampling_type, "c.sampling_type_id")),
            filtering_by("species", where_clause(mdb, params$species, "c.species_id")),
            filtering_by("sex", where_clause(mdb, params$sex, "c.sex_id")),
            NULL), collapse = " AND "),
        " GROUP BY ", paste(seq_len(length(group_cols) + 1), collapse=","),
        " ORDER BY ", paste(seq_len(length(group_cols) + 1), collapse=","),
        "")

    # Break data up by sample and annotate each
    samples <- unique(out$sample)
    structure(lapply(samples, function (sample) {
        do.call(structure, c(
            list(out[out$sample == sample,names(out) != 'sample']),
            structure(
                lapply(group_cols, function(col) params[[col]]),
                names = group_cols),
            list(generator = generator)))
    }), names = samples)
}
