# Return area, size
mfdb_area_size <- function (mdb, params) {
    if (is.null(params$area)) stop("Must specify 'area' in params")
    mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("area"),
        filter_cols = c(),
        calc_cols = c("SUM(c.size) size"),
        core_table = "areacell",
        col_defs = list(area = "c.areacell_id"),
        generator = "mfdb_area_size")
}

# Return year, step, area, temperature (mean)
mfdb_temperature <- function (mdb, params = list()) {
    mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("year", "timestep", "area"),
        filter_cols = c(),
        calc_cols = c("AVG(c.temperature) temperature"),
        col_defs = list(year = "c.year", timestep = "c.month", area = "c.areacell_id"),
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
        col_defs = list(
            year = "c.year", timestep = "c.month", area = "c.areacell_id", age = "c.age",
            maturity_stage = "c.maturity_stage_id", length = "c.length",
            institute = "c.institute_id", gear = "c.gear_id", vessel = "c.vessel_id",
            sampling_type = "c.sampling_type_id", species ="c.species_id", sex = "c.sex_id"),
        calc_cols = c(),
        core_table = "(SELECT sur.institute_id, sur.gear_id, sur.vessel_id, sur.sampling_type_id, sam.* FROM sample sam INNER JOIN survey sur ON sam.survey_id = sur.survey_id)",
        generator = "mfdb_sample_grouping") {

    # Call relevant clause function for all group_cols
    clauses <- function (cols, fn) {
        unlist(lapply(cols, function (col) fn(
                mdb,
                params[[col]],
                col_defs[[col]],
                ifelse(col == 'timestep', 'step', col))))
    }

    # Importing is probably done, so create indexes if we need to
    mfdb_finish_import(mdb)

    # Do we know what these map to in SQL?
    for (col in c(group_cols, filter_cols)) {
        if (is.null(col_defs[[col]])) {
            stop("Unknown column ", col)
        }
    }

    # Call pre-query for all groups
    for (col in union(group_cols, filter_cols)) {
        x <- pre_query(mdb, params[[col]], ifelse(col == 'timestep', 'step', col))
    }

    out <- mfdb_fetch(mdb,
        "SELECT ", paste(c(
            paste(paste(
                clauses(group_cols, sample_clause),
                NULL, collapse = "|| '.' ||"), "AS sample"),
            clauses(group_cols, select_clause),
            calc_cols,
            NULL), collapse = ","),
        " FROM ", paste(c(
            paste(core_table, "c"),
            clauses(union(group_cols, filter_cols), from_clause),
            NULL), collapse = ","),
        " WHERE ", paste(c(
            paste("c.case_study_id =", sql_quote(mdb$case_study_id)),
            clauses(union(group_cols, filter_cols), where_clause),
            NULL), collapse = " AND "),
        " GROUP BY ", paste(seq_len(length(group_cols) + 1), collapse=","),
        " ORDER BY ", paste(seq_len(length(group_cols) + 1), collapse=","),
        NULL)

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
