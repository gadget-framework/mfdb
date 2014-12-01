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
    #TODO: check cols has known entries
    mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("year", "timestep", "area", cols),
        calc_cols = c(
            ", SUM(c.count) AS number"),
        generator = "mfdb_stock_count")
}

# Return year,step,area,age,number (# of samples),mean (length)
mfdb_meanlength <- function (mdb, params) {
    out <- mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("year", "timestep", "area", "age"),
        calc_cols = c(
            "SUM(c.count) AS number",
            "AVG(c.count * c.length) * (COUNT(*)::float / SUM(c.count)) AS mean"),
        generator = "mfdb_meanlength")
    out
}

# Return year,step,area,age,number (# of samples),mean (length), stddev (length)
mfdb_meanlength_stddev <- function (mdb, params) {
    # SCHEMA: Need a weighted stddev function
    # TODO: Do we need to know the resolution of the input data to avoid oversampling?
    out <- mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("year", "timestep", "area", "age"),
        calc_cols = c(
            "SUM(c.count) AS number",
            "AVG(c.count * c.length) * (COUNT(*)::float / SUM(c.count)) AS mean",
            "0 AS stddev"),
        generator = "mfdb_meanlength_stddev")
    out
}

# Return year,step,area,age,number (# of samples),mean (weight)
mfdb_meanweight <- function (mdb, params) {
    out <- mfdb_sample_grouping(mdb,
        params = params,
        calc_cols = c(
            "SUM(c.count) AS number",
            "AVG(c.count * c.weight) * (COUNT(*)::float / SUM(c.count)) AS mean"),
        generator = "mfdb_meanweight")
    out
}

# Return year,step,area,age,number (# of samples),mean (weight), stddev (weight)
mfdb_meanweight_stddev <- function (mdb, params) {
    # SCHEMA: Don't have weight_stddev, aggregation function
    out <- mfdb_sample_grouping(mdb,
        params = params,
        calc_cols = c(
            "SUM(c.count) AS number",
            "AVG(c.count * c.weight) * (COUNT(*)::float / SUM(c.count)) AS mean",
            "0 AS stddev"),
        generator = "mfdb_meanweight_stddev")
    out
}

# Return year,step,area,age,length,number (# of samples)
mfdb_agelength <- function (mdb, params) {
    out <- mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("year", "timestep", "area", "age", "length"),
        calc_cols = c(
            "SUM(c.count) AS number"),
        generator = "mfdb_agelength")
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

    x <- grouping_by("timestep",       pre_query(mdb, params$timestep, "step"))
    x <- grouping_by("area",           pre_query(mdb, params$area, "area"))
    x <- grouping_by("age",            pre_query(mdb, params$age, "age"))
    x <- grouping_by("maturity_stage", pre_query(mdb, params$maturity_stage, "maturity_stage"))

    out <- mfdb_fetch(mdb,
        "SELECT ", paste(c(
            paste(paste0(c(
                grouping_by("timestep", sample_clause(mdb, params$timestep, "c.month", "step")),
                grouping_by("area",     sample_clause(mdb, params$area, "c.areacell_id", "area")),
                grouping_by("age",     sample_clause(mdb, params$age, "c.age", "age")),
                grouping_by("maturity_stage", sample_clause(mdb, params$maturity_stage, "c.maturity_stage", "maturity_stage")),
                NULL), collapse = "|| '.' ||"), "AS sample"),
            grouping_by("year",     select_clause(mdb, params$year, "c.year", "year")),
            grouping_by("timestep", select_clause(mdb, params$timestep, "c.month", "step")),
            grouping_by("area",     select_clause(mdb, params$area, "c.areacell_id", "area")),
            grouping_by("age",      select_clause(mdb, params$age, "c.age", "age")),
            grouping_by("maturity_stage", select_clause(mdb, params$maturity_stage, "c.maturity_stage", "maturity_stage")),
            grouping_by("length",  select_clause(mdb, params$length, "c.length", "length")),
            calc_cols,
            NULL), collapse = ","),
        " FROM ", paste(c(
            paste(core_table, "c"),
            grouping_by("timestep", from_clause(mdb, params$timestep, "c.month", "step")),
            grouping_by("area",     from_clause(mdb, params$area, "c.areacell_id", "area")),
            grouping_by("age",      from_clause(mdb, params$age, "c.age", "age")),
            grouping_by("maturity_stage", from_clause(mdb, params$maturity_stage, "c.maturity_stage", "maturity_stage")),
            NULL), collapse = ","),
        " WHERE ", paste(c(
            paste("c.case_study_id =", sql_quote(mdb$case_study_id)),
            grouping_by("year",     where_clause(mdb, params$year, "c.year", "year")),
            grouping_by("timestep", where_clause(mdb, params$timestep, "c.month", "step")),
            grouping_by("area",     where_clause(mdb, params$area, "c.areacell_id", "area")),
            grouping_by("age",     where_clause(mdb, params$age, "c.age", "age")),
            grouping_by("maturity_stage", where_clause(mdb, params$maturity_stage, "c.maturity_stage", "maturity_stage")),
            filtering_by("length", where_clause(mdb, params$length, "c.length")),
            filtering_by("institute", where_clause(mdb, params$institute, "c.institute_id")),
            filtering_by("gear", where_clause(mdb, params$gear, "c.gear_id")),
            filtering_by("vessel", where_clause(mdb, params$vessel, "c.vessel_id")),
            filtering_by("sampling_type", where_clause(mdb, params$sampling_type, "c.sampling_type_id")),
            filtering_by("species", where_clause(mdb, params$species, "c.species_id")),
            filtering_by("sex", where_clause(mdb, params$sex, "c.sex_id")),
            NULL), collapse = " AND "),
        " GROUP BY ", paste(1:(1 + sum(unlist(Vectorize(grouping_by, 'str')(group_cols, 1)))), collapse=","),
        " ORDER BY ", paste(1:(1 + sum(unlist(Vectorize(grouping_by, 'str')(group_cols, 1)))), collapse=","),
        "")

    # Break data up by sample and annotate each
    samples <- unique(out$sample)
    structure(lapply(samples, function (sample) {
        do.call(structure, c(
            list(out[out$sample == sample,names(out) != 'sample']),
            grouping_by("timestep", list(timestep = params$timestep)),
            grouping_by("area",    list(area = params$area)),
            grouping_by("age",     list(age = params$age)),
            grouping_by("length",  list(length = params$length)),
            list(generator = generator)))
    }), names = samples)
}
