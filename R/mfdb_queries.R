# Return area, size
mfdb_area_size <- function (mdb, params) {
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

# Return year,step,area,stock,age,length, number (of samples)
# TODO: stock isn't a group_cols yet
mfdb_stock_count <- function (mdb, params) {
    mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("timestep", "stock", "area", "age", "length"),
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
        if(is.null(params[[str]])) stop("params must contain value for ", str)
        return(if_true)
    }
    # If filtering, then do it if possible
    filtering_by <- function(str, if_true = TRUE, if_false = NULL) {
        if (str %in% filter_cols && !is.null(params[[str]])) if_true else if_false
    }

    # Importing is probably done, so create indexes if we need to
    mfdb_finish_import(mdb)

    x <- grouping_by("timestep", group_to_table(mdb$db, "temp_ts", params$timestep, datatype = "INT", save_temp_tables = mdb$save_temp_tables))
    x <- grouping_by("area", group_to_table(mdb$db, "temp_area", params$area, datatype = "VARCHAR(10)", save_temp_tables = mdb$save_temp_tables))
    x <- grouping_by("age", group_to_table(mdb$db, "temp_age", params$age, datatype = "INT", save_temp_tables = mdb$save_temp_tables))

    out <- mfdb_fetch(mdb,
        "SELECT ", paste(c(
            paste(paste0(c(
                grouping_by("timestep", "tts.sample"),
                grouping_by("area",    "tarea.sample"),
                grouping_by("age",     "tage.sample"),
                NULL), collapse = "|| '.' ||"), "AS sample"),
            grouping_by("year",     select_clause(params$year, "c.year", "year")),
            grouping_by("timestep", "tts.name AS step"),
            grouping_by("area",    "tarea.name AS area"),
            grouping_by("age",     "tage.name AS age"),
            grouping_by("length",  select_clause(params$length, "c.length", "length")),
            calc_cols,
            NULL), collapse = ","),
        " FROM ", paste(c(
            paste(core_table, "c"),
            grouping_by("timestep", "temp_ts tts"),
            grouping_by("area",    "temp_area tarea, division div"),
            grouping_by("age",     "temp_age tage"),
            NULL), collapse = ","),
        " WHERE ", paste(c(
            paste("c.case_study_id =", sql_quote(mdb$case_study_id)),
            grouping_by("year",     where_clause(params$year, "c.year")),
            grouping_by("timestep", "c.month = tts.value"),
            grouping_by("area",    "c.case_study_id = div.case_study_id AND c.areacell_id = div.areacell_id AND div.division = tarea.value"),
            grouping_by("age",     "c.age = tage.value"),
            filtering_by("length", where_clause(params$length, "c.length")),
            filtering_by("institute", where_clause(params$institute, "c.institute_id")),
            filtering_by("gear", where_clause(params$gear, "c.gear_id")),
            filtering_by("vessel", where_clause(params$vessel, "c.vessel_id")),
            filtering_by("sampling_type", where_clause(params$sampling_type, "c.sampling_type_id")),
            filtering_by("species", where_clause(params$species, "c.species_id")),
            filtering_by("sex", where_clause(params$sex, "c.sex_id")),
            # TODO: where_clause(params$marketcategory, "c.marketcategory")),
            # TODO: where_clause(params$samplingstrategy, "c.samplingstrategy")),
            # TODO: where_clause(params$maturitystage, "c.maturitystage")),
            NULL), collapse = " AND "),
        " GROUP BY ", paste(1:(1 + length(group_cols)), collapse=","),
        " ORDER BY ", paste(1:(1 + length(group_cols)), collapse=","),
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
