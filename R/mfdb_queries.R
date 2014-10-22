# Return area, size
mfdb_area_size <- function (mdb, params) {
    mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("areas"),
        filter_cols = c(),
        calc_cols = c("SUM(c.size) size"),
        core_table = "areacell",
        generator = "mfdb_area_size")
}

# Return year, step, area, temperature (mean)
mfdb_temperature <- function (mdb, params = list()) {
    mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("year", "timestep", "areas"),
        filter_cols = c(),
        calc_cols = c("AVG(c.temperature) temperature"),
        core_table = "temperature",
        generator = "mfdb_temperature")
}

# Return year,step,area,stock,age,length, number (of samples)
mfdb_stock_count <- function (mdb, params) {
    mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("timestep", "stocks", "areas", "ages", "lengths"),
        calc_cols = c(
            ", SUM(sam.count) AS number"),
        generator = "mfdb_stock_count")
}

# Return year,step,area,age,number (# of samples),mean (length), stddev (length)
mfdb_meanlength_stddev <- function (mdb, params) {
    # SCHEMA: Don't have length_stddev, need a weighted stddev function
    # TODO: Do we need to know the resolution of the input data to avoid oversampling?
    out <- mfdb_sample_grouping(mdb,
        params = params,
        calc_cols = c(
            ", SUM(sam.count) AS number",
            ", AVG(sam.count * sam.length) * (COUNT(*)::float / SUM(sam.count)) AS mean",
            ", 0 AS stddev"),
        generator = "mfdb_meanlength_stddev")
    out
}

# Return year,step,area,age,number (# of samples),mean (length)
mfdb_meanlength <- function (mdb, params) {
    out <- mfdb_sample_grouping(mdb,
        params = params,
        calc_cols = c(
            ", SUM(sam.count) AS number",
            ", AVG(sam.count * sam.length) * (COUNT(*)::float / SUM(sam.count)) AS mean"),
        generator = "mfdb_meanlength")
    out
}

# Return year,step,area,age,number (# of samples),mean (weight)
mfdb_meanweight <- function (mdb, params) {
    out <- mfdb_sample_grouping(mdb,
        params = params,
        calc_cols = c(
            ", SUM(sam.count) AS number",
            ", AVG(sam.count * sam.weight) * (COUNT(*)::float / SUM(sam.count)) AS mean"),
        generator = "mfdb_meanweight")
    out
}

# Return year,step,area,age,number (# of samples),mean (weight), stddev (weight)
mfdb_meanweight_stddev <- function (mdb, params) {
    # SCHEMA: Don't have weight_stddev, aggregation function
    out <- mfdb_sample_grouping(mdb,
        params = params,
        calc_cols = c(
            ", SUM(sam.count) AS number",
            ", AVG(sam.count * sam.weight) * (COUNT(*)::float / SUM(sam.count)) AS mean",
            ", 0 AS stddev"),
        generator = "mfdb_meanweight_stddev")
    out
}

# Return year,step,area,age,length,number (# of samples)
mfdb_agelength <- function (mdb, params) {
    out <- mfdb_sample_grouping(mdb,
        params = params,
        group_cols = c("timestep", "areas", "ages", "lengths"),
        calc_cols = c(
            ", SUM(sam.count) AS number"),
        generator = "mfdb_meanweight_stddev")
    out
}

# Group up sample data by area, age or length
mfdb_sample_grouping <- function (mdb,
        params = list(),
        group_cols = c("year", "timestep", "areas", "ages"),
        filter_cols = c(),
        calc_cols = c(),
        core_table = "(SELECT * FROM survey sur, sample sam WHERE sur.survey_id = sam.sample_id)",
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

    x <- grouping_by("timestep", group_to_table(mdb$db, "temp_ts", params$timestep, datatype = "INT", save_temp_tables = mdb$save_temp_tables))
    x <- grouping_by("areas", group_to_table(mdb$db, "temp_area", params$areas, datatype = "VARCHAR(10)", save_temp_tables = mdb$save_temp_tables))
    x <- grouping_by("ages", group_to_table(mdb$db, "temp_age", params$ages, datatype = "INT", save_temp_tables = mdb$save_temp_tables))

    out <- mfdb_fetch(mdb,
        "SELECT ", paste(c(
            paste(paste0(c(
                grouping_by("timestep", "tts.sample"),
                grouping_by("areas",    "tarea.sample"),
                grouping_by("ages",     "tage.sample"),
                NULL), collapse = "|| '.' ||"), "AS sample"),
            grouping_by("year",     "c.year AS year"),
            grouping_by("timestep", "tts.name AS step"),
            grouping_by("areas",    "tarea.name AS area"),
            grouping_by("ages",     "tage.name AS age"),
            grouping_by("lengths",  select_clause(params$lengths, "c.length", "length")),
            calc_cols,
            NULL), collapse = ","),
        " FROM ", paste(c(
            paste(core_table, "c"),
            grouping_by("timestep", "temp_ts tts"),
            grouping_by("areas",    "temp_area tarea, division div"),
            grouping_by("ages",     "temp_age tage"),
            NULL), collapse = ","),
        " WHERE ", paste(c(
            paste("c.case_study_id =", sql_quote(mdb$case_study_id)),
            grouping_by("year",     paste("c.year IN", sql_quote(params$year, always_bracket = TRUE))),
            grouping_by("timestep", "c.month = tts.value"),
            grouping_by("areas",    "c.case_study_id = div.case_study_id AND c.areacell_id = div.areacell_id AND div.division = tarea.value"),
            grouping_by("ages",     "c.age = tage.value"),
            filtering_by("lengths", where_clause(params$lengths, "c.length")),
            filtering_by("institute", sql_col_condition("c.institute_id", params$institute, lookup="institute")),
            filtering_by("gear", sql_col_condition("c.gear_id", params$gear, lookup="gear")),
            filtering_by("vessel", sql_col_condition("c.vessel_id", params$vessel, lookup="vessel")),
            filtering_by("sampling_type", sql_col_condition("c.sampling_type_id", params$sampling_type, lookup="sampling_type")),
            filtering_by("species", sql_col_condition("c.species_id", params$species, lookup="species")),
            filtering_by("sex", sql_col_condition("c.sex_id", params$sex, lookup="sex")),
            # TODO: sql_col_condition("c.stock", params$stocks, lookup="stock"),
            # TODO: sql_col_condition("c.marketcategory", params$marketcategory, lookup="marketcategory"),
            # TODO: sql_col_condition("c.samplingstrategy", params$samplingstrategy, lookup="samplingstrategy"),
            # TODO: sql_col_condition("c.maturitystage", params$maturitystage, lookup="maturitystage"),
            NULL), collapse = " AND "),
        " GROUP BY ", paste(1:(1 + length(group_cols)), collapse=","),
        " ORDER BY ", paste(1:(1 + length(group_cols)), collapse=","),
        "")

    # Break data up by sample and annotate each
    samples <- unique(out$sample)
    structure(lapply(samples, function (sample) {
        do.call(structure, c(
            list(out[out$sample == sample,names(out) != 'sample'], generator = generator),
            grouping_by("timestep", list(timestep = params$timestep)),
            grouping_by("areas",    list(areas = params$areas)),
            grouping_by("ages",     list(ages = params$areas)),
            grouping_by("lengths",  list(lengths = params$lengths)),
            NULL))
    }), names = samples)
}
