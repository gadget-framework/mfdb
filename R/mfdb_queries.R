mfdb_area_sizes <- function (mdb, params = list()) {
    #TODO:
}

mfdb_temperatures <- function (mdb, params = list()) {
    #TODO:
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
        group_cols = c("timestep", "areas", "ages"),
        calc_cols = c(),
        generator = "mfdb_sample_grouping") {

    # True iff str is in the group_cols parameter
    grouping_by <- function(str) {
        str %in% group_cols
    }

    params <- c(params, mdb$defaultparams)
    mdb$logger$info(params)
    # Store groups into temporary tables for joining
    if (grouping_by("timestep")) group_to_table(mdb$db, "temp_ts", params$timestep, datatype = "INT", save_tables = mdb$save_tables)
    if (grouping_by("areas")) group_to_table(mdb$db, "temp_area", params$areas, datatype = "VARCHAR(10)", save_tables = mdb$save_tables)
    if (grouping_by("ages"))  group_to_table(mdb$db, "temp_age", params$ages, datatype = "INT", save_tables = mdb$save_tables)

    query <- paste(c(
        "SELECT 's'",
        if (grouping_by("timestep")) "|| '.' || tts.sample",
        if (grouping_by("areas"))    "|| '.' || tarea.sample",
        if (grouping_by("ages"))     "|| '.' || tage.sample",
        " AS sample",
        ", sam.year",
        if (grouping_by("timestep")) ", tts.name AS step",
        # TODO: if (grouping_by("stocks"))   ", spe.stock AS stock",
        if (grouping_by("areas"))    ", tarea.name AS area",
        if (grouping_by("ages"))     ", tage.name AS age",
        if (grouping_by("lengths"))  select_clause(params$lengths, "sam.length", "length"),
        calc_cols,
        "FROM survey sur, sample sam",
        if (grouping_by("timestep")) ", temp_ts tts",
        if (grouping_by("areas"))    ", temp_area tarea, area a",
        if (grouping_by("ages"))     ", temp_age tage",
        "WHERE sur.survey_id = sam.survey_id",
        if (grouping_by("timestep")) "AND sam.month = tts.value",
        if (grouping_by("areas"))    "AND sam.areacell = a.areacell AND a.division = tarea.value",
        if (grouping_by("ages"))     "AND sam.age = tage.value",
        where_clause(params$lengths, "sam.length"),
        sql_col_condition("sur.institute", params$institute, lookup="institute"),
        sql_col_condition("sur.gear", params$gear, lookup="gear"),
        sql_col_condition("sur.vessel", params$vessel, lookup="vessel"),
        sql_col_condition("sur.samplingtype", params$samplingtype, lookup="samplingtype"),
        sql_col_condition("sam.year", params$year),
        sql_col_condition("sam.species", params$species, lookup="species"),
        sql_col_condition("sam.sex", params$sex, lookup="sex"),
        # TODO: sql_col_condition("spe.stock", params$stocks, lookup="l_stock"),
        # TODO: sql_col_condition("spe.marketcategory", params$marketcategory, lookup="l_marketcategory"),
        # TODO: sql_col_condition("cas.samplingstrategy", params$samplingstrategy, lookup="l_samplingstrategy"),
        # TODO: sql_col_condition("lec.maturitystage", params$maturitystage, lookup="l_maturitystage"),
        "GROUP BY ", paste(1:(2 + length(group_cols)), collapse=","),
        "ORDER BY ", paste(1:(2 + length(group_cols)), collapse=","),
        ""), collapse = " ")
    mdb$logger$debug(query)

    # Fetch all data, break it up by sample and annotate each
    out <- dbFetch(dbSendQuery(mdb$db, query), -1)
    samples <- unique(out$sample)
    structure(lapply(samples, function (sample) {
        structure(
            out[out$sample == sample,names(out) != 'sample'],
            generator = generator,
            areas = params$areas,
            ages = params$ages,
            lengths = params$lengths,
            timestep = params$timestep)
    }), names = samples)
}
