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
            ", SUM(age.agenum) AS number"),
        generator = "mfdb_meanlength_stddev")
}

# Return year,step,area,age,number (# of samples),mean (length), stddev (length)
mfdb_meanlength_stddev <- function (mdb, params) {
    # TODO: Really need to define a weighted stddev aggregate function
    # SCHEMA: What we want is lengthmean, lengthstddev when grouping by area,age.
    # SCHEMA: This isn't the same as length-as-a-group, unless you have multiple rows for each dimension or unaggregated data
    # TODO: Need to use the middle of the length group
    out <- mfdb_sample_grouping(mdb,
        params = params,
        calc_cols = c(
            ", SUM(age.agenum) AS number",
            ", AVG(age.agenum * lec.lengthcell) * (COUNT(*)::float / SUM(age.agenum)) AS mean",
            ", 0 AS stddev"),
        generator = "mfdb_meanlength_stddev")
    out
}

# Return year,step,area,age,number (# of samples),mean (length)
mfdb_meanlength <- function (mdb, params) {
    # TODO: Need to use the middle of the length group
    out <- mfdb_sample_grouping(mdb,
        params = params,
        calc_cols = c(
            ", SUM(age.agenum) AS number",
            ", AVG(age.agenum * lec.lengthcell) * (COUNT(*)::float / SUM(age.agenum)) AS mean"),
        generator = "mfdb_meanlength")
    out
}

# Return year,step,area,age,number (# of samples),mean (weight)
mfdb_meanweight <- function (mdb, params) {
    # TODO: Really need to define a weighted stddev aggregate function
    out <- mfdb_sample_grouping(mdb,
        params = params,
        calc_cols = c(
            ", SUM(age.agenum) AS number",
            ", SUM(age.agenum * age.weightagemean) / SUM(age.agenum) AS mean"),
        generator = "mfdb_meanweight")
    out
}

# Return year,step,area,age,number (# of samples),mean (weight), stddev (weight)
mfdb_meanweight_stddev <- function (mdb, params) {
    # SCHEMA: Don't have weightagestddev
    out <- mfdb_sample_grouping(mdb,
        params = params,
        calc_cols = c(
            ", SUM(age.agenum) AS number",
            ", SUM(age.agenum * age.weightagemean) / SUM(age.agenum) AS mean",
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
            ", SUM(age.agenum) AS number"),
        generator = "mfdb_meanweight_stddev")
    out
}

# SCHEMA: CREATE INDEX age_lengthcellid ON age (lengthcellid);

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
    if (grouping_by("areas")) group_to_table(mdb$db, "temp_area", params$areas, datatype = "INT", save_tables = mdb$save_tables)
    if (grouping_by("ages"))  group_to_table(mdb$db, "temp_age", params$ages, datatype = "INT", save_tables = mdb$save_tables)

    # TODO: Add in indrection, so a gridcell can be in multiple subdivisions
    query <- paste(c(
        "SELECT 's'",
        if (grouping_by("timestep")) "|| '.' || tts.sample",
        if (grouping_by("areas"))    "|| '.' || tarea.sample",
        if (grouping_by("ages"))     "|| '.' || tage.sample",
        " AS sample",
        ", sam.year",
        if (grouping_by("timestep")) ", tts.name AS step",
        if (grouping_by("stocks"))   ", spe.stock AS stock",
        if (grouping_by("areas"))    ", tarea.name AS area",
        if (grouping_by("ages"))     ", tage.name AS age",
        if (grouping_by("lengths"))  select_clause(params$lengths, "lec.lengthcell", "length"),
        calc_cols,
        "FROM sample sam, species spe, catchsample cas, lengthcell lec, age age",
        if (grouping_by("timestep")) ", temp_ts tts",
        if (grouping_by("areas"))    ", temp_area tarea",
        if (grouping_by("ages"))     ", temp_age tage",
        "WHERE sam.sampleid = spe.sampleid AND spe.speciesid = cas.speciesid AND cas.catchsampleid = lec.catchsampleid AND lec.lengthcellid = age.lengthcellid",
        if (grouping_by("timestep")) "AND sam.month = tts.value",
        if (grouping_by("areas"))    "AND sam.subdivision = tarea.value",
        if (grouping_by("ages"))     "AND age.age = tage.value",
        where_clause(params$lengths, "lec.lengthcell"),
        sql_col_condition("sam.institute", params$institute, lookup="l_institute"),
        sql_col_condition("sam.year", params$year),
        sql_col_condition("sam.gearclass", params$gearclass, lookup="l_gearclass"),
        sql_col_condition("sam.gearsubclass", params$gearsubclass, lookup="l_gearsubclass"),
        sql_col_condition("sam.vesselclass", params$vesselclass, lookup="l_vesselclass"),
        sql_col_condition("sam.vesselsubclass", params$vesselsubclass, lookup="l_vesselsubclass"),
        sql_col_condition("spe.species", params$species, lookup="l_species"),
        sql_col_condition("spe.stock", params$stocks, lookup="l_stock"),
        sql_col_condition("spe.marketcategory", params$marketcategory, lookup="l_marketcategory"),
        sql_col_condition("cas.samplingtype", params$samplingtype, lookup="l_samplingtype"),
        sql_col_condition("cas.samplingstrategy", params$samplingstrategy, lookup="l_samplingstrategy"),
        sql_col_condition("lec.maturitystage", params$maturitystage, lookup="l_maturitystage"),
        sql_col_condition("lec.sexcode", params$sex, lookup="l_sexcode"),
        "GROUP BY ", paste(1:(2 + length(group_cols)), collapse=","),
        "ORDER BY ", paste(1:(2 + length(group_cols)), collapse=","),
        ""), collapse = " ")
    mdb$logger$debug(query)

    # Fetch all data, break it up by sample and annotate each
    out <- fetch(dbSendQuery(mdb$db, query), -1)
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
