#        areas = c(),	# c('101', '101:1001'), e.g. Will group at most granular
#        timesteps = mfdb_group("ts", c(1,2,3),c(4,5,6)), groupings of months,
#        todo = NULL) {
mfdb <- function(db_connection = NULL, defaultparams = list()) {
    if (is.null(db_connection)) {
        db_connection <- dbConnect(PostgreSQL(),
                dbname="dw0605",
                host="/tmp/")
    }
    invisible(structure(list(
            logger = getLogger('mfdb'),
            defaultparams = c(defaultparams, list(
                    timesteps = mfdb_group(year = c(1,2,3,4,5,6,7,8,9,10,11,12)))),
            db = db_connection), class = "mfdb"))
}

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

sql_quote <- function(v) {
    paste0("'", gsub("'", "''", v) ,"'")
}
select_clause <- function(x, col, outputname) {
    if (is.null(x)) return("")
    UseMethod("select_clause")
}
where_clause <- function(x, col) {
    if (is.null(x)) return("")
    UseMethod("where_clause")
}

# Group up sample data by area, age or length
mfdb_sample_grouping <- function (mdb,
        params = list(),
        group_cols = c("timestep", "areas", "ages"),
        calc_cols = c(),
        generator = "mfdb_sample_grouping") {
    # Turn vector into a SQL IN condition, NA = NULL, optionally go via a lookup table.
    sql_col_condition <- function(col, v, lookup = NULL) {
        if (!is.vector(v)) return("")
        paste(
            "AND (", col, "IN (",
            if (!is.null(lookup)) paste(
                "SELECT", sub('^[a-z]+\\.', '', col), "FROM", lookup, "WHERE code IN ("),
            paste(
                sapply(v[!is.na(v)], function (x) { sql_quote(x) }),
                collapse = ","),
            if (!is.null(lookup)) ")",
            ")",
            if (NA %in% v) paste("OR", col, "IS NULL"),
            ")")
    }
    # Generate interval condition given 2 values
    sql_interval_condition <- function(col, int_group, min_exclusive = FALSE, max_exclusive = FALSE) {
        if(is.null(int_group)) return("")
        paste("AND", col, if (min_exclusive) ">" else ">=", sql_quote(int_group$int_min),
              "AND", col, if (max_exclusive) "<" else "<=", sql_quote(int_group$int_max))
    }
    group_to_table <- function(db, table_name, group, datatype = "INT") {
        #TODO: This error message provides table name, not parameter
        if (is.null(group)) stop(paste("You must provide a mfdb_group for", table_name))
        #TODO: Assign random ID attribute to group, use this as table name or re-use table if it already has one
        # Remove the table if it exists, and recreate it
        tryCatch(dbSendQuery(db, paste("DROP TABLE", table_name)), error = function (e) {})
        dbSendQuery(db, paste(
                "CREATE TEMPORARY TABLE", table_name, "(sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value ", datatype, ")"))

        # Flatten out into multiple key:value rows, populate table in one hit
        group_table <- denormalize(group)
        by(group_table, 1:nrow(group_table), function(v) {
            #NB: Once we upgrade postgresql can use multi-row insert form
            dbSendQuery(db, paste0("INSERT INTO ", table_name, " (sample, name, value) VALUES (", sql_quote(v[[1]]), ",", sql_quote(v[[2]]), ",", sql_quote(v[[3]]), ")"))
        })
    }
    # True iff str is in the group_cols parameter
    grouping_by <- function(str) {
        str %in% group_cols
    }

    params <- c(params, mdb$defaultparams)
    mdb$logger$info(params)
    # Store groups into temporary tables for joining
    if (grouping_by("timestep")) group_to_table(mdb$db, "temp_ts", params$timestep, datatype = "INT")
    if (grouping_by("areas")) group_to_table(mdb$db, "temp_area", params$areas, datatype = "INT")
    if (grouping_by("ages"))  group_to_table(mdb$db, "temp_age", params$ages, datatype = "INT")

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
