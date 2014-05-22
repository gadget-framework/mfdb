#        areas = c(),	# c('101', '101:1001'), e.g. Will group at most granular
#        timesteps = mfdb_group("ts", c(1,2,3),c(4,5,6)), groupings of months,
#        todo = NULL) {
mfdb <- function(db_connection = NULL, defaultparams = list()) {
    if (is.null(db_connection)) {
        db_connection <- dbConnect(dbDriver("PostgreSQL"),
                dbname="dw0605",
                host="/tmp/")
    }
    structure(list(
            logger = getLogger('mfdb'),
            defaultparams = c(defaultparams, list(
                    lengthcellsize = 20, # TODO: Do we have to hardcode this? Could use lag()
                    timesteps = mfdb_group("ts", c(1,2,3,4,5,6,7,8,9,10,11,12)))),
            db = db_connection), class = "mfdb")
}

mfdb_area_sizes <- function (mdb, params = list()) {
    #TODO:
}

mfdb_temperatures <- function (mdb, params = list()) {
    #TODO:
}

# Return year,step,area,age,number (# of samples),mean (length), stddev (length)
mfdb_meanlength_stddev <- function (mdb, params = list()) {
    params <- c(params, mdb$defaultparams)
    mdb$logger$info(params)

    # TODO: Really need to define a weighted stddev aggregate function
    out <- mfdb_sample_grouping(mdb, params = params, calc_cols = c(
        ", SUM(age.agenum) AS number",
        ", AVG(age.agenum * (lec.lengthcell + ", params$lengthcellsize/2, ")) * (COUNT(*)::float / SUM(age.agenum)) AS mean",
        ", 0 AS stddev"))
    # TODO: Return list of columns that should be displayed as attribute too
    attr(out, "generator") <- "mfdb_meanlength_stddev"
    out
}

# Return year,step,area,age,number (# of samples),mean (length), stddev (length)
mfdb_meanlength <- function (mdb, params = list()) {
    params <- c(params, mdb$defaultparams)
    mdb$logger$info(params)

    # TODO: Really need to define a weighted stddev aggregate function
    out <- mfdb_sample_grouping(mdb, params = params, calc_cols = c(
        ", SUM(age.agenum) AS number",
        ", AVG(age.agenum * (lec.lengthcell + ", params$lengthcellsize/2, ")) * (COUNT(*)::float / SUM(age.agenum)) AS mean"))
    attr(out, "generator") <- "mfdb_meanlength"
    out
}

# Useful indexes:-
# CREATE INDEX age_lengthcellid ON age (lengthcellid);

# == mean-length
# area
# label1
# label2
#
# == mean-length.areaagg
# label1	101 102
# label2	103
# ==> Labels --> areas (subdivisions or divisions in DSTDW)
#
# == area
# areas 101 102 103
# size 100 200 300
# ==> Numeric areas --> size/temp over time

# mfdb_group(label1 = c(101, 102), label2 = c(103))
# ==> Table
#   key  	value
#   label1	101.1
#   label1	101.2
#   label1	101.3
#   label1	102.1
#   label1	102.2
#   label2	103.1
#   label2	103.2


# Aggregators
# time: label -> list
#   mfdb_group() is fine
# age:  label -> list
#   mfdb_group() is fine.
# area: label -> list
#   mfdb_areas('COD:101' => c('COD:101:1', 'COD:101:3'))
#   mfdb_group_like('101', '104')
#   * Group by values, display anothing relating to these
#   * Do I make an explicit table? Sample only has 70 areas down to gridcell
#     ==> Make explicit table (keys, everything that starts with that)
#      SELECT key1, value FROM areas WHERE key1 LIKE ? UNION ...
#      SELECT * FROM (SELECT DISTINCT division AS key, division || ':' || SUBSTRING(subdivision FROM 4 FOR 1) || ':'|| coalesce(gridcell, '') AS value FROM sample) moo WHERE value LIKE '101:%';
# length: label -> min < max
#   How does this interact with lengthstep?
#   mfdb_group_interval(min, max_non_inclusive, step)
#   Do we want to label them?
#   ==> No extra table, BETWEEN condition for min, max, then divide/floor by step

# Group up sample data by area, age or length
mfdb_sample_grouping <- function (mdb, params = list(), calc_cols = c()) {
    # Turn vector into a SQL IN condition, NA = NULL, optionally go via a lookup table.
    sql_col_condition <- function(col, v, lookup = NULL) {
        if (!is.vector(v)) return("")
        #TODO: Not sql-safe
        paste(
            "AND (", col, "IN (",
            if (!is.null(lookup)) paste(
                "SELECT", sub('^[a-z]+\\.', '', col), "FROM", lookup, "WHERE code IN ("),
            paste(
                sapply(v[!is.na(v)], function (x) { paste0("'", x, "'") }),
                collapse = ","),
            if (!is.null(lookup)) ")",
            ")",
            if (NA %in% v) paste("OR", col, "IS NULL"),
            ")")
    }
    # Generate interval condition given 2 values
    sql_interval_condition <- function(col, int_group, min_exclusive = FALSE, max_exclusive = FALSE) {
        if(is.null(int_group)) return("")
        #TODO: Not sql-safe
        paste("AND", col, if (min_exclusive) ">" else ">=", int_group$int_min,
              "AND", col, if (max_exclusive) "<" else "<=", int_group$int_max)
    }
    group_to_table <- function(db, table_name, group, datatype = "INT") {
        #TODO: Assign random ID attribute to group, use this as table name or re-use table if it already has one
        # Remove the table if it exists, and recreate it
        tryCatch(dbSendQuery(db, paste("DROP TABLE", table_name)), error = function (e) {})
        dbSendQuery(db, paste(
                "CREATE TEMPORARY TABLE", table_name, "(sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value ", datatype, ")"))

        # Flatten out into multiple key:value rows, populate table in one hit
        group_table <- denormalize(group)
        by(group_table, 1:nrow(group_table), function(v) {
            #NB: Once we upgrade postgresql can use multi-row insert form
            dbSendQuery(db, paste0("INSERT INTO ", table_name, " (sample, name, value) VALUES ('", v[[1]], "','", v[[2]], "','", v[[3]], "')"))
        })
    }

    # Store timestep data as a table, so we can join to it
    group_to_table(mdb$db, "temp_area", params$areas, datatype = "INT")
    group_to_table(mdb$db, "temp_ts", params$timestep, datatype = "INT")
    group_to_table(mdb$db, "temp_age", params$ages, datatype = "INT")

    # TODO: Add in indrection, so a gridcell can be in multiple subdivisions
    query <- paste(c(
        "SELECT MAX(tts.sample) ||'-'|| MAX(tarea.sample) ||'-'|| MAX(tage.sample) AS sample",
        ", sam.year",
        ", tts.name AS step",
        ", tarea.name AS area",
        ", tage.name AS age",
        calc_cols,
        "FROM sample sam, species spe, catchsample cas, lengthcell lec, age age",
        ", temp_ts tts",
        ", temp_age tage",
        ", temp_area tarea",
        "WHERE sam.sampleid = spe.sampleid AND spe.speciesid = cas.speciesid AND cas.catchsampleid = lec.catchsampleid AND lec.lengthcellid = age.lengthcellid",
        "AND sam.month = tts.value",
        "AND age.age = tage.value",
        "AND sam.subdivision = tarea.value",
        sql_interval_condition("lengthcell", params$lengths, max_exclusive = TRUE),
        sql_col_condition("sam.institute", params$institute, lookup="l_institute"),
        sql_col_condition("sam.year", params$year),
        sql_col_condition("sam.gearclass", params$gearclass, lookup="l_gearclass"),
        sql_col_condition("sam.gearsubclass", params$gearsubclass, lookup="l_gearsubclass"),
        sql_col_condition("sam.vesselclass", params$vesselclass, lookup="l_vesselclass"),
        sql_col_condition("sam.vesselsubclass", params$vesselsubclass, lookup="l_vesselsubclass"),
        sql_col_condition("spe.species", params$species, lookup="l_species"),
        sql_col_condition("spe.stock", params$stock, lookup="l_stock"),
        sql_col_condition("spe.marketcategory", params$marketcategory, lookup="l_marketcategory"),
        sql_col_condition("cas.samplingtype", params$samplingtype, lookup="l_samplingtype"),
        sql_col_condition("cas.samplingstrategy", params$samplingstrategy, lookup="l_samplingstrategy"),
        sql_col_condition("lec.maturitystage", params$maturitystage, lookup="l_maturitystage"),
        sql_col_condition("lec.sexcode", params$sex, lookup="l_sexcode"),
        "GROUP BY sam.year",
        ", tts.name, tage.name, tarea.name",
        "ORDER BY year, step, area, age"), collapse = " ")
    mdb$logger$debug(query)

    # Make data.table object, annotate with generation information
    out <- fetch(dbSendQuery(mdb$db, query), -1)
    attr(out, "generator") <- "mfdb_grouping"
    attr(out, "areas") <- params$areas
    attr(out, "ages") <- params$ages
    out
}
