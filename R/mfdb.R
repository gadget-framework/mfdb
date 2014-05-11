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

# Return year,step,area,age,number,mean,stddev
mfdb_meanlength <- function (mdb, params = list(), generate_stddev = TRUE) {
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
    sql_interval_condition <- function(col, min, max, min_exclusive = FALSE, max_exclusive = FALSE) {
        if(is.null(min) || is.null(max)) return("")
        #TODO: Not sql-safe
        paste("AND", col, if (min_exclusive) ">" else ">=", min,
              "AND", col, if (max_exclusive) "<" else "<=", max)
    }
    group_to_table <- function(db, name, group, datatype = "INT") {
        table_name <- paste0("temp_", name)
        # Remove the table if it exists, and recreate it
        tryCatch(dbSendQuery(db, paste("DROP TABLE", table_name)), error = function (e) {})
        dbSendQuery(db, paste(
                "CREATE TEMPORARY TABLE", table_name, "(name VARCHAR(10), value ", datatype, ")"))

        # Flatten out into multiple key:value rows, populate table in one hit
        for (v in denormalize(group)) {
            #NB: Once we upgrade postgresql can use multi-row insert form
            dbSendQuery(db, paste0("INSERT INTO ", table_name, " (name, value) VALUES ('", v[1], "','", v[2], "')"))
        }
    }

    params <- c(params, mdb$defaultparams)
    mdb$logger$info(params)

    # Sort area array into division, subdivision and gridcell conditions
    area_group <- 0
    division <- c() ; subdivision <- c() ; gridcell <- c()
    for (parts in sapply(params$areas, function (x) { unlist(strsplit(x, ":")) })) {
        area_group <- max(area_group, length(parts))
        # TODO: This isn't AND'ing the conditions, but not sure we care?
        if(!is.na(parts[1])) division <- c(division, parts[1])
        if(!is.na(parts[2])) subdivision <- c(subdivision, parts[1])
        if(!is.na(parts[3])) gridcell <- c(gridcell, parts[1])
    }
    if (area_group == 0) {
        area_group <- c()
    } else {
        area_group <- c("sam.division", "sam.subdivision", "sam.gridcell")[1:area_group]
    }

    # Store timestep data as a table, so we can join to it
    group_to_table(mdb$db, "ts", params$timestep, datatype = "INT")
    group_to_table(mdb$db, "age", params$age, datatype = "INT")

    # Should have cols year,step,area,age,number,mean,stddev
    query <- paste(
        "SELECT sam.year",
        ", tts.name AS step",
        ", ", paste(if (length(area_group) == 0) "'allareas'" else area_group, collapse = "||':'||"), " AS area",
        ", tage.name AS age",
        ", SUM(age.agenum) AS number",
        ", AVG(age.agenum * (lec.lengthcell + ", params$lengthcellsize/2, ")) * (COUNT(*)::float / SUM(age.agenum)) AS mean",
        if (generate_stddev) ", 0 AS stddev" else "", # TODO: Really need to define a weighted stddev aggregate function
        "FROM sample sam, species spe, catchsample cas, lengthcell lec, age age",
        ", temp_ts tts",
        ", temp_age tage",
        "WHERE sam.sampleid = spe.sampleid AND spe.speciesid = cas.speciesid AND cas.catchsampleid = lec.catchsampleid AND lec.lengthcellid = age.lengthcellid",
        "AND sam.month = tts.value",
        "AND age.age = tage.value",
        sql_interval_condition("lengthcell", params$lengthcellmin, params$lengthcellmax, max_exclusive = TRUE),
        sql_col_condition("sam.institute", params$institute, lookup="l_institute"),
        sql_col_condition("sam.year", params$year),
        sql_col_condition("sam.division", division),
        sql_col_condition("sam.subdivision", subdivision),
        sql_col_condition("sam.gridcell", gridcell),
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
        "GROUP BY sam.year, tts.name",
        paste(lapply(area_group, function (x) { paste(",", x) }), collapse = ""),
        ", tage.name",
        "ORDER BY 1,2,3,4")
    mdb$logger$debug(query)

    # Make data.table object, annotate with generation information
    out <- fetch(dbSendQuery(mdb$db, query), -1)
    attr(out, "generator") <- "mfdb_meanlength"
    attr(out, "area") <- params$area
    attr(out, "age") <- params$age
    out
}
