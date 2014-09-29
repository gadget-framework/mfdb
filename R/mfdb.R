#        areas = c(),	# c('101', '101:1001'), e.g. Will group at most granular
#        timesteps = mfdb_group("ts", c(1,2,3),c(4,5,6)), groupings of months,
#        todo = NULL) {
mfdb <- function(db_connection = NULL, defaultparams = list(), save_tables = FALSE) {
    if (is.null(db_connection)) {
        db_connection <- dbConnect(PostgreSQL(), dbname="mf", host="/tmp/pg_mfdb")
    }
    mdb <- structure(list(
            logger = getLogger('mfdb'),
            defaultparams = c(defaultparams, list(
                    timesteps = mfdb_group(year = c(1,2,3,4,5,6,7,8,9,10,11,12)))),
            save_tables = save_tables,
            db = db_connection), class = "mfdb")

    schema_version <- tryCatch(
        dbFetch(dbSendQuery(mdb$db, "SELECT version FROM mfdb_schema")),
        error = function (e) NULL)

    if (is.null(schema_version) || length(schema_version) == 0) {
        # Nothing there yet, can create tables
        mdb$logger$info("Creating schema from scratch")
        create_tables(mdb)
    } else if (length(schema_version) > 1) {
        stop(paste("DB schema table has too many entries"))
    } else if (schema_version[1][1] != package_major_version()) {
        stop(paste("DB Schema version", schema_version[1][1], "does not match package version", package_major_version()))
    } else {
        mdb$logger$debug("Schema up-to-date")
    }

    invisible(mdb)
}

create_tables <- function(mdb) {
    send_query <- function (mdb, query) {
        if (is.null(mdb)) {
            cat(query, "\n\n")
        } else {
            dbSendQuery(mdb$db, query)
        }
    }

    send_query(mdb, sql_create_table(
        "mfdb_schema", "Table to keep track of schema version",
        "version INT NOT NULL"))
    send_query(mdb, paste0("INSERT INTO mfdb_schema VALUES (", sql_quote(package_major_version()), ")"))

    create_lookup <- function(name, desc) {
        send_query(mdb, sql_create_table(
            name, desc,
            paste0(name, "Id SERIAL PRIMARY KEY"), "Numeric ID for this entry",
            "name VARCHAR(1024) NOT NULL", "Short name used in data files",
            "description VARCHAR(1024) NOT NULL DEFAULT ''", "Long description",
            "UNIQUE(name)", ""))
    }
    create_lookup("institute", "")
    create_lookup("fleet", "")
    create_lookup("gear", "")
    create_lookup("vessel", "")
    create_lookup("marketCategory", "")
    create_lookup("samplingType", "")
    create_lookup("species", "")

    send_query(mdb, sql_create_table(
        "survey", "Description of survey",
        "surveyId SERIAL PRIMARY KEY", "",
        "dataSource VARCHAR(1024) NOT NULL", "Name of file/URL data came from",
        "year INT NOT NULL", "Year survey was undertaken",
        "instituteId INT REFERENCES institute(instituteId)", "Institute that undertook survey",
        "fleetId INT REFERENCES fleet(fleetId)", "Fleet name",
        "gearId INT REFERENCES gear(gearId)", "Gear used",
        "vesselId INT REFERENCES vessel(vesselId)", "Vessel used",
        "marketCategoryId INT REFERENCES marketCategory(marketCategoryId)", "Market category",
        "samplingTypeId INT REFERENCES samplingType(samplingTypeId)", "Sampling type"))

    # TODO: Should we have a numeric ID for areacell?
    send_query(mdb, sql_create_table(
        "areas", "Mapping of areacells to divisions",
        "dataSource VARCHAR(1024) NOT NULL", "Name of file/URL data came from",
        "division VARCHAR(10) NOT NULL", "",
        "areacell VARCHAR(10) NOT NULL", "e.g. ICES gridcell",
        "PRIMARY KEY(dataSource, division, areacell)", ""))

    send_query(mdb, sql_create_table(
        "sample", "Samples within survey",
        "sampleId SERIAL PRIMARY KEY", "",
        "surveyId INT REFERENCES survey(surveyId)", "",
        "month INT NOT NULL", "Month survey was undertaken",
        "areacell VARCHAR(10)", "e.g. ICES gridcell",
        "speciesId INT REFERENCES species(speciesId)", "",
        "lengthCellMin INT", "Lower bound of lengthcell",
        "lengthCellMax INT", "Upper bound of lengthcell"))
}

# Return the major version of the package
package_major_version <- function () gsub("\\..*", "", packageVersion("mfdb"))
