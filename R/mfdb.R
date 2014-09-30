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
            paste0(name, "_id SERIAL PRIMARY KEY"), "Numeric ID for this entry",
            "name VARCHAR(1024) NOT NULL", "Short name used in data files",
            "description VARCHAR(1024) NOT NULL DEFAULT ''", "Long description",
            "UNIQUE(name)", ""))
    }

    create_lookup("institute", "")
    create_lookup("fleet", "")
    create_lookup("gear", "")
    create_lookup("vessel", "")
    create_lookup("market_category", "")
    create_lookup("sampling_type", "")
    send_query(mdb, sql_create_table(
        "survey", "Description of survey",
        "survey_id SERIAL PRIMARY KEY", "",
        "data_source VARCHAR(1024) NOT NULL", "Name of file/URL data came from",
        "institute_id INT REFERENCES institute(institute_id)", "Institute that undertook survey",
        "fleet_id INT REFERENCES fleet(fleet_id)", "Fleet name",
        "gear_id INT REFERENCES gear(gear_id)", "Gear used",
        "vessel_id INT REFERENCES vessel(vessel_id)", "Vessel used",
        "market_category_id INT REFERENCES market_category(market_category_id)", "Market category",
        "sampling_type_id INT REFERENCES sampling_type(sampling_type_id)", "Sampling type"))

    # TODO: Should we have a numeric ID for areacell?
    send_query(mdb, sql_create_table(
        "area", "Mapping of areacells to divisions",
        "data_source VARCHAR(1024) NOT NULL", "Name of file/URL data came from",
        "division VARCHAR(10) NOT NULL", "",
        "areacell VARCHAR(10) NOT NULL", "e.g. ICES gridcell",
        "PRIMARY KEY(data_source, division, areacell)", ""))

    create_lookup("sex", "")
    create_lookup("species", "")
    send_query(mdb, sql_create_table(
        "sample", "Samples within survey",
        "sample_id SERIAL PRIMARY KEY", "",
        "survey_id INT REFERENCES survey(survey_id)", "",
        # Grouping columns
        "year INT NOT NULL", "Year sample was undertaken",
        "month INT NOT NULL", "Month sample was undertaken",
        "CHECK(month BETWEEN 1 AND 12)", "",
        "areacell VARCHAR(10)", "e.g. ICES gridcell",
        "species_id INT REFERENCES species(species_id)", "",
        "age INT", "Age (years)",
        "sex_id INT", "Sex ID",
        "lengthcell_min INT", "Lower bound of lengthcell",
        "lengthcell_max INT", "Upper bound of lengthcell",
        # Aggregation columns
        "count INT", "Number of fish within the criteria",
        "length_mean REAL", "Mean length of fish within the criteria",
        "length_var REAL", "Variance of fish length within the criteria",
        "weight_mean REAL", "Mean weight of fish within the criteria",
        "weight_var REAL", "Variance of fish weight within the criteria"))
}

# Return the major version of the package
package_major_version <- function () gsub("\\..*", "", packageVersion("mfdb"))
