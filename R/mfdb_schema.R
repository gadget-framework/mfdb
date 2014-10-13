# Print commands that will create schema
mfdb_show_schema <- function() {
    schema_from_0(function (query) { cat(query, "\n\n") })
    return(schema_from_0(NULL))
}

# Check to see if we need to update schema do it, if !read_only
mfdb_update_schema <- function(mdb, read_only = FALSE) {
    # Find out existing schema version
    schema_version <- tryCatch(
        mfdb_fetch(mdb, "SELECT version FROM mfdb_schema"),
        error = function (e) NULL)

    if (is.null(schema_version) || length(schema_version) == 0) {
        # Nothing there yet, can create tables
        mdb$logger$info("Creating schema from scratch")
        schema_from_0(function (query) { dbSendQuery(mdb$db, query) })
    } else if (length(schema_version) > 1) {
        stop(paste("DB schema table has too many entries"))
    } else if (schema_version[1][1] != package_major_version()) {
        stop(paste("DB Schema version", schema_version[1][1], "does not match package version", package_major_version()))
    } else {
        mdb$logger$debug("Schema up-to-date")
    }
}

# Create MFDB schema from scratch, or print commands
schema_from_0 <- function(send) {
    create_taxonomy <- function(name, desc, id_type = "INT") {
        send(sql_create_table(
            name, desc,
            paste0(name, "_id ", id_type," PRIMARY KEY"), "Numeric ID for this entry",
            "name VARCHAR(1024) NOT NULL", "Short name used in data files / output data",
            "CHECK(name ~ '^[A-Za-z0-9_.]+$')", "Labels should be in ltree notation",
            "description VARCHAR(1024)", "Long description",
            "UNIQUE(name)", ""))
    }

    send(sql_create_table(
        "mfdb_schema", "Table to keep track of schema version",
        "version INT NOT NULL"))
    send(paste0("INSERT INTO mfdb_schema VALUES (", sql_quote(package_major_version()), ")"))

    create_taxonomy("case_study", "")
    create_taxonomy("institute", "")
    create_taxonomy("gear", "")
    create_taxonomy("vessel", "")
    create_taxonomy("sampling_type", "")
    send(sql_create_table(
        "survey", "Description of survey",
        "survey_id SERIAL PRIMARY KEY", "",
        "case_study_id INT REFERENCES case_study(case_study_id)", "Case study data is relevant to",
        "data_source VARCHAR(1024) NOT NULL", "Name of file/URL data came from",
        "UNIQUE(data_source)", "",
        "institute_id INT REFERENCES institute(institute_id)", "Institute that undertook survey",
        "gear_id INT REFERENCES gear(gear_id)", "Gear used",
        "vessel_id INT REFERENCES vessel(vessel_id)", "Vessel used",
        "sampling_type_id INT REFERENCES sampling_type(sampling_type_id)", "Sampling type"))

    # TODO: Should we have a numeric ID for areacell?
    send(sql_create_table(
        "area", "Mapping of areacells to divisions",
        "area_id SERIAL PRIMARY KEY", "",
        "case_study_id INT REFERENCES case_study(case_study_id)", "Case study data is relevant to",
        "data_source VARCHAR(1024) NOT NULL", "Name of file/URL data came from",
        "division VARCHAR(10) NOT NULL", "",
        "areacell VARCHAR(10) NOT NULL", "e.g. ICES gridcell"))

    create_taxonomy("sex", "")
    create_taxonomy("species", "", id_type = "BIGINT")
    send(sql_create_table(
        "sample", "Samples within survey",
        "sample_id SERIAL PRIMARY KEY", "",
        "survey_id INT REFERENCES survey(survey_id)", "",
        # Grouping columns
        "year INT NOT NULL", "Year sample was undertaken",
        "month INT NOT NULL", "Month sample was undertaken",
        "CHECK(month BETWEEN 1 AND 12)", "",
        "areacell VARCHAR(10)", "e.g. ICES gridcell",
        "species_id BIGINT REFERENCES species(species_id)", "",
        "age INT", "Age (years)",
        "sex_id INT", "Sex ID",
        "length REAL", "Length of fish / mean length of all fish",
        "length_var REAL", "Length variance of all fish (given aggregated data)",
        "length_min INT", "Minimum theoretical value in this group",
        "weight REAL", "Weight of fish / mean weight of all fish",
        "weight_var REAL", "Weight variance of all fish (given aggregated data)",
        "count INT NOT NULL DEFAULT 1", "Number of fish meeting this criteria"))
}

# Populate tables with package-provided data
mfdb_update_taxonomy <- function(mdb) {
    mfdb_import_taxonomy(mdb, "case_study", case_study)
    mfdb_import_taxonomy(mdb, "institute", institute)
    mfdb_import_taxonomy(mdb, "gear", gear)
    mfdb_import_taxonomy(mdb, "vessel", vessel)
    mfdb_import_taxonomy(mdb, "sampling_type", sampling_type)

    mfdb_import_taxonomy(mdb, "sex", sex)
    mfdb_import_taxonomy(mdb, "species", species)
}

# Return the major version of the package
package_major_version <- function () gsub("\\..*", "", packageVersion("mfdb"))
