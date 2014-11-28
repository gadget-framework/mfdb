# Print commands that will create schema
mfdb_show_schema <- function() {
    schema_from_0(NULL)
    invisible(NULL)
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
        schema_from_0(mdb)
    } else if (length(schema_version) > 1) {
        stop(paste("DB schema table has too many entries"))
    } else if (schema_version[1][1] != package_major_version()) {
        stop(paste("DB Schema version", schema_version[1][1], "does not match package version", package_major_version()))
    } else {
        mdb$logger$debug("Schema up-to-date")
    }
}

# Create MFDB schema from scratch, or print commands
schema_from_0 <- function(mdb) {
    create_taxonomy <- function(name, desc, id_type = "INT") {
        mfdb_create_table(mdb, name, desc, cols = c(
            paste0(name, "_id"), paste0(id_type, " PRIMARY KEY"), "Numeric ID for this entry",
            "name", "VARCHAR(1024) NOT NULL", "Short name used in data files / output data (in ltree notation)",
            "description", "VARCHAR(1024)", "Long description"
        ), keys = c(
            "CHECK(name ~ '^[A-Za-z0-9_.]+$')",
            "UNIQUE(name)"
        ))
    }

    mfdb_create_table(mdb, "mfdb_schema", "Table to keep track of schema version", cols = c(
        "version", "INT NOT NULL", "Version of MFDB schema"))
    mfdb_insert(mdb, "mfdb_schema", list(version = package_major_version()))

    create_taxonomy("case_study", "")
    mfdb_create_table(mdb, "areacell", "Vocabulary of available area cells", cols = c(
        "case_study_id", "INT REFERENCES case_study(case_study_id)", "Case study data is relevant to",
        "areacell_id", "INT", "",
        "name", "VARCHAR(1024) NOT NULL", "Short name used in data files / output data",
        "size", "INT", "Size of areacell"
    ), keys = c(
        "PRIMARY KEY(case_study_id, areacell_id)"
    ))
    mfdb_create_table(mdb, "division", "Grouping of area cells into divisions", cols = c(
        "division_id", "SERIAL PRIMARY KEY", "",
        "case_study_id", "INT REFERENCES case_study(case_study_id)", "Case study data is relevant to",
        "division", "VARCHAR(10) NOT NULL", "",
        "areacell_id", "INT", "Contained areacell"
    ), keys = c(
        "FOREIGN KEY(case_study_id, areacell_id) REFERENCES areacell(case_study_id, areacell_id)"
    ))
    mfdb_create_table(mdb, "temperature", "Time-series data for areacell temperature", cols = c(
        "temperature_id", "SERIAL PRIMARY KEY", "",
        "case_study_id", "INT REFERENCES case_study(case_study_id)", "Case study data is relevant to",
        "areacell_id", "INT", "Areacell data relates to",
        "year", "INT NOT NULL", "Year sample was undertaken",
        "month", "INT NOT NULL", "Month sample was undertaken",
        "temperature", "INT NOT NULL", "Temperature at this point in time"
    ), keys = c(
        "UNIQUE(areacell_id, year, month)",
        "FOREIGN KEY(case_study_id, areacell_id) REFERENCES areacell(case_study_id, areacell_id)"
    ))

    create_taxonomy("institute", "")
    create_taxonomy("gear", "")
    create_taxonomy("vessel", "")
    create_taxonomy("sampling_type", "")
    mfdb_create_table(mdb, "survey", "Description of survey", cols = c(
        "survey_id", "SERIAL PRIMARY KEY", "",
        "case_study_id", "INT REFERENCES case_study(case_study_id)", "Case study data is relevant to",
        "data_source", "VARCHAR(1024) NOT NULL", "Name of file/URL data came from",
        "institute_id", "INT REFERENCES institute(institute_id)", "Institute that undertook survey",
        "gear_id", "INT REFERENCES gear(gear_id)", "Gear used",
        "vessel_id", "INT REFERENCES vessel(vessel_id)", "Vessel used",
        "sampling_type_id", "INT REFERENCES sampling_type(sampling_type_id)", "Sampling type"
    ), keys = c(
        "UNIQUE(data_source)"
    ))

    create_taxonomy("sex", "")
    create_taxonomy("maturity_stage", "")
    create_taxonomy("species", "", id_type = "BIGINT")
    mfdb_create_table(mdb, "sample", "Samples within survey", cols = c(
        "sample_id", "SERIAL PRIMARY KEY", "",
        "survey_id", "INT REFERENCES survey(survey_id)", "",
        "case_study_id", "INT REFERENCES case_study(case_study_id)", "Case study data is relevant to",
        # Grouping columns
        "year", "INT NOT NULL", "Year sample was undertaken",
        "month", "INT NOT NULL", "Month sample was undertaken",
        "areacell_id", "INT", "Areacell data relates to",
        "species_id", "BIGINT REFERENCES species(species_id)", "",
        "age", "INT", "Age (years)",
        "sex_id", "INT REFERENCES sex(sex_id)", "Sex ID",
        "maturity_stage_id", "INT REFERENCES maturity_stage(maturity_stage_id)", "Maturity Stage ID",
        "length", "REAL", "Length of fish / mean length of all fish",
        "length_var", "REAL", "Length variance of all fish (given aggregated data)",
        "length_min", "INT", "Minimum theoretical value in this group",
        "weight", "REAL", "Weight of fish / mean weight of all fish",
        "weight_var", "REAL", "Weight variance of all fish (given aggregated data)",
        "count", "INT NOT NULL DEFAULT 1", "Number of fish meeting this criteria"
    ), keys = c(
        "CHECK(month BETWEEN 1 AND 12)",
        "FOREIGN KEY(case_study_id, areacell_id) REFERENCES areacell(case_study_id, areacell_id)"))
}

mfdb_taxonomy <- c("case_study", "institute", "fleet", "gear", "vessel", "market_category", "sampling_type", "sex", "maturity_stage", "species")
mfdb_cs_taxonomy <- c("areacell")

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

# Create any required indexes, if they don't already exist
mfdb_create_indexes <- function(mdb) {
   create_index <- function (table, cols) {
       tryCatch(mfdb_send(mdb, sql_create_index(table, cols)),
           error = function (e) {
               if (!grepl('relation "[^"]+" already exists', e$message)) stop(e)
           })
   }

   mdb$logger$info("Creating indexes")
}

# Return the major version of the package
package_major_version <- function () gsub("\\..*", "", packageVersion("mfdb"))
