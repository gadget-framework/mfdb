# Print commands that will create schema
mfdb_show_schema <- function() {
    schema_from_0(NULL)
    invisible(NULL)
}

# Destroy everything in current schema
mfdb_destroy_schema <- function(mdb) {
    for(t in c('sample', 'survey', 'temperature', 'division', mfdb_cs_taxonomy, mfdb_taxonomy, 'mfdb_schema')) {
        mdb$logger$info(paste("Removing table", t))
        tryCatch(mfdb_send(mdb, "DROP TABLE ", t, " CASCADE"), error = function(e) {
            if(grepl("does not exist", e$message)) return();
            stop(e)
        })
    }
    invisible(TRUE)
}

# Check to see if we need to update schema do it,
mfdb_update_schema <- function(mdb) {
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
        stop(paste(
            "DB Schema version", schema_version[1][1],
            "does not match package version", package_major_version(),
            "no upgrade step available. call mdb(destroy_schema = TRUE) first.",
            "Warning: This *will destroy* any existing data"))
    } else {
        mdb$logger$debug("Schema up-to-date")
    }
}

# Create MFDB schema from scratch, or print commands
schema_from_0 <- function(mdb) {
    fk <- function (...) {
        tbls <- c(...)[c(...) %in% mfdb_taxonomy]
        cs_tbls <- c(...)[c(...) %in% mfdb_cs_taxonomy]
        c(
            if (length(cs_tbls) > 0) paste0("FOREIGN KEY(case_study_id, ", cs_tbls, "_id) REFERENCES ", cs_tbls, "(case_study_id, ", cs_tbls, "_id)"),
            if (length(tbls) > 0) paste0("FOREIGN KEY(", tbls, "_id) REFERENCES ", tbls, "(", tbls, "_id)"),
            NULL
        )
    }

    mfdb_create_table(mdb, "mfdb_schema", "Table to keep track of schema version", cols = c(
        "version", "INT NOT NULL", "Version of MFDB schema"))
    mfdb_insert(mdb, "mfdb_schema", list(version = package_major_version()))

    # Create all required taxonomy tables
    for (t in mfdb_taxonomy) {
        mfdb_create_table(mdb, t, "", cols = c(
            paste0(t, "_id"), ifelse(t == "species", "BIGINT", "INT"), "Numeric ID for this entry",
            "name", "VARCHAR(1024) NOT NULL", "Short name used in data files / output data (in ltree notation)",
            "description", "VARCHAR(1024)", "Long description",
            NULL
        ), keys = c(
            paste0(c("PRIMARY KEY(", paste0(t, "_id"), ")"), collapse = ""),
            "CHECK(name ~ '^[A-Za-z0-9_.\\-]+$')",
            paste0("UNIQUE(name)"),
            NULL
        ))
    }
    for (t in mfdb_cs_taxonomy) {
        mfdb_create_table(mdb, t, "", cols = c(
            "case_study_id", "INT REFERENCES case_study(case_study_id)", "Case study data is relevant to",
            paste0(t, "_id"), ifelse(t == "data_source", "SERIAL", "INT"), "Numeric ID for this entry",
            "name", "VARCHAR(1024) NOT NULL", "Short name used in data files / output data (in ltree notation)",
            if (t == "areacell") c(
                "size", "INT", "Size of areacell",
                NULL
            ) else c(
                "description", "VARCHAR(1024)", "Long description",
                NULL
            ),
            NULL
        ), keys = c(
            paste0(c("PRIMARY KEY(case_study_id, ", paste0(t, "_id"), ")"), collapse = ""),
            "CHECK(name ~ '^[A-Za-z0-9_.\\-]+$')",
            paste0("UNIQUE(case_study_id, name)"),
            NULL
        ))
    }

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
        "temperature", "REAL NOT NULL", "Temperature at this point in time"
    ), keys = c(
        "UNIQUE(case_study_id, areacell_id, year, month)",
        fk('areacell')
    ))

    mfdb_create_table(mdb, "sample", "Samples within a survey", cols = c(
        "sample_id", "SERIAL PRIMARY KEY", "",
        "data_source_id", "INT", "",
        "case_study_id", "INT REFERENCES case_study(case_study_id)", "Case study data is relevant to",

        "institute_id", "INT REFERENCES institute(institute_id)", "Institute that undertook survey",
        "gear_id", "INT REFERENCES gear(gear_id)", "Gear used",
        "vessel_id", "INT REFERENCES vessel(vessel_id)", "Vessel used",
        "sampling_type_id", "INT", "Sampling type",

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
        fk('data_source', 'areacell', 'sampling_type')
    ))
}

mfdb_taxonomy <- c("case_study", "institute", "fleet", "gear", "vessel", "market_category", "sex", "maturity_stage", "species")
mfdb_cs_taxonomy <- c("areacell", "sampling_type", "data_source")

# Populate tables with package-provided data
mfdb_update_taxonomy <- function(mdb) {
    mfdb_import_taxonomy(mdb, "case_study", mfdb::case_study)
    mfdb_import_taxonomy(mdb, "institute", mfdb::institute)
    mfdb_import_taxonomy(mdb, "gear", mfdb::gear)
    mfdb_import_taxonomy(mdb, "vessel", mfdb::vessel)

    mfdb_import_taxonomy(mdb, "sex", mfdb::sex)
    mfdb_import_taxonomy(mdb, "maturity_stage", mfdb::maturity_stage)
    mfdb_import_taxonomy(mdb, "species", mfdb::species)
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
