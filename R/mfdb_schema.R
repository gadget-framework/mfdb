# Print commands that will create schema
mfdb_show_schema <- function() {
    schema_from_0(NULL)
    invisible(NULL)
}

# Destroy everything in current schema
mfdb_destroy_schema <- function(mdb) {
    for(t in c('prey', 'predator', 'sample', 'survey', 'division', 'survey_index', mfdb_cs_taxonomy, mfdb_taxonomy, 'mfdb_schema')) {
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
    schema_version <- tryCatch({
        res <- mfdb_fetch(mdb, "SELECT MAX(version) FROM mfdb_schema")
        ifelse(nrow(res) == 0, 0, res[1, 1])
    }, error = function (e) 0)
    target_version <- package_major_version()

    # Find appropriate function and run it
    fn <- tryCatch(get(paste0("schema_from_", schema_version)), error = function (e) {
        stop(paste(
            "DB Schema version", schema_version,
            "does not match package version", target_version,
            "& no upgrade step available.\n",
            "Call mfdb(destroy_schema = TRUE) first.",
            "Warning: This *will destroy* any existing data"))
    })
    fn(mdb)
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

    mdb$logger$info("Creating schema from scratch")

    mfdb_create_table(mdb, "mfdb_schema", "Table to keep track of schema version", cols = c(
        "version", "INT NOT NULL", "Version of MFDB schema"))
    mfdb_insert(mdb, "mfdb_schema", list(version = package_major_version()))

    # Create all required taxonomy tables
    for (t in c(mfdb_taxonomy, mfdb_cs_taxonomy)) mfdb_create_taxonomy_table(mdb, t)

    mfdb_create_table(mdb, "survey_index", "Indices used to modify surveys", cols = c(
        "survey_index_id", "SERIAL PRIMARY KEY", "",
        "data_source_id", "INT", "",
        "case_study_id", "INT REFERENCES case_study(case_study_id)", "Case study data is relevant to",
        "index_type_id", "INT", "",

        "areacell_id", "INT", "Areacell data relates to",
        "year", "INT NOT NULL", "Year sample was taken",
        "month", "INT NOT NULL", "Month sample was taken",
        "value", "REAL NOT NULL", "Value at this point in time"
    ), keys = c(
        fk('data_source', 'index_type', 'areacell')
    ))

    mfdb_create_table(mdb, "division", "Grouping of area cells into divisions", cols = c(
        "division_id", "SERIAL PRIMARY KEY", "",
        "case_study_id", "INT REFERENCES case_study(case_study_id)", "Case study data is relevant to",

        "division", "VARCHAR(10) NOT NULL", "",
        "areacell_id", "INT", "Contained areacell"
    ), keys = c(
        "FOREIGN KEY(case_study_id, areacell_id) REFERENCES areacell(case_study_id, areacell_id)"
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

    mfdb_create_table(mdb, "predator", "Predators in predator/prey sample", cols = c(
        "predator_id", "SERIAL PRIMARY KEY", "",
        "data_source_id", "INT NOT NULL", "",
        "case_study_id", "INT NOT NULL REFERENCES case_study(case_study_id)", "Case study data is relevant to",

        "institute_id", "INT REFERENCES institute(institute_id)", "Institute that undertook survey",
        "gear_id", "INT REFERENCES gear(gear_id)", "Gear used",
        "vessel_id", "INT REFERENCES vessel(vessel_id)", "Vessel used",
        "sampling_type_id", "INT", "Sampling type",

        "year", "INT NOT NULL", "Year sample was undertaken",
        "month", "INT NOT NULL", "Month sample was undertaken",
        "areacell_id", "INT", "Areacell data relates to",

        "stomach_name", "VARCHAR(128) NOT NULL", "Stomach identifier",
        "species_id", "BIGINT REFERENCES species(species_id)", "",
        "age", "INT", "Age (years)",
        "sex_id", "INT REFERENCES sex(sex_id)", "Sex ID",
        "maturity_stage_id", "INT REFERENCES maturity_stage(maturity_stage_id)", "Maturity Stage ID",
        "stomach_state_id", "INT REFERENCES stomach_state(stomach_state_id)", "Status of stomach when caught",

        "length", "REAL", "Length of predator",
        "weight", "REAL", "Weight of predator",
        NULL
    ), keys = c(
        "CHECK(month BETWEEN 1 AND 12)",
        fk('data_source', 'areacell', 'sampling_type')
    ))

    mfdb_create_table(mdb, "prey", "Prey in predator/prey sample", cols = c(
        "prey_id", "SERIAL PRIMARY KEY", "",
        "predator_id", "INT NOT NULL REFERENCES predator(predator_id)", "The stomach this sample was found in",

        "species_id", "BIGINT REFERENCES species(species_id)", "",
        "digestion_stage_id", "INT REFERENCES digestion_stage(digestion_stage_id)", "Digestion stage",

        "length", "REAL", "Length of prey / mean length of all prey",
        "weight", "REAL", "Weight of prey / mean weight of all prey",
        "count", "INT NOT NULL DEFAULT 1", "Number of prey meeting this criteria",
        NULL
    ), keys = c(
    ))
}

schema_from_2 <- function(mdb) {
    mdb$logger$info("Schema up-to-date")
}

mfdb_taxonomy <- c("case_study", "institute", "fleet", "gear", "vessel", "market_category", "sex", "maturity_stage", "species", "stomach_state", "digestion_stage")
mfdb_cs_taxonomy <- c("areacell", "sampling_type", "data_source", "index_type")
mfdb_measurement_tables <- c('survey_index', 'division', 'sample', 'predator', 'prey')

mfdb_create_taxonomy_table <- function(mdb, table_name) {
    key_col <- paste0(table_name, "_id")
    if (table_name %in% mfdb_taxonomy) {
        mfdb_create_table(mdb, table_name, "", cols = c(
            key_col, ifelse(table_name == "species", "BIGINT", "INT"), "Numeric ID for this entry",
            "name", "VARCHAR(1024) NOT NULL", "Short name used in data files / output data (in ltree notation)",
            "description", "VARCHAR(1024)", "Long description",
            NULL
        ), keys = c(
            paste0(c("PRIMARY KEY(", key_col, ")"), collapse = ""),
            "CHECK(name ~ '^[A-Za-z0-9_.\\-]+$')",
            paste0("UNIQUE(name)"),
            NULL
        ))
    } else if (table_name %in% mfdb_cs_taxonomy) {
        mfdb_create_table(mdb, table_name, "", cols = c(
            "case_study_id", "INT REFERENCES case_study(case_study_id)", "Case study data is relevant to",
            key_col, ifelse(table_name == "data_source", "SERIAL", "INT"), "Numeric ID for this entry",
            "name", "VARCHAR(1024) NOT NULL", "Short name used in data files / output data (in ltree notation)",
            if (table_name == "areacell") c(
                "size", "INT", "Size of areacell",
                NULL
            ) else c(
                "description", "VARCHAR(1024)", "Long description",
                NULL
            ),
            NULL
        ), keys = c(
            paste0(c("PRIMARY KEY(case_study_id, ", key_col, ")"), collapse = ""),
            "CHECK(name ~ '^[A-Za-z0-9_.\\-]+$')",
            paste0("UNIQUE(case_study_id, name)"),
            NULL
        ))
    }
}

# Populate tables with package-provided data
mfdb_update_taxonomy <- function(mdb) {
    for (t in mfdb_taxonomy) {
        mfdb_import_taxonomy(mdb, t, get(t, pos = as.environment("package:mfdb")))
    }
}

# Populate case-study taxonomy tables with default data
mfdb_update_cs_taxonomy <- function(mdb) {
    mfdb_import_cs_taxonomy(mdb, "index_type", data.frame(
        id = c(99999),
        name = 'temperature',
        stringsAsFactors = FALSE))
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
