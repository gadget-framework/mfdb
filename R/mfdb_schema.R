# Print commands that will create schema
mfdb_show_schema <- function() {
    schema_from_0(NULL)
    invisible(NULL)
}

# Destroy everything in current schema
mfdb_destroy_schema <- function(mdb) {
    if (mdb$schema == 'public') {
        for(t in c('prey', 'predator', 'sample', 'survey', 'division', 'survey_index', 'fleet', mfdb_cs_taxonomy, mfdb_taxonomy, 'mfdb_schema')) {
            mdb$logger$info(paste("Removing table", t))
            tryCatch(mfdb_send(mdb, "DROP TABLE ", mdb$schema, ".", t, " CASCADE"), error = function(e) {
                if(grepl("does not exist", e$message)) return();
                stop(e)
            })
        }
    } else {
        mfdb_send(mdb, "DROP SCHEMA ", mdb$schema, " CASCADE")
    }
    invisible(TRUE)
}

# Check to see if we need to update schema do it,
mfdb_update_schema <- function(
        mdb,
        schema_version = -1,
        target_version = package_major_version()) {
    while (TRUE) {
        # Find out existing schema version, if it's what we want return
        schema_version <- tryCatch({
            res <- mfdb_fetch(mdb, "SELECT MAX(version) FROM ", ifelse(
                target_version < 5,
                "mfdb_schema",
                paste0(mdb$schema, ".mfdb_schema")))
            ifelse(nrow(res) == 0, 0, res[1, 1])
        }, error = function (e) 0)

        if (schema_version == target_version) {
            return();
        }

        if (schema_version > target_version) {
            stop("Cannot downgrade schema from ", schema_version, " to ", target_version)
        }

        fn <- tryCatch(get(paste0("schema_from_", schema_version)), error = function (e) {
            stop(paste(
                "DB Schema version", schema_version,
                "does not match package version", target_version,
                "& no upgrade step available.\n",
                "Call mfdb('", mdb$schema, "', destroy_schema = TRUE) first.",
                "Warning: This *will destroy* any existing data"))
            })
        fn(mdb)
    }
}

# Generate foreign key definition for each table given
fk <- function (...) {
    tbls <- c(...)[c(...) %in% c(mfdb_taxonomy, mfdb_cs_taxonomy)]
    c(
        if (length(tbls) > 0) paste0("FOREIGN KEY(", tbls, "_id) REFERENCES ", tbls, "(", tbls, "_id)"),
        NULL
    )
}

# Create MFDB schema from scratch, or print commands
schema_from_0 <- function(mdb) {
    mdb$logger$info("Creating schema from scratch")

    mfdb_create_table(mdb, "mfdb_schema", "Table to keep track of schema version", cols = c(
        "version", "INT NOT NULL", "Version of MFDB schema"))
    mfdb_insert(mdb, "mfdb_schema", list(version = package_major_version()))

    # Create all required taxonomy tables
    for (t in c(mfdb_taxonomy, mfdb_cs_taxonomy)) mfdb_create_taxonomy_table(mdb, t)

    mfdb_create_table(mdb, "survey_index", "Indices used to modify surveys", cols = c(
        "survey_index_id", "SERIAL PRIMARY KEY", "",
        "data_source_id", "INT", "",
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

        "division", "VARCHAR(10) NOT NULL", "",
        "areacell_id", "INT", "Contained areacell"
    ), keys = c(
        "FOREIGN KEY(areacell_id) REFERENCES areacell(areacell_id)"
    ))

    mfdb_create_table(mdb, "sample", "Samples within a survey", cols = c(
        "sample_id", "SERIAL PRIMARY KEY", "",
        "data_source_id", "INT", "",

        "institute_id", "INT REFERENCES institute(institute_id)", "Institute that undertook survey",
        "gear_id", "INT REFERENCES gear(gear_id)", "Gear used",
        "vessel_id", "INT", "Vessel used",
        "tow_id", "INT", "Tow used",
        "sampling_type_id", "INT", "Sampling type",

        "year", "INT NOT NULL", "Year sample was undertaken",
        "month", "INT NOT NULL", "Month sample was undertaken",
        "areacell_id", "INT", "Areacell data relates to",
        "species_id", "BIGINT REFERENCES species(species_id)", "",
        "age", "REAL", "Age (years)",
        "sex_id", "INT REFERENCES sex(sex_id)", "Sex ID",
        "maturity_stage_id", "INT REFERENCES maturity_stage(maturity_stage_id)", "Maturity Stage ID",

        "length", "REAL", "Length of fish / mean length of all fish",
        "length_var", "REAL", "Length variance of all fish (given aggregated data)",
        "length_min", "INT", "Minimum theoretical value in this group",
        "weight", "REAL", "Weight of fish / mean weight of all fish",
        "weight_var", "REAL", "Weight variance of all fish (given aggregated data)",
        "count", "REAL DEFAULT 1", "Number of fish meeting this criteria"
    ), keys = c(
        "CHECK(month BETWEEN 1 AND 12)",
        fk('data_source', 'areacell', 'vessel', 'tow', 'sampling_type')
    ))

    mfdb_create_table(mdb, "predator", "Predators in predator/prey sample", cols = c(
        "predator_id", "SERIAL PRIMARY KEY", "",
        "data_source_id", "INT NOT NULL", "",

        "institute_id", "INT REFERENCES institute(institute_id)", "Institute that undertook survey",
        "gear_id", "INT REFERENCES gear(gear_id)", "Gear used",
        "vessel_id", "INT", "Vessel used",
        "tow_id", "INT", "Tow used",
        "sampling_type_id", "INT", "Sampling type",

        "year", "INT NOT NULL", "Year sample was undertaken",
        "month", "INT NOT NULL", "Month sample was undertaken",
        "areacell_id", "INT", "Areacell data relates to",

        "stomach_name", "VARCHAR(128) NOT NULL", "Stomach identifier",
        "species_id", "BIGINT REFERENCES species(species_id)", "",
        "age", "REAL", "Age (years)",
        "sex_id", "INT REFERENCES sex(sex_id)", "Sex ID",
        "maturity_stage_id", "INT REFERENCES maturity_stage(maturity_stage_id)", "Maturity Stage ID",
        "stomach_state_id", "INT REFERENCES stomach_state(stomach_state_id)", "Status of stomach when caught",

        "length", "REAL", "Length of predator",
        "weight", "REAL", "Weight of predator",
        NULL
    ), keys = c(
        "CHECK(month BETWEEN 1 AND 12)",
        fk('data_source', 'areacell', 'vessel', 'tow', 'sampling_type')
    ))

    mfdb_create_table(mdb, "prey", "Prey in predator/prey sample", cols = c(
        "prey_id", "SERIAL PRIMARY KEY", "",
        "predator_id", "INT NOT NULL REFERENCES predator(predator_id)", "The stomach this sample was found in",

        "species_id", "BIGINT REFERENCES species(species_id)", "",
        "digestion_stage_id", "INT REFERENCES digestion_stage(digestion_stage_id)", "Digestion stage",

        "length", "REAL", "Length of prey / mean length of all prey",
        "weight", "REAL", "Weight of prey / mean weight of all prey",
        "count", "INT DEFAULT 1", "Number of prey meeting this criteria",
        NULL
    ), keys = c(
    ))
}

schema_from_2 <- function(mdb) {
    mdb$logger$info("Upgrading schema from version 2")
    mfdb_send(mdb, "ALTER TABLE sample ALTER COLUMN count DROP NOT NULL")
    mfdb_send(mdb, "ALTER TABLE sample ALTER COLUMN count TYPE REAL")

    mfdb3_taxonomy <- c("case_study", "institute", "gear", "vessel", "market_category", "sex", "maturity_stage", "species", "stomach_state", "digestion_stage")
    mfdb3_cs_taxonomy <- c("areacell", "fleet", "sampling_type", "data_source", "index_type")
    mfdb3_create_taxonomy_table <- function(mdb, table_name) {
        key_col <- paste0(table_name, "_id")
        if (table_name %in% mfdb3_taxonomy) {
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
        } else if (table_name %in% mfdb3_cs_taxonomy) {
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

    # Recreate fleet as a CS taxonomy
    if (mfdb_table_exists(mdb, 'fleet')) {
        mfdb_send(mdb, "DROP TABLE fleet")
        mfdb3_create_taxonomy_table(mdb, "fleet")
    }

    mfdb_send(mdb, "UPDATE mfdb_schema SET version = 3")
}

schema_from_3 <- function(mdb) {
    mdb$logger$info("Upgrading schema from version 3")
    mfdb_send(mdb, "ALTER TABLE prey ALTER COLUMN count DROP NOT NULL")

    mfdb4_taxonomy <- c("case_study", "institute", "gear", "vessel_type", "market_category", "sex", "maturity_stage", "species", "stomach_state", "digestion_stage")
    mfdb4_cs_taxonomy <- c("areacell", "sampling_type", "data_source", "index_type", "tow", "vessel")
    mfdb4_create_table <- function(mdb, name, desc, cols = c(), keys = c()) {
        items <- matrix(c(
            cols,
            unlist(lapply(keys, function (k) c(k, "", "")))
        ), nrow = 3)

        row_to_string <- function (i) {
            paste0("    ",
                items[1,i],
                (if (nzchar(items[2,i])) paste("\t", items[2,i])),
                (if (i == ncol(items)) "" else ","),
                (if (nzchar(items[3,i])) paste("\t--", items[3,i])),
                "\n")
        }

        mfdb_send(mdb,
            if (nzchar(desc)) paste0("-- ", desc, "\n", collapse = ""),
            "CREATE TABLE ", name, " (\n",
            vapply(1:ncol(items), row_to_string, ""),
            ")")
        if (nzchar(desc)) mfdb_send(mdb,
            "COMMENT ON TABLE ", name,
            " IS ", sql_quote(desc))
        for (i in 1:ncol(items)) {
            if (nzchar(items[3,i])) mfdb_send(mdb,
                "COMMENT ON COLUMN ", name, ".", items[1,i],
                " IS ", sql_quote(items[3,i]))
        }
    }
    fk4 <- function (...) {
        tbls <- c(...)[c(...) %in% mfdb4_taxonomy]
        cs_tbls <- c(...)[c(...) %in% mfdb4_cs_taxonomy]
        c(
            if (length(cs_tbls) > 0) paste0("FOREIGN KEY(case_study_id, ", cs_tbls, "_id) REFERENCES ", cs_tbls, "(case_study_id, ", cs_tbls, "_id)"),
            if (length(tbls) > 0) paste0("FOREIGN KEY(", tbls, "_id) REFERENCES ", tbls, "(", tbls, "_id)"),
            NULL
        )
    }
    mfdb4_create_taxonomy_table <- function(mdb, table_name) {
        key_col <- paste0(table_name, "_id")
        if (table_name %in% mfdb4_taxonomy) {
            mfdb4_create_table(mdb, table_name, "", cols = c(
                key_col, ifelse(table_name == "species", "BIGINT", "INT"), "Numeric ID for this entry",
                "name", "VARCHAR(1024) NOT NULL", "Short name used in data files / output data (in ltree notation)",
                "t_group", paste0("VARCHAR(1024) NULL"), "Value grouping (short name)",
                "description", "VARCHAR(1024)", "Long description",
                NULL
            ), keys = c(
                paste0(c("PRIMARY KEY(", key_col, ")"), collapse = ""),
                "CHECK(name ~ '^[A-Za-z0-9_.\\-]+$')",
                paste0("UNIQUE(name)"),
                paste0("FOREIGN KEY (t_group) REFERENCES ", table_name, "(name)"),
                NULL
            ))
        } else if (table_name %in% mfdb4_cs_taxonomy) {
            mfdb4_create_table(mdb, table_name, "", cols = c(
                "case_study_id", "INT REFERENCES case_study(case_study_id)", "Case study data is relevant to",
                key_col, ifelse(table_name == "data_source", "SERIAL", "INT"), "Numeric ID for this entry",
                "name", "VARCHAR(1024) NOT NULL", "Short name used in data files / output data (in ltree notation)",
                "t_group", paste0("VARCHAR(1024) NULL"), "Value grouping (short name)",
                if (table_name == "areacell") c(
                    "size", "INT", "Size of areacell",
                    NULL
                ) else if (table_name == "vessel") c(
                    "vessel_type_id", "INT", "Vessel type used",
                    "full_name", "TEXT", "Full name of vessel",
                    "length", "REAL", "Vessel length (m)",
                    "power", "REAL", "Vessel engine power (KW)",
                    "tonnage", "REAL", "Vessel gross tonnage",
                    NULL
                ) else if (table_name == "tow") c(
                    "latitude", "REAL", "Latutide of sample",
                    "longitude", "REAL", "Longitude of sample",
                    "depth", "REAL", "Tow depth (m)",
                    "length", "REAL", "Tow length (m)",
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
                paste0("FOREIGN KEY (case_study_id, t_group) REFERENCES ", table_name, "(case_study_id, name)"),
                NULL
            ))
        }
    }

    for (t in c(mfdb4_taxonomy, mfdb4_cs_taxonomy)) {
        if (t %in% c('tow', 'vessel_type')) {
            next
        }
        col_exists <- mfdb_fetch(mdb, "SELECT COUNT(*)",
            " FROM information_schema.columns",
            " WHERE table_schema = ", sql_quote(mdb$schema),
            " AND table_name = ", sql_quote(t),
            " AND column_name = 't_group'",
            NULL)
        if (col_exists > 0) {
            # Do nothing, already there
        } else if (t %in% setdiff(mfdb4_cs_taxonomy, 'vessel')) {
            mfdb_send(mdb, "ALTER TABLE ", t, " ADD COLUMN t_group VARCHAR(1024) NULL")
            mfdb_send(mdb, "ALTER TABLE ", t, " ADD FOREIGN KEY (case_study_id, t_group) REFERENCES ", t, "(case_study_id, name)")
        } else {
            mfdb_send(mdb, "ALTER TABLE ", t, " ADD COLUMN t_group VARCHAR(1024) NULL")
            mfdb_send(mdb, "ALTER TABLE ", t, " ADD FOREIGN KEY (t_group) REFERENCES ", t, "(name)")
        }
    }

    mfdb4_create_taxonomy_table(mdb, "tow")
    mfdb_send(mdb, "ALTER TABLE vessel RENAME TO vessel_type")
    mfdb_send(mdb, "ALTER TABLE vessel_type RENAME COLUMN vessel_id TO vessel_type_id")
    mfdb4_create_taxonomy_table(mdb, "vessel")
    # Create vessel for each vessel type so we can map data
    mfdb_insert(mdb, 'vessel', mfdb_fetch(mdb,
        "SELECT DISTINCT s.case_study_id",
        ", s.vessel_id AS vessel_id",
        ", v.name AS name",
        ", s.vessel_id AS vessel_type_id",
        " FROM sample s",
        " JOIN vessel_type v ON s.vessel_id = v.vessel_type_id"))

    for (t in c('sample', 'predator')) {
        mfdb_send(mdb, "ALTER TABLE ", t, " ADD COLUMN tow_id INT")
        mfdb_send(mdb, "ALTER TABLE ", t, " ADD ", fk4('tow'))

        mfdb_send(mdb, "ALTER TABLE ", t, " RENAME COLUMN vessel_id TO vessel_type_id")
        mfdb_send(mdb, "ALTER TABLE ", t, " ADD COLUMN vessel_id INT")
        mfdb_send(mdb, "ALTER TABLE ", t, " ADD ", fk4('vessel'))

        # Use new vessel_id column, drop old mapping
        mfdb_send(mdb, "UPDATE ", t, " SET vessel_id = vessel_type_id")
        mfdb_send(mdb, "ALTER TABLE ", t, " DROP COLUMN vessel_type_id")
    }

    # It's not actually being used
    if (mfdb_table_exists(mdb, 'fleet')) {
        mfdb_send(mdb, "DROP TABLE fleet")
    }

    mfdb_send(mdb, "UPDATE mfdb_schema SET version = 4")
}

schema_from_4 <- function(mdb) {
    stop("Cannot upgrade directly from 4-->5, need to recreate in separate schema")
}

schema_from_5 <- function(mdb) {
    mdb$logger$info("Upgrading schema from version 5")

    mfdb_send(mdb, "ALTER TABLE sample ALTER COLUMN age TYPE REAL")
    mfdb_send(mdb, "ALTER TABLE predator ALTER COLUMN age TYPE REAL")
    mfdb_send(mdb, "ALTER TABLE areacell ALTER COLUMN size TYPE REAL")

    mfdb_send(mdb, "UPDATE mfdb_schema SET version = 6")
}

schema_from_6 <- function(mdb) {
    mdb$logger$info("Schema up-to-date")
}

mfdb_taxonomy <- c("case_study", "institute", "gear", "vessel_type", "market_category", "sex", "maturity_stage", "species", "stomach_state", "digestion_stage")
mfdb_cs_taxonomy <- c("areacell", "sampling_type", "data_source", "index_type", "tow", "vessel")
mfdb_measurement_tables <- c('survey_index', 'division', 'sample', 'predator', 'prey')

mfdb_create_taxonomy_table <- function(mdb, table_name) {
    key_col <- paste0(table_name, "_id")
    if (table_name %in% mfdb_taxonomy) {
        mfdb_create_table(mdb, table_name, "", cols = c(
            key_col, ifelse(table_name == "species", "BIGINT", "INT"), "Numeric ID for this entry",
            "name", "VARCHAR(1024) NOT NULL", "Short name used in data files / output data (in ltree notation)",
            "t_group", paste0("VARCHAR(1024) NULL"), "Value grouping (short name)",
            "description", "VARCHAR(1024)", "Long description",
            NULL
        ), keys = c(
            paste0(c("PRIMARY KEY(", key_col, ")"), collapse = ""),
            "CHECK(name ~ '^[A-Za-z0-9_.\\-]+$')",
            paste0("UNIQUE(name)"),
            paste0("FOREIGN KEY (t_group) REFERENCES ", table_name, "(name)"),
            NULL
        ))
    } else if (table_name %in% mfdb_cs_taxonomy) {
        mfdb_create_table(mdb, table_name, "", cols = c(
            key_col, ifelse(table_name == "data_source", "SERIAL", "INT"), "Numeric ID for this entry",
            "name", "VARCHAR(1024) NOT NULL", "Short name used in data files / output data (in ltree notation)",
            "t_group", paste0("VARCHAR(1024) NULL"), "Value grouping (short name)",
            if (table_name == "areacell") c(
                "size", "REAL", "Size of areacell",
                NULL
            ) else if (table_name == "vessel") c(
                "vessel_type_id", "INT", "Vessel type used",
                "full_name", "TEXT", "Full name of vessel",
                "length", "REAL", "Vessel length (m)",
                "power", "REAL", "Vessel engine power (KW)",
                "tonnage", "REAL", "Vessel gross tonnage",
                NULL
            ) else if (table_name == "tow") c(
                "latitude", "REAL", "Latutide of sample",
                "longitude", "REAL", "Longitude of sample",
                "depth", "REAL", "Tow depth (m)",
                "length", "REAL", "Tow length (m)",
                NULL
            ) else c(
                "description", "VARCHAR(1024)", "Long description",
                NULL
            ),
            NULL
        ), keys = c(
            paste0(c("PRIMARY KEY(", key_col, ")"), collapse = ""),
            "CHECK(name ~ '^[A-Za-z0-9_.\\-]+$')",
            paste0("UNIQUE(name)"),
            paste0("FOREIGN KEY (t_group) REFERENCES ", table_name, "(name)"),
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

# Create any required functions, if they don't already exist
mfdb_update_functions <- function(mdb) {
   mfdb_create_aggregate(mdb, "WEIGHTED_MEAN",
       input_type = c("numeric", "numeric"), # value, weight
       state_type = "numeric[2]",
       init_cond = "{0,0}",  # Total, count
       accum_body = "$$ SELECT CASE WHEN $2 IS NULL THEN $1 ELSE ARRAY [
         $1[1] + $2 * $3,
         $1[2] + $3
       ] END $$ LANGUAGE 'sql'",
       final_body = "$$ SELECT CASE WHEN $1[2] = 0 THEN NULL ELSE $1[1] / $1[2] END $$ LANGUAGE 'sql'",
   )

    # See (2) in http://www.derivations.org/stdev.pdf
    mfdb_create_aggregate(mdb, "WEIGHTED_STDDEV",
       input_type = c("numeric", "numeric"), # value, weight
       state_type = "numeric[3]", # total, sum, sqsum
       init_cond = "{0,0,0,0}",
       accum_body = "$$ SELECT CASE WHEN $2 IS NULL THEN $1 ELSE ARRAY [
         $1[1] + $3,             -- total += weight
         $1[2] + $3 * $2,        -- sum += weight * value
         $1[3] + $3 * POW($2,2)  -- sqsum += weight * value**2
       ] END $$ LANGUAGE 'sql'",
       return_type = "double precision",
       final_body = "$$ SELECT CASE WHEN $1[1] < 2 THEN NULL ELSE |/ ( (1/($1[1] - 1)) * ($1[3] - POW($1[2], 2) / $1[1]) ) END $$ LANGUAGE 'sql'")
   mdb$logger$info("Creating indexes")
}

# Return the major version of the package
package_major_version <- function () gsub("\\..*", "", packageVersion("mfdb"))
