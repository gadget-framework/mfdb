# Create MFDB schema from scratch, or print commands
schema_from_0 <- function(mdb) {
    mdb$logger$info("Creating schema from scratch")
    schema_create_tables(mdb)

    # Populate tables with package-provided data
    for (t in mfdb_taxonomy_tables) {
        if (exists(t, where = as.environment("package:mfdb"))) {
            mfdb_import_taxonomy(mdb, t, get(t, pos = as.environment("package:mfdb")))
        }
    }
    mfdb_import_cs_taxonomy(mdb, "index_type", data.frame(
        id = c(99999),
        name = 'temperature',
        stringsAsFactors = FALSE))
}

# Functions that upgrade an existing schema version x to x+1
schema_from_2 <- function(mdb) mfdb_transaction(mdb, {
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
})

schema_from_3 <- function(mdb) mfdb_transaction(mdb, {
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
})

schema_from_4 <- function(mdb) {
    stop("Cannot upgrade directly from 4-->5, need to recreate in separate schema")
}

schema_from_5 <- function(mdb) mfdb_transaction(mdb, {
    mdb$logger$info("Upgrading schema from version 5")

    mfdb_send(mdb, "ALTER TABLE sample ALTER COLUMN age TYPE REAL")
    mfdb_send(mdb, "ALTER TABLE predator ALTER COLUMN age TYPE REAL")
    mfdb_send(mdb, "ALTER TABLE areacell ALTER COLUMN size TYPE REAL")
    mfdb_send(mdb, "ALTER TABLE tow ADD COLUMN end_latitude REAL")
    mfdb_send(mdb, "ALTER TABLE tow ADD COLUMN end_longitude REAL")
    mfdb_send(mdb, "ALTER TABLE tow ADD COLUMN start TIMESTAMP WITH TIME ZONE")
    mfdb_send(mdb, "ALTER TABLE tow ADD COLUMN duration REAL")

    mfdb_send(mdb, "ALTER TABLE gear ADD COLUMN mesh_size REAL")
    mfdb_send(mdb, "ALTER TABLE gear ADD COLUMN mesh_size_min REAL")
    mfdb_send(mdb, "ALTER TABLE gear ADD COLUMN mesh_size_max REAL")

    mfdb_send(mdb, "UPDATE mfdb_schema SET version = 6")
})

schema_from_6 <- function(mdb) {
    mdb$logger$info("Upgrading schema from version 6")

    mfdb_send(mdb, "ALTER TABLE sample ALTER COLUMN age TYPE NUMERIC(10,5)")
    mfdb_send(mdb, "ALTER TABLE predator ALTER COLUMN age TYPE NUMERIC(10,5)")

    # Add port & trip taxonomies
    for (t in c(
        'port',
        'trip',
        NULL)) mfdb_create_taxonomy_table(mdb, t)
    mfdb_send(mdb, "ALTER TABLE sample ADD COLUMN trip_id INT REFERENCES trip(trip_id)")
    mfdb_send(mdb, "ALTER TABLE predator ADD COLUMN trip_id INT REFERENCES trip(trip_id)")

    # Add population taxonomy
    for (t in c('population')) mfdb_create_taxonomy_table(mdb, t)
    mfdb_send(mdb, "ALTER TABLE sample ADD COLUMN population_id INT REFERENCES population(population_id)")

    # Add vessel_owner taxonomy
    for (t in c('vessel_owner')) mfdb_create_taxonomy_table(mdb, t)
    mfdb_send(mdb, "ALTER TABLE vessel ADD COLUMN vessel_owner_id INT REFERENCES vessel_owner(vessel_owner_id)")

    # Add liver/gonad/stomach weight columns as well as overall weight
    mfdb_send(mdb, "ALTER TABLE sample ADD COLUMN liver_weight REAL")
    mfdb_send(mdb, "ALTER TABLE sample ADD COLUMN liver_weight_var REAL")
    mfdb_send(mdb, "ALTER TABLE sample ADD COLUMN gonad_weight REAL")
    mfdb_send(mdb, "ALTER TABLE sample ADD COLUMN gonad_weight_var REAL")
    mfdb_send(mdb, "ALTER TABLE sample ADD COLUMN stomach_weight REAL")
    mfdb_send(mdb, "ALTER TABLE sample ADD COLUMN stomach_weight_var REAL")
    mfdb_send(mdb, "ALTER TABLE sample ADD COLUMN gutted_weight REAL")
    mfdb_send(mdb, "ALTER TABLE sample ADD COLUMN gutted_weight_var REAL")

    mfdb_send(mdb, "UPDATE mfdb_schema SET version = 7")
}

schema_from_7 <- function(mdb) {
    mdb$logger$info("Schema up-to-date")
}

