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

# Perform query and return all results
mfdb_fetch <- function(mfdb, ...) {
    res <- dbSendQuery(mfdb$db, paste0(..., collapse = ""))
    out <- dbFetch(res)
    dbClearResult(res)
    return(out)
}

# Insert a vector row or data.frame of rows into table_name
mfdb_insert <- function(mfdb, table_name, data_in, returning = "", extra = c()) {
    insert_row <- function (r) {
        res <- dbSendQuery(mfdb$db, paste0("INSERT INTO ", paste(table_name, collapse = ""),
            " (", paste(c(names(r), names(extra)), collapse=","), ") VALUES ",
            sql_quote(c(r, extra)),
            (if (nzchar(returning)) paste0(c(" RETURNING ", returning), collapse = "") else ""),
            "", collapse = ""))
        out <- if (nzchar(returning)) dbFetch(res) else dbGetRowsAffected(res)
        dbClearResult(res)
        return(out)
    }
    if (!is.data.frame(data_in)) {
        # Insert single row
        return(insert_row(data_in))
    } else {
        # Insert rows
        #TODO: Should be batching
        return(vapply(seq_len(nrow(data_in)), function (i) { insert_row(data_in[i,]) }, 0))
    }
}

# Execute code block within a DB transaction, roll back on error, commit otherwise
mfdb_transaction <- function(mdb, transaction) {
    mdb$logger$info("Starting transaction...")
    dbSendQuery(mdb$db, "BEGIN TRANSACTION")
    ret <- tryCatch(transaction, error = function (e) { e })
    if ("error" %in% class(ret)) {
        mdb$logger$warn("Rolling back transaction...")
        dbRollback(mdb$db)
        stop(ret)
    }
    mdb$logger$info("Committing transaction...")
    dbCommit(mdb$db)
    invisible(TRUE)
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

    create_taxonomy <- function(name, desc, id_type = "INT") {
        send_query(mdb, sql_create_table(
            name, desc,
            paste0(name, "_id ", id_type," PRIMARY KEY"), "Numeric ID for this entry",
            "name VARCHAR(1024) NOT NULL", "Short name used in data files / output data",
            "CHECK(name ~ '^[A-Za-z0-9_.]+$')", "Labels should be in ltree notation",
            "description VARCHAR(1024)", "Long description",
            "UNIQUE(name)", ""))
    }

    create_taxonomy("institute", "")
    create_taxonomy("gear", "")
    create_taxonomy("vessel", "")
    create_taxonomy("sampling_type", "")
    send_query(mdb, sql_create_table(
        "survey", "Description of survey",
        "survey_id SERIAL PRIMARY KEY", "",
        "data_source VARCHAR(1024) NOT NULL", "Name of file/URL data came from",
        "UNIQUE(data_source)", "",
        "institute_id INT REFERENCES institute(institute_id)", "Institute that undertook survey",
        "gear_id INT REFERENCES gear(gear_id)", "Gear used",
        "vessel_id INT REFERENCES vessel(vessel_id)", "Vessel used",
        "sampling_type_id INT REFERENCES sampling_type(sampling_type_id)", "Sampling type"))

    # TODO: Should we have a numeric ID for areacell?
    send_query(mdb, sql_create_table(
        "area", "Mapping of areacells to divisions",
        "area_id SERIAL PRIMARY KEY", "",
        "data_source VARCHAR(1024) NOT NULL", "Name of file/URL data came from",
        "division VARCHAR(10) NOT NULL", "",
        "areacell VARCHAR(10) NOT NULL", "e.g. ICES gridcell"))

    create_taxonomy("sex", "")
    create_taxonomy("species", "", id_type = "BIGINT")
    send_query(mdb, sql_create_table(
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
        "weight REAL", "Weight of fish / mean weight of all fish",
        "count INT NOT NULL DEFAULT 1", "Number of fish meeting this criteria"))
}

# Populate with package-provided data
mfdb_update_taxonomies <- function(mdb) {
    mfdb_import_taxonomy(mdb, "institute", institute)
    mfdb_import_taxonomy(mdb, "gear", gear)
    mfdb_import_taxonomy(mdb, "vessel", vessel)
    mfdb_import_taxonomy(mdb, "sampling_type", sampling_type)

    mfdb_import_taxonomy(mdb, "sex", sex)
    mfdb_import_taxonomy(mdb, "species", data.frame(
        id = species$id,
        name = species$name,
        description = paste0(species$common_name, " (", species$scientific_name, ")")
    ))
}

# Return the major version of the package
package_major_version <- function () gsub("\\..*", "", packageVersion("mfdb"))
