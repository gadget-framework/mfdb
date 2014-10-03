# Import a lookup table e.g. mfdb_import_taxonomy(mdb, "species", read.csv('species.csv'))
# new_data should have columns id, name, description
mfdb_import_taxonomy <- function (mdb, table_name, new_data) {
    # Is table_name one of the recognised tables?
    if (!(table_name %in% c("institute", "fleet", "gear", "vessel", "market_category", "sampling_type", "sex", "species"))) {
        stop("Unknown taxonomy table ", table_name)
    }

    # Fetch all existing ids
    existing <- unlist(mfdb_fetch(mdb, "SELECT ", table_name, "_id FROM ", table_name))

    # Either add or update rows. Removing is risky, since we might have dependent data
    mfdb_transaction(mdb, {
        apply(new_data, 1, function (row) {
            if (as.numeric(row[['id']]) %in% existing) { # NB: Got mushed to a string when row became a vector, probably bad
                dbSendQuery(mdb$db, paste0(
                    "UPDATE ", table_name,
                    " SET name = ", sql_quote(row[['name']]),
                    ", description = ", sql_quote(row[['description']]),
                    " WHERE ", table_name, "_id = ", sql_quote(row[['id']])))
            } else {
                dbSendQuery(mdb$db, paste0(
                    "INSERT INTO ", table_name,
                    " (", table_name, "_id, name, description) VALUES ",
                    sql_quote(row[c('id', 'name', 'description')])))
            }
        })
    })
}

mfdb_import_survey <- function (mdb, data_in, ...) {
    survey_metadata <- list(...)
    sanitise_col <- function (data_in, col_name, default = NULL, lookup = NULL) {
        col <- data_in[[col_name]]
        if (is.null(col)) {
            if (!is.null(default)) return(default);
            stop("Input data is missing ", col_name)
        }
        if (!is.null(lookup)) {
            col <- factor(col)
            # Fetch corresponding id for each level
            new_levels <- mfdb_fetch(mdb,
                "SELECT name, ", lookup, "id FROM ", lookup, " AS id",
                "WHERE name IN ", sql_quote(levels(col)))
            row.names(new_levels) <- new_levels$name
            new_levels <- new_levels[levels(col),]$id
            if (length(new_levels[is.na(new_levels)]) > 0) {
                # TODO: Decent error message
                stop("Data does not match vocabulary")
            }

            # Return vector with proper levels
            levels(col) <- new_levels
        }
        return(col)
    }

    # Sanitise data
    survey_metadata <- list(
        data_source = sanitise_col(survey_metadata, 'data_source'),
        institute_id = sanitise_col(survey_metadata, 'institute', lookup = 'institute'),
        fleet_id = sanitise_col(survey_metadata, 'fleet', lookup = 'fleet'),
        gear_id = sanitise_col(survey_metadata, 'gear', lookup = 'gear'),
        vessel_id = sanitise_col(survey_metadata, 'vessel', lookup = 'vessel'),
        market_category_id = sanitise_col(survey_metadata, 'market_category', lookup = 'market_category'),
        sampling_type_id = sanitise_col(survey_metadata, 'sampling_type', lookup = 'sampling_type'))
    survey_sample <- data.frame(
        year = sanitise_col(data_in, 'year', default = c(survey_metadata$year)),
        month = sanitise_col(data_in, 'month'),
        areacell = sanitise_col(data_in, 'areacell'),
        species_id = sanitise_col(data_in, 'species', lookup = 'species', default = c(NA)),
        age = sanitise_col(data_in, 'age', default = c(NA)),
        sex_id = sanitise_col(data_in, 'sex', lookup = 'sex', default = c(NA)),
        length = sanitise_col(data_in, 'length', default = c(NA)),
        weight = sanitise_col(data_in, 'weight', default = c(NA)),
        count = sanitise_col(data_in, 'weight', default = c(1)))

    # Remove data_source and re-insert
    mfdb_transaction(mdb, {
        dbSendQuery(mdb$db, "DELETE FROM survey WHERE data_source = ", sql_quote(survey_metadata$data_source), " CASCADE")
        res <- mfdb_insert(mdb, 'survey', survey_metadata, returning = "survey_id")
        res <- mfdb_insert(mdb, 'sample', survey_sample, extra = res$survey_id)
    })
}

mfdb_import_areas <- function (mfdb) {
}
