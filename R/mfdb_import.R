# Import a lookup table e.g. mfdb_import_taxonomy(mdb, "species", read.csv('species.csv'))
# new_data should have columns id, name, description
mfdb_import_taxonomy <- function (mdb, table_name, new_data, extra_cols = c('description')) {
    # Is table_name one of the recognised tables?
    if (!(table_name %in% mfdb_taxonomy | table_name %in% mfdb_cs_taxonomy)) {
        stop("Unknown taxonomy table ", table_name)
    }
    cs_specific <- (table_name %in% mfdb_cs_taxonomy)

    # Order incoming data by id
    new_data <- new_data[order(new_data$id),]

    # Fetch all existing ids, quit if all are there
    existing <- mfdb_fetch(mdb,
        "SELECT ", table_name, "_id id, name",
        " FROM ", table_name,
        if (cs_specific) c(" WHERE case_study_id = ", mdb$case_study_id) else "",
        " ORDER BY 1")

    if (nrow(existing) > 0 && is.logical(all.equal(new_data$id, existing$id))
                           && is.logical(all.equal(as.character(new_data$name), as.character(existing$name)))) {
        mdb$logger$debug(paste0("Taxonomy ", table_name ," up-to-date"))
        return()
    }

    # Either add or update rows. Removing is risky, since we might have dependent data.
    # Also don't want to remove data if partitioned by case study
    mfdb_transaction(mdb, {
        #TODO: Turn mfdb_insert into mfdb_upsert, use it
        vapply(seq_len(nrow(new_data)), function (i) {
            if (new_data[i, 'id'] %in% existing$id) { # NB: Got mushed to a string when row became a vector, probably bad
                mfdb_send(mdb,
                    "UPDATE ", table_name,
                    " SET name = ", sql_quote(new_data[i, 'name']),
                    vapply(extra_cols, function(r) paste0(",", r, " = ", sql_quote(new_data[i, r])), ""),
                    " WHERE ", table_name, "_id = ", sql_quote(new_data[i, 'id']),
                    if (cs_specific) c(" AND case_study_id = ", mdb$case_study_id) else "")
            } else {
                mfdb_send(mdb,
                    "INSERT INTO ", table_name,
                    " (", paste(c(
                        if (cs_specific) "case_study_id" else NULL,
                        paste0(table_name, "_id"),
                        "name",
                        extra_cols), collapse = ","), ") VALUES ",
                    sql_quote(c(
                        if (cs_specific) mdb$case_study_id else NULL,
                        new_data[i, c('id', 'name', extra_cols)])))
            }
            return(0)
        }, 0)
    })
}

mfdb_import_survey <- function (mdb, data_in, ...) {
    survey_metadata <- list(...)

    # Sanitise data
    survey_metadata <- list(
        data_source = sanitise_col(mdb, survey_metadata, 'data_source'),
        case_study_id = c(mdb$case_study_id),
        institute_id = sanitise_col(mdb, survey_metadata, 'institute', lookup = 'institute'),
        gear_id = sanitise_col(mdb, survey_metadata, 'gear', lookup = 'gear'),
        vessel_id = sanitise_col(mdb, survey_metadata, 'vessel', lookup = 'vessel'),
        sampling_type_id = sanitise_col(mdb, survey_metadata, 'sampling_type', lookup = 'sampling_type'))
    survey_sample <- data.frame(
        year = sanitise_col(mdb, data_in, 'year', default = c(survey_metadata$year)),
        month = sanitise_col(mdb, data_in, 'month'),
        areacell_id = sanitise_col(mdb, data_in, 'areacell', lookup = 'areacell'),
        species_id = sanitise_col(mdb, data_in, 'species', lookup = 'species', default = c(NA)),
        age = sanitise_col(mdb, data_in, 'age', default = c(NA)),
        sex_id = sanitise_col(mdb, data_in, 'sex', lookup = 'sex', default = c(NA)),
        length = sanitise_col(mdb, data_in, 'length', default = c(NA)),
        length_var = sanitise_col(mdb, data_in, 'length_var', default = c(NA)),
        length_min = sanitise_col(mdb, data_in, 'length_min', default = c(NA)),
        weight = sanitise_col(mdb, data_in, 'weight', default = c(NA)),
        weight_var = sanitise_col(mdb, data_in, 'weight_var', default = c(NA)),
        count = sanitise_col(mdb, data_in, 'count', default = c(1)))

    # Remove data_source and re-insert
    mfdb_transaction(mdb, {
        dbSendQuery(mdb$db, paste0("DELETE FROM sample WHERE ",
            " case_study_id IN ", sql_quote(mdb$case_study_id, always_bracket = TRUE),
            " AND survey_id = (SELECT survey_id FROM survey WHERE data_source = ", sql_quote(survey_metadata$data_source), ")",
            ""))
        dbSendQuery(mdb$db, paste0("DELETE FROM survey WHERE data_source = ", sql_quote(survey_metadata$data_source)))
        res <- mfdb_insert(mdb, 'survey', survey_metadata, returning = "survey_id")
        res <- mfdb_insert(mdb, 'sample', survey_sample, extra = c(survey_id = res$survey_id))
    })
}

# Import area data
mfdb_import_area <- function(mdb, data_in) {
    mfdb_import_taxonomy(mdb, 'areacell',
        data.frame(
            id = sanitise_col(mdb, data_in, 'id'),
            name = sanitise_col(mdb, data_in, 'name'),
            size = sanitise_col(mdb, data_in, 'size', default = c(NA))),
        extra_cols = c('size'))
}

# Import divisions
mfdb_import_division <- function (mdb, data_in) {
    if(!is.list(data_in)) {
        stop("data_in should be a list of areacell vectors")
    }
    mfdb_transaction(mdb, {
        dbSendQuery(mdb$db, paste0(
            "DELETE FROM division WHERE",
            " case_study_id IN ", sql_quote(mdb$case_study_id, always_bracket = TRUE),
            " AND division IN ", sql_quote(names(data_in), always_bracket = TRUE),
            ""))
        res <- mfdb_insert(mdb, 'division', data.frame(
            case_study_id = c(mdb$case_study_id),
            division = unlist(lapply(names(data_in), function(n) { rep(n, length(data_in[[n]])) })),
            areacell_id = sanitise_col(mdb, data.frame(areacell = unlist(data_in)), 'areacell', lookup = 'areacell')))
    })
}

# Import temperature data for entire region
mfdb_import_temperature <- function(mdb, data_in) {
    mfdb_transaction(mdb, {
        dbSendQuery(mdb$db, paste0(
            "DELETE FROM temperature WHERE",
            " case_study_id IN ", sql_quote(mdb$case_study_id, always_bracket = TRUE),
            ""))
        res <- mfdb_insert(mdb, 'temperature', data.frame(
            case_study_id = c(mdb$case_study_id),
            year = sanitise_col(mdb, data_in, 'year'),
            month = sanitise_col(mdb, data_in, 'month'),
            areacell_id = sanitise_col(mdb, data_in, 'areacell', lookup = 'areacell'),
            temperature = sanitise_col(mdb, data_in, 'temperature')))
    })
}

# Check column content, optionally resolving lookup
sanitise_col <- function (mdb, data_in, col_name, default = NULL, lookup = NULL) {
    col <- data_in[[col_name]]
    if (is.null(col)) {
        if (!is.null(default)) return(default);
        stop("Input data is missing ", col_name)
    }
    if (!is.null(lookup)) {
        col <- factor(col)
        # Fetch corresponding id for each level
        new_levels <- mfdb_fetch(mdb,
            "SELECT name, ", lookup, "_id FROM ", lookup, " AS id",
            " WHERE name IN ", sql_quote(levels(col), always_bracket = TRUE),
            if(lookup %in% mfdb_cs_taxonomy) c(" AND case_study_id = ", mdb$case_study_id))
        if(nrow(new_levels) == 0) {
            stop("None of the input data matches ", lookup, " vocabulary")
        }
        row.names(new_levels) <- new_levels$name

        new_levels <- new_levels[levels(col), paste0(lookup, '_id')]
        if (length(new_levels[is.na(new_levels)]) > 0) {
            # TODO: Decent error message
            stop("Data does not match ", lookup, " vocabulary")
        }

        # Return vector with proper levels
        col <- new_levels[as.numeric(col)]
    }
    return(col)
}
