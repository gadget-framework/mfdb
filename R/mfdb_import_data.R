# Import temperature data for entire region
mfdb_import_temperature <- function(mdb, data_in) {
    data_in$value <- data_in$temperature
    data_in$index_type = 'temperature'
    mfdb_import_survey_index(mdb, data_in, data_source = "default_temperature")
}

mfdb_import_survey <- function (mdb, data_in, data_source = 'default_sample') {
    # Work out whether we're importing mean weights or total weights
    if ("weight_total" %in% colnames(data_in) && "weight" %in% colnames(data_in)) {
        stop("Cannot specify both weight and weight_total")
    } else if ("weight_total" %in% colnames(data_in) && "count" %in% colnames(data_in)) {
        # Convert total to a mean for storage
        data_in$weight <- data_in$weight_total / data_in$count
        weight_col <- "weight"
        count_default <- c(1)
    } else if ("weight_total" %in% colnames(data_in)) {
        weight_col <- "weight_total"
        count_default <- c(NA)
    } else {
        weight_col <- "weight"
        count_default <- c(1)
    }

    # Sanitise data
    survey_sample <- data.frame(
        case_study_id = if (nrow(data_in) > 0) c(mdb$case_study_id) else c(),
        institute_id = sanitise_col(mdb, data_in, 'institute', lookup = 'institute', default = c(NA)),
        gear_id = sanitise_col(mdb, data_in, 'gear', lookup = 'gear', default = c(NA)),
        vessel_id = sanitise_col(mdb, data_in, 'vessel', lookup = 'vessel', default = c(NA)),
        tow_id = sanitise_col(mdb, data_in, 'tow', lookup = 'tow', default = c(NA)),
        sampling_type_id = sanitise_col(mdb, data_in, 'sampling_type', lookup = 'sampling_type', default = c(NA)),
        year = sanitise_col(mdb, data_in, 'year'),
        month = sanitise_col(mdb, data_in, 'month', test = function (x) x %in% 1:12),
        areacell_id = sanitise_col(mdb, data_in, 'areacell', lookup = 'areacell'),
        species_id = sanitise_col(mdb, data_in, 'species', lookup = 'species', default = c(NA)),
        age = sanitise_col(mdb, data_in, 'age', default = c(NA)),
        sex_id = sanitise_col(mdb, data_in, 'sex', lookup = 'sex', default = c(NA)),
        maturity_stage_id = sanitise_col(mdb, data_in, 'maturity_stage', lookup = 'maturity_stage', default = c(NA)),
        length = sanitise_col(mdb, data_in, 'length', default = c(NA)),
        length_var = sanitise_col(mdb, data_in, 'length_var', default = c(NA)),
        length_min = sanitise_col(mdb, data_in, 'length_min', default = c(NA)),
        weight = sanitise_col(mdb, data_in, weight_col, default = c(NA)),
        weight_var = sanitise_col(mdb, data_in, 'weight_var', default = c(NA)),
        count = sanitise_col(mdb, data_in, 'count', default = count_default))


    # Likely to be pretty big, so pre-load data into a temporary table
    temp_tbl <- mfdb_bulk_copy(mdb, 'sample', survey_sample, function (temp_tbl) mfdb_disable_constraints(mdb, 'sample', mfdb_transaction(mdb, {
        # Remove data_source and re-insert
        data_source_id <- get_data_source_id(mdb, data_source)
        mfdb_send(mdb, "DELETE FROM sample",
            " WHERE case_study_id = ", sql_quote(mdb$case_study_id),
            " AND data_source_id = ", sql_quote(data_source_id),
            NULL)
        if (nrow(survey_sample) > 0) mfdb_send(mdb,
            "INSERT INTO sample",
            " (", paste(names(survey_sample), collapse=","), ", data_source_id)",
            " SELECT ", paste(names(survey_sample), collapse=","), ", ", sql_quote(data_source_id),
            " FROM ", temp_tbl,
            NULL)
    })))
}

mfdb_import_survey_index <- function (mdb, data_in, data_source = 'default_index') {
    data_in <- data.frame(
        case_study_id = if (nrow(data_in) > 0) c(mdb$case_study_id) else c(),
        areacell_id = sanitise_col(mdb, data_in, 'areacell', lookup = 'areacell'),
        index_type_id = sanitise_col(mdb, data_in, 'index_type', lookup = 'index_type'),
        year = sanitise_col(mdb, data_in, 'year'),
        month = sanitise_col(mdb, data_in, 'month'),
        value = sanitise_col(mdb, data_in, 'value'))

    temp_tbl <- mfdb_bulk_copy(mdb, 'survey_index', data_in, function (temp_tbl) mfdb_transaction(mdb, {
        # Remove data_source and re-insert
        data_source_id <- get_data_source_id(mdb, data_source)
        mfdb_send(mdb, "DELETE FROM survey_index",
            " WHERE case_study_id = ", sql_quote(mdb$case_study_id),
            " AND data_source_id = ", sql_quote(data_source_id),
            NULL)
        if (nrow(data_in) > 0) mfdb_send(mdb,
            "INSERT INTO survey_index",
            " (", paste(names(data_in), collapse=","), ", data_source_id)",
            " SELECT ", paste(names(data_in), collapse=","), ", ", sql_quote(data_source_id),
            " FROM ", temp_tbl)
    }))
}

# Import 2 data frames, one for predators, one for prey
mfdb_import_stomach <- function(mdb, predator_data, prey_data, data_source = "default_stomach") {
    predator_data <- data.frame(
        case_study_id = if (nrow(predator_data) > 0) c(mdb$case_study_id) else c(),

        institute_id = sanitise_col(mdb, predator_data, 'institute', lookup = 'institute', default = c(NA)),
        gear_id = sanitise_col(mdb, predator_data, 'gear', lookup = 'gear', default = c(NA)),
        vessel_id = sanitise_col(mdb, predator_data, 'vessel', lookup = 'vessel', default = c(NA)),
        tow_id = sanitise_col(mdb, predator_data, 'tow', lookup = 'tow', default = c(NA)),
        sampling_type_id = sanitise_col(mdb, predator_data, 'sampling_type', lookup = 'sampling_type', default = c(NA)),

        year = sanitise_col(mdb, predator_data, 'year'),
        month = sanitise_col(mdb, predator_data, 'month', test = function (x) x %in% 1:12),
        areacell_id = sanitise_col(mdb, predator_data, 'areacell', lookup = 'areacell'),

        stomach_name = sanitise_col(mdb, predator_data, 'stomach_name'),
        species_id = sanitise_col(mdb, predator_data, 'species', lookup = 'species', default = c(NA)),
        age = sanitise_col(mdb, predator_data, 'age', default = c(NA)),
        sex_id = sanitise_col(mdb, predator_data, 'sex', lookup = 'sex', default = c(NA)),
        maturity_stage_id = sanitise_col(mdb, predator_data, 'maturity_stage', lookup = 'maturity_stage', default = c(NA)),
        stomach_state_id = sanitise_col(mdb, predator_data, 'stomach_state', lookup = 'stomach_state', default = c(NA)),

        length = sanitise_col(mdb, predator_data, 'length', default = c(NA)),
        weight = sanitise_col(mdb, predator_data, 'weight', default = c(NA)),
        stringsAsFactors = TRUE)
    prey_data <- data.frame(
        predator_id = as.factor(sanitise_col(mdb, prey_data, 'stomach_name')),
        species_id = sanitise_col(mdb, prey_data, 'species', lookup = 'species', default = c(NA)),
        digestion_stage_id = sanitise_col(mdb, prey_data, 'digestion_stage', lookup = 'digestion_stage', default = c(NA)),

        length = sanitise_col(mdb, prey_data, 'length', default = c(NA)),
        weight = sanitise_col(mdb, prey_data, 'weight', default = c(NA)),
        count = sanitise_col(mdb, prey_data, 'count', default = c(1)),
        stringsAsFactors = TRUE)

    # NB: Postgresql has transactional DDL, so this is fine.
    mfdb_transaction(mdb, mfdb_disable_constraints(mdb, 'prey', mfdb_disable_constraints(mdb, 'predator', {
        data_source_id <- get_data_source_id(mdb, data_source)

        # Delete everything with matching data_source
        mfdb_send(mdb, "DELETE FROM prey WHERE predator_id IN",
            "(SELECT predator_id FROM predator",
            " WHERE case_study_id = ", sql_quote(mdb$case_study_id),
            " AND data_source_id = ", sql_quote(data_source_id),
            ")",
            NULL)
        mfdb_send(mdb, "DELETE FROM predator",
            " WHERE case_study_id = ", sql_quote(mdb$case_study_id),
            " AND data_source_id = ", sql_quote(data_source_id),
            NULL)

        # If there's no data, leave at this point
        if (nrow(predator_data) == 0 || nrow(prey_data) == 0) return()

        # Insert predator data, returning all IDs
        res <- mfdb_bulk_copy(mdb, 'predator', predator_data, function (temp_predator) {
            mfdb_fetch(mdb,
                "INSERT INTO predator",
                " (", paste(names(predator_data), collapse=","), ", data_source_id)",
                " SELECT ", paste(names(predator_data), collapse=","), ", ", sql_quote(data_source_id),
                " FROM ", temp_predator,
                " RETURNING predator_id",
                NULL)
        })

        # Map predator names to database IDs
        new_levels <- structure(
            res$predator_id,
            names = as.character(predator_data$stomach_name))[levels(prey_data$predator_id)]
        if (any(is.na(new_levels))) {
            stop("Prey data mentions stomachs not in predator data: ",
                paste(levels(prey_data$predator_id)[is.na(new_levels)], collapse = ","))
        }
        levels(prey_data$predator_id) <- new_levels

        # Insert prey data
        mfdb_bulk_copy(mdb, 'prey', prey_data, function (temp_prey) {
            mfdb_send(mdb,
                "INSERT INTO prey",
                " (", paste(names(prey_data), collapse=","), ")",
                " SELECT ", paste(names(prey_data), collapse=","),
                " FROM ", temp_prey,
                NULL)
        })
    })))
}

# Check column content, optionally resolving lookup
sanitise_col <- function (mdb, data_in, col_name, default = NULL, lookup = NULL, test = NULL) {
    if (nrow(data_in) == 0) {
        # No data of any form, so return nothing
        return (c())
    }

    data_col_name <- grep(paste0('^', col_name, '$'), names(data_in), ignore.case=TRUE, value=TRUE)
    if (length(data_col_name) == 0) {
        if (!is.null(default)) return(default);
        stop("Input data is missing '", col_name, "'. Columns available: ", paste(names(data_in), collapse = ","))
    }
    col <- data_in[,data_col_name[[1]]]

    if (!is.null(test)) {
        mismatches <- test(col)
        if (!all(mismatches)) {
            mismatches <- col[!mismatches]
            stop(
                paste0("Column ", data_col_name[[1]], " has items that don't match condition: "),
                paste(head(mismatches, n = 50), collapse = ","),
                ifelse(length(mismatches) > 50, ', ...', ''),
                NULL)
        }
    }

    if (!is.null(lookup)) {
        col <- factor(col)

        # No levels means column is just NA.
        if (length(levels(col)) == 0) {
            return(c(NA))
        }

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
        if (any(is.na(new_levels))) {
            mismatches <- levels(col)[is.na(new_levels)]
            stop("Input data has items that don't match ", lookup, " vocabulary: ",
                paste(head(mismatches, n = 50), collapse = ","),
                ifelse(length(mismatches) > 50, ', ...', ''),
                NULL)
        }

        # Return vector with proper levels
        col <- new_levels[as.numeric(col)]
    }
    return(col)
}

# Fetch / create a data source ID.
get_data_source_id <- function(mdb, data_source) {
    if (length(data_source) > 1) {
        stop("data_source should not be a vector with more than 1 element")
    }
    res <- mfdb_fetch(mdb, "SELECT data_source_id FROM data_source",
        " WHERE case_study_id IN ", sql_quote(mdb$case_study_id, always_bracket = TRUE),
        " AND name = ", sql_quote(data_source),
        NULL)
    if (nrow(res) > 0) {
        return(res[1,1])
    }

    # Doesn't exist yet, create
    res <- mfdb_insert(mdb, 'data_source', c(
        case_study_id = mdb$case_study_id,
        name = data_source,
        NULL), returning = "data_source_id")
    return(res$data_source_id)
}
