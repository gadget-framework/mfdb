# Import a lookup table e.g. mfdb_import_taxonomy(mdb, "species", read.csv('species.csv'))
# data_in should have columns id, name, description
mfdb_import_taxonomy <- function (mdb, table_name, data_in, extra_cols = c('description')) {
    # Is table_name one of the recognised tables?
    if (!(table_name %in% mfdb_taxonomy | table_name %in% mfdb_cs_taxonomy)) {
        stop("Unknown taxonomy table ", table_name)
    }
    cs_specific <- (table_name %in% mfdb_cs_taxonomy)

    # Check there's something to do first
    if (nrow(data_in) == 0) {
        mdb$logger$info(paste0("Taxonomy ", table_name ," no updates to make"))
        return()
    }

    # Order incoming data by id
    id_col <- paste0(table_name, '_id')
    data_in <- data_in[c('id', 'name', extra_cols)]
    names(data_in) <- c(id_col, 'name', extra_cols)

    # Crush factors in data.frame, convert integer names to character
    for (n in names(data_in)) {
        if (n == "name" || is.factor(data_in[[n]])) {
            data_in[[n]] <- as.character(data_in[[n]])
        }
    }

    # Fetch all existing ids, quit if all are there
    existing <- mfdb_fetch(mdb,
        "SELECT ", id_col, ", name, ", paste(extra_cols, collapse = ", "),
        " FROM ", table_name,
        if (cs_specific) c(" WHERE case_study_id = ", mdb$case_study_id) else "",
        " ORDER BY 1")

    # Throw away rows which don't need updating
    if (nrow(existing) > 0) {
        data_in <- data_in[!(data_in$name %in% merge(
            existing[, c('name', extra_cols)],
            data_in[,  c('name', extra_cols)])$name), ]
        if (nrow(data_in) == 0) {
            mdb$logger$info(paste0("Taxonomy ", table_name ," up-to-date"))
            return()
        }
    }
    mdb$logger$info(paste0("Taxonomy ", table_name ," needs updating"))

    mfdb_transaction(mdb, {
        # New rows should be inserted
        # NB: We don't handle the case of new data having the same ID as existing data.
        #     In practice this probably won't happen, but a reasonable solution would be nice.
        mfdb_insert(mdb,
            table_name,
            data_in[data_in$name == setdiff(data_in$name, existing$name), ],
            extra = (if (cs_specific) c(case_study_id = mdb$case_study_id) else c()))

        # Rows with matching names should be updated, but existing ids kept
        if (nrow(existing) > 0) mfdb_update(mdb,
            table_name,
            merge(existing[, c(id_col, 'name')], data_in[, c('name', extra_cols)]),
            where = if (cs_specific) list(case_study_id = mdb$case_study_id) else c())
    })

    invisible(NULL)
}

# Import any cs_specific taxonomies
mfdb_import_cs_taxonomy <- function(mdb, taxonomy_name, data_in) {
    if (!(taxonomy_name %in% mfdb_cs_taxonomy)) {
        stop(
            "Unknown taxonomy name '", taxonomy_name,
            "' should be one of ", paste(mfdb_cs_taxonomy, collapse = ", "))
    }
    mfdb_import_taxonomy(mdb, taxonomy_name,
        data.frame(
            id = sanitise_col(mdb, data_in, 'id', default = seq_len(length(data_in$name))),
            name = sanitise_col(mdb, data_in, 'name'),
            description = sanitise_col(mdb, data_in, 'description', default = c("")),
            size = sanitise_col(mdb, data_in, 'size', default = c(NA))),
        extra_cols = if (taxonomy_name == 'areacell') c('size') else c('description'))
    invisible(NULL)
}
mfdb_import_area <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'areacell', data_in)
mfdb_import_sampling_type <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'sampling_type', data_in)

mfdb_import_survey <- function (mdb, data_in, data_source = 'default_sample') {
    # Sanitise data
    survey_sample <- data.frame(
        case_study_id = c(mdb$case_study_id),
        institute_id = sanitise_col(mdb, data_in, 'institute', lookup = 'institute', default = c(NA)),
        gear_id = sanitise_col(mdb, data_in, 'gear', lookup = 'gear', default = c(NA)),
        vessel_id = sanitise_col(mdb, data_in, 'vessel', lookup = 'vessel', default = c(NA)),
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
        weight = sanitise_col(mdb, data_in, 'weight', default = c(NA)),
        weight_var = sanitise_col(mdb, data_in, 'weight_var', default = c(NA)),
        count = sanitise_col(mdb, data_in, 'count', default = c(1)))


    # Likely to be pretty big, so pre-load data into a temporary table
    temp_tbl <- mfdb_bulk_copy(mdb, 'sample', survey_sample, function (temp_tbl) mfdb_disable_constraints(mdb, 'sample', mfdb_transaction(mdb, {
        # Remove data_source and re-insert
        data_source_id <- get_data_source_id(mdb, data_source)
        mfdb_send(mdb, "DELETE FROM sample",
            " WHERE case_study_id = ", sql_quote(mdb$case_study_id),
            " AND data_source_id = ", sql_quote(data_source_id),
            NULL)
        mfdb_send(mdb,
            "INSERT INTO sample",
            " (", paste(names(survey_sample), collapse=","), ", data_source_id)",
            " SELECT ", paste(names(survey_sample), collapse=","), ", ", sql_quote(data_source_id),
            " FROM ", temp_tbl,
            NULL)
    })))
}

mfdb_import_survey_index <- function (mdb, data_in, data_source = 'default_index') {
    data_in <- data.frame(
        case_study_id = c(mdb$case_study_id),
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
        mfdb_send(mdb,
            "INSERT INTO survey_index",
            " (", paste(names(data_in), collapse=","), ", data_source_id)",
            " SELECT ", paste(names(data_in), collapse=","), ", ", sql_quote(data_source_id),
            " FROM ", temp_tbl)
    }))
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
    data_in$value <- data_in$temperature
    data_in$index_type = 'temperature'
    mfdb_import_survey_index(mdb, data_in, data_source = "default_temperature")
}

# Import 2 data frames, one for predators, one for prey
mfdb_import_stomach <- function(mdb, predator_data, prey_data, data_source = "default_stomach") {
    predator_data <- data.frame(
        case_study_id = c(mdb$case_study_id),

        institute_id = sanitise_col(mdb, predator_data, 'institute', lookup = 'institute', default = c(NA)),
        gear_id = sanitise_col(mdb, predator_data, 'gear', lookup = 'gear', default = c(NA)),
        vessel_id = sanitise_col(mdb, predator_data, 'vessel', lookup = 'vessel', default = c(NA)),
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

    temp_tbl <- mfdb_bulk_copy(mdb, 'predator', predator_data, function (temp_predator) mfdb_transaction(mdb, {
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

        # Insert predator data, returning all IDs
        res <- mfdb_fetch(mdb,
            "INSERT INTO predator",
            " (", paste(names(predator_data), collapse=","), ", data_source_id)",
            " SELECT ", paste(names(predator_data), collapse=","), ", ", sql_quote(data_source_id),
            " FROM ", temp_predator,
            " RETURNING predator_id",
            NULL)

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
        res <- mfdb_insert(mdb, 'prey', prey_data)
    }))
}

# Check column content, optionally resolving lookup
sanitise_col <- function (mdb, data_in, col_name, default = NULL, lookup = NULL, test = NULL) {
    data_col_name <- grep(paste0('^', col_name, '$'), names(data_in), ignore.case=TRUE, value=TRUE)
    if (length(data_col_name) == 0) {
        if (!is.null(default)) return(default);
        stop("Input data is missing ", col_name)
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
