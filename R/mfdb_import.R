# Import a lookup table e.g. mfdb_import_taxonomy(mdb, "species", read.csv('species.csv'))
# data_in should have columns id, name, description
mfdb_import_taxonomy <- function (mdb, table_name, data_in, extra_cols = c('description')) {
    # Is table_name one of the recognised tables?
    if (!(table_name %in% mfdb_taxonomy | table_name %in% mfdb_cs_taxonomy)) {
        stop("Unknown taxonomy table ", table_name)
    }
    cs_specific <- (table_name %in% mfdb_cs_taxonomy)

    # Order incoming data by id
    id_col <- paste0(table_name, '_id')
    data_in <- data_in[order(data_in$id), c('id', 'name', extra_cols)]
    names(data_in) <- c(id_col, 'name', extra_cols)

    # Fetch all existing ids, quit if all are there
    existing <- mfdb_fetch(mdb,
        "SELECT ", table_name, "_id id, name",
        " FROM ", table_name,
        if (cs_specific) c(" WHERE case_study_id = ", mdb$case_study_id) else "",
        " ORDER BY 1")

    if (nrow(existing) > 0 && is.logical(all.equal(data_in[[id_col]], existing$id))
                           && is.logical(all.equal(as.character(data_in$name), as.character(existing$name)))) {
        mdb$logger$debug(paste0("Taxonomy ", table_name ," up-to-date"))
        return()
    }

    # Either add or update rows. Removing is risky, since we might have dependent data.
    # Also don't want to remove data if partitioned by case study
    mfdb_transaction(mdb, {
        mfdb_insert(mdb,
            table_name,
            data_in[!(data_in[[id_col]] %in% existing$id),],
            extra = (if (cs_specific) c(case_study_id = mdb$case_study_id) else c()))
        mfdb_update(mdb,
            table_name,
            data_in[data_in[[id_col]] %in% existing$id,],
            where = if (cs_specific) list(case_study_id = mdb$case_study_id) else c())
    })
}

mfdb_import_survey <- function (mdb, data_in, data_source = 'default_sample') {
    # Sanitise data
    survey_sample <- data.frame(
        case_study_id = c(mdb$case_study_id),
        institute_id = sanitise_col(mdb, data_in, 'institute', lookup = 'institute', default = c(NA)),
        gear_id = sanitise_col(mdb, data_in, 'gear', lookup = 'gear', default = c(NA)),
        vessel_id = sanitise_col(mdb, data_in, 'vessel', lookup = 'vessel', default = c(NA)),
        sampling_type_id = sanitise_col(mdb, data_in, 'sampling_type', lookup = 'sampling_type', default = c(NA)),
        year = sanitise_col(mdb, data_in, 'year'),
        month = sanitise_col(mdb, data_in, 'month'),
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

    # Fetch table definition from DB, so we can recreate for temporary table
    cols <- mfdb_fetch(mdb, "SELECT column_name, data_type",
        " FROM information_schema.columns",
        " WHERE table_schema = 'public' AND table_name = 'sample'",
        NULL
    )
    rownames(cols) <- cols$column_name

    # Likely to be pretty big, so pre-load data into a temporary table
    mdb$logger$info("Writing sample rows to temporary table")
    tryCatch(mfdb_send(mdb, "DROP TABLE mfdb_temp_insert"), error = function(e) {
        if(grepl("does not exist", e$message)) return();
        stop(e)
    })
    mfdb_send(mdb, "SET CLIENT_ENCODING TO 'LATIN1'") # Not sure.
    dbWriteTable(mdb$db, "mfdb_temp_insert", survey_sample, row.names = FALSE,
        field.types = structure(cols[names(survey_sample), 'data_type'], names = names(survey_sample)))
    mfdb_send(mdb, "SET CLIENT_ENCODING TO 'UTF8'")

    # Remove data_source and re-insert
    mfdb_transaction(mdb, {
        data_source_id <- get_data_source_id(mdb, data_source)
        mfdb_send(mdb, "DELETE FROM sample",
            " WHERE case_study_id = ", sql_quote(mdb$case_study_id),
            " AND data_source_id = ", sql_quote(data_source_id),
            NULL)
        mfdb_send(mdb,
            "INSERT INTO sample",
            " (", paste(names(survey_sample), collapse=","), ", data_source_id)",
            " SELECT ", paste(names(survey_sample), collapse=","), ", ", sql_quote(data_source_id),
            " FROM mfdb_temp_insert")
    })
    mfdb_send(mdb, "DROP TABLE mfdb_temp_insert");
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

# Import sampling type data
mfdb_import_sampling_type <- function(mdb, data_in) {
    mfdb_import_taxonomy(mdb, 'sampling_type', data.frame(
        id = sanitise_col(mdb, data_in, 'id', default = seq_len(length(data_in))),
        name = sanitise_col(mdb, data_in, 'name'),
        description = sanitise_col(mdb, data_in, 'description', default = c("")))
    )
}

# Check column content, optionally resolving lookup
sanitise_col <- function (mdb, data_in, col_name, default = NULL, lookup = NULL) {
    data_col_name <- grep(col_name, names(data_in), ignore.case=TRUE, value=TRUE)
    if (length(data_col_name) == 0) {
        if (!is.null(default)) return(default);
        stop("Input data is missing ", col_name)
    }
    col <- data_in[[data_col_name[[1]]]]

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
