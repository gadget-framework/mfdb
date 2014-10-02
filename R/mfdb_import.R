# Import a lookup table e.g. mfdb_import_identifiers("species", read.csv('species.csv'))
mfdb_import_identifiers <- function (mfdb, table_name, new_data) {
    # Is table_name one of the recognised tables?
    if (!(table_name %in% c("institute", "fleet", "gear", "vessel", "market_category", "sampling_type", "species"))) {
        stop("Unknown identifier table ", table_name)
    }

    # Fetch all existing names
    existing = unlist(dbFetch(dbSendQuery(mfdb$db, paste0("SELECT name FROM ", table_name))))

    # Either add or update rows. Removing is risky, since we might have dependent data
    dbSendQuery(mfdb$db, "BEGIN TRANSACTION")
    for (name in unlist(new_data[1])) {
        desc <- as.character(new_data[new_data[1] == name,2])
        if (name %in% existing) {
            dbSendQuery(mfdb$db, paste0(
                "UPDATE ", table_name,
                " SET description = ", sql_quote(desc),
                " WHERE name = ", sql_quote(name)))
        } else {
            dbSendQuery(mfdb$db, paste0(
                "INSERT INTO ", table_name,
                " (name, description) VALUES ",
                " (", sql_quote(name), ",", sql_quote(desc), ")"))
        }
    }
    dbCommit(mfdb$db)
    invisible(TRUE)
}

# NB: Dead, but exicting code
aggregate_survey_data <- function (survey_data_in, length_cell_size) {
    grouping_cols <- c("year", "month", "area_cell", "species", "age", "sex")
    grouping <- function (col) {
        as.formula(paste(
            col,
            "~",
            paste(c(grouping_cols, paste("length %/%", length_cell_size)), collapse = "+"),
            sep = " "))
    }

    # Generate counts first, use this to build data frame
    survey_data_agg <- aggregate(grouping("year*0"), data = survey_data_in, length)

    # Return combined version of data
    return(cbind(
        survey_data_agg[,grouping_cols],
        length_cell_min = survey_data_agg[,length(grouping_cols) + 1] * length_cell_size,
        length_cell_max = survey_data_agg[,length(grouping_cols) + 1] * length_cell_size + length_cell_size,
        count = survey_data_agg[,length(grouping_cols) + 2],
        length_mean = aggregate(grouping("length"), data = survey_data_in, mean)[,length(grouping_cols) + 2],
        length_var  = aggregate(grouping("length"), data = survey_data_in, var)[,length(grouping_cols) + 2],
        weight_mean = aggregate(grouping("weight"), data = survey_data_in, mean)[,length(grouping_cols) + 2],
        weight_var  = aggregate(grouping("weight"), data = survey_data_in, var)[,length(grouping_cols) + 2]))
}

mfdb_import_survey <- function (mfdb, data_in, ...) {
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
            new_levels <- mfdb_fetch(mfdb,
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
    dbSendQuery(mfdb$db, "BEGIN TRANSACTION")
    dbSendQuery(mfdb$db, "DELETE FROM survey WHERE data_source = ", sql_quote(survey_metadata$data_source), " CASCADE")
    res <- mfdb_insert(mfdb, 'survey', survey_metadata, returning = "survey_id")
    res <- mfdb_insert(mfdb, 'sample', survey_sample, extra = res$survey_id)
    dbCommit(mfdb$db)
    invisible(TRUE)
}

mfdb_import_areas <- function (mfdb) {
}
