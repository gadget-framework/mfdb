# Import a lookup table e.g. mfdb_import_taxonomy(mdb, "species", read.csv('species.csv'))
# data_in should have columns id, name, description
mfdb_import_taxonomy <- function (mdb, table_name, data_in, extra_cols = c('description')) {
    # Is table_name one of the recognised tables?
    if (!(table_name %in% mfdb_taxonomy_tables)) {
        stop("Unknown taxonomy table ", table_name)
    }

    # Check there's something to do first
    if (nrow(data_in) == 0) {
        mdb$logger$info(paste0("Taxonomy ", table_name ," no updates to make"))
        return()
    }

    if (!('t_group' %in% names(data_in))) {
        data_in$t_group <- c(NA)
    }
    extra_cols <- c('t_group', extra_cols)

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

    mfdb_transaction(mdb, mfdb_bulk_copy(mdb, table_name, data_in, function (temp_tbl) {
        # Remove rows where nothing changed, if we remove all of them, exit.
        # NB: This won't work if an extra_col is float (e.g. latitude), but should only be an optimisation
        matching_rows <- mfdb_send(mdb, "DELETE FROM ", temp_tbl, " WHERE ", id_col, " IN (",
            "SELECT tmp.", id_col,
            " FROM ", temp_tbl, " AS tmp",
            " JOIN ", table_name, " AS cur ON cur.name = tmp.name",
            " WHERE ", paste0("cur.", extra_cols, " = tmp.", extra_cols, collapse = " AND "),
            ")", result = "rowcount")
        if (matching_rows >= nrow(data_in)) return(NULL)

        # Update all rows where names match, remove
        mfdb_send(mdb, "UPDATE ", table_name, " AS cur",
            " SET ", paste0(extra_cols, " = tmp.", extra_cols, collapse = ","),
            " FROM ", temp_tbl, " AS tmp",
            " WHERE cur.name = tmp.name")
        mfdb_send(mdb, "DELETE FROM ", temp_tbl, " AS tmp WHERE tmp.name IN (SELECT name FROM ", table_name, ")")

        # Renumber remaining entries if there's an overlap
        has_overlap <- mfdb_fetch(mdb, "SELECT EXISTS(",
            "SELECT 1 FROM ", table_name, " cur, ", temp_tbl, " tmp WHERE cur.", id_col, " = tmp.", id_col,
            ")")[1,1]
        if (has_overlap) {
            max_id <- mfdb_fetch(mdb, "SELECT MAX(", id_col, ") FROM ", table_name)[1,1]
            mfdb_send(mdb,
                "UPDATE ", temp_tbl,
                " SET ", id_col, " = ", id_col, " + ", max_id)
        }

        # Insert remaining rows into table
        mfdb_send(mdb,
            "INSERT INTO ", table_name,
            " (", paste(c(id_col, 'name', extra_cols), collapse=","), ")",
            " SELECT ", paste(c(id_col, 'name', extra_cols), collapse=","),
            " FROM ", temp_tbl,
            NULL)
    }))

    invisible(NULL)
}

# Import any cs_specific taxonomies
mfdb_import_cs_taxonomy <- function(mdb, taxonomy_name, data_in) {
    if (!(taxonomy_name %in% mfdb_taxonomy_tables)) {
        stop(
            "Unknown taxonomy name '", taxonomy_name,
            "' should be one of ", paste(mfdb_taxonomy_tables, collapse = ", "))
    }

    col_defs <- mfdb_taxonomy_table_defs[[taxonomy_name]]$cols
    col_defs <- t(matrix(col_defs, nrow = 3))
    extra_cols <- col_defs[,1]
    sanitized_extra_data <- lapply(extra_cols, function (col) {
        if (grepl("_id$", col)) {
            lookup_tbl <- col_def_get_foreign_key(col_defs[col_defs[,1] == col, 2])
            return(sanitise_col(mdb, data_in, gsub("_id$", "", col), lookup = lookup_tbl$table, default = c(NA)))
        }
        return(sanitise_col(mdb, data_in, col, default = c(NA)))
    })
    names(sanitized_extra_data) <- extra_cols

    mfdb_import_taxonomy(mdb, taxonomy_name,
        cbind(data.frame(
            id = sanitise_col(mdb, data_in, 'id', default = seq_len(length(data_in$name))),
            name = sanitise_col(mdb, data_in, 'name'),
            t_group = sanitise_col(mdb, data_in, 't_group', default = c(NA)),

            size = sanitise_col(mdb, data_in, 'size', default = c(NA)),

            stringsAsFactors = FALSE), sanitized_extra_data),
        extra_cols = extra_cols)

    if (taxonomy_name == 'areacell' && 'division' %in% colnames(data_in)) {
        # Import division data if available
        division_data <- data_in[,c('name', 'division'), drop = FALSE]
        colnames(division_data) <- c('areacell', 'division')
        mfdb_import_division(mdb, division_data)
    }

    invisible(NULL)
}
mfdb_import_area <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'areacell', data_in)
mfdb_import_sampling_type <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'sampling_type', data_in)
mfdb_import_bait_type_taxonomy <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'bait_type', data_in)
mfdb_import_tow_taxonomy <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'tow', data_in)
mfdb_import_net_type_taxonomy <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'net_type', data_in)
mfdb_import_population_taxonomy <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'population', data_in)
mfdb_import_port_taxonomy <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'port', data_in)
mfdb_import_trip_taxonomy <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'trip', data_in)
mfdb_import_vessel_taxonomy <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'vessel', data_in)
mfdb_import_vessel_owner_taxonomy <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'vessel_owner', data_in)
mfdb_import_species_taxonomy <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'species', data_in)
mfdb_import_gear_taxonomy <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'gear', data_in)

mfdb_empty_taxonomy <- function(mdb, taxonomy_name) {
    if (!(taxonomy_name %in% mfdb_taxonomy_tables)) {
        stop("Unknown taxonomy table ", taxonomy_name)
    }
    mfdb_send(mdb, "DELETE FROM ", taxonomy_name)
}

# Import divisions
mfdb_import_division <- function (mdb, data_in) {
    if(is.data.frame(data_in)) {
        if (length(intersect(colnames(data_in), c('division', 'areacell'))) < 2) {
            stop("data.frame needs both division and areacell columns")
        }
        data_in <- data.frame(
            division = sanitise_col(mdb, data_in, 'division'),
            areacell_id = sanitise_col(mdb, data_in, 'areacell', lookup = 'areacell'),
            stringsAsFactors = FALSE)
    } else if(is.list(data_in)) {
        data_in <- data.frame(
            division = unlist(lapply(names(data_in), function(n) { rep(n, length(data_in[[n]])) })),
            areacell_id = sanitise_col(mdb, data.frame(areacell = unlist(data_in)), 'areacell', lookup = 'areacell'))
    } else {
        stop("data_in should be a list of areacell vectors")
    }

    mfdb_transaction(mdb, {
        mfdb_send(mdb,
            "DELETE FROM division",
            " WHERE division IN ", sql_quote(unique(data_in$division), always_bracket = TRUE),
            "")
        res <- mfdb_insert(mdb, 'division', data_in)
    })
}
