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

    # Fetch all existing ids, quit if all are there
    existing <- mfdb_fetch(mdb,
        "SELECT ", id_col, ", name, ", paste(extra_cols, collapse = ", "),
        " FROM ", table_name,
        " ORDER BY 1")

    # Throw away rows which don't need updating
    if (nrow(existing) > 0) {
        # NB: This won't work if an extra_col is float (say latitude), but
        #     detecting this will probably use up just as much time as updating.
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
        new_data <- data_in[data_in$name %in% setdiff(data_in$name, existing$name), ]

        # If some ids appear in both new and existing, bump entire range up so we don't intersect
        overlapping_ids <- intersect(new_data[[id_col]], existing[[id_col]])
        if (length(overlapping_ids) > 0) {
            new_data[[id_col]] <- max(existing[[id_col]]) + new_data[[id_col]]
        }

        mfdb_insert(mdb, table_name, new_data)

        # Rows with matching names should be updated, but existing ids kept
        if (nrow(existing) > 0) mfdb_update(mdb,
            table_name,
            merge(existing[, c(id_col, 'name')], data_in[, c('name', extra_cols)]),
            where = c())
    })

    invisible(NULL)
}

# Import any cs_specific taxonomies
mfdb_import_cs_taxonomy <- function(mdb, taxonomy_name, data_in) {
    if (!(taxonomy_name %in% mfdb_taxonomy_tables)) {
        stop(
            "Unknown taxonomy name '", taxonomy_name,
            "' should be one of ", paste(mfdb_taxonomy_tables, collapse = ", "))
    }

    extra_cols <- mfdb_get_taxonomy_extra_cols(taxonomy_name)
    sanitized_extra_data <- lapply(extra_cols, function (col) {
        if (grepl("_id$", col)) {
            return(sanitise_col(mdb, data_in, gsub("_id$", "", col), lookup = gsub("_id$", "", col), default = c(NA)))
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
mfdb_import_tow_taxonomy <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'tow', data_in)
mfdb_import_vessel_taxonomy <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'vessel', data_in)
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
        dbSendQuery(mdb$db, paste0(
            "DELETE FROM division",
            " WHERE division IN ", sql_quote(unique(data_in$division), always_bracket = TRUE),
            ""))
        res <- mfdb_insert(mdb, 'division', data_in)
    })
}
