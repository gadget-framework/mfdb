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

    if (taxonomy_name == 'areacell') {
        extra_cols <- c('size')
    } else if (taxonomy_name == 'vessel') {
        extra_cols <- c('vessel_type_id', 'full_name', 'length', 'power', 'tonnage')
    } else if (taxonomy_name == 'tow') {
        extra_cols <- c('latitude', 'longitude', 'depth', 'length')
    } else {
        extra_cols <- c('description')
    }

    mfdb_import_taxonomy(mdb, taxonomy_name,
        data.frame(
            id = sanitise_col(mdb, data_in, 'id', default = seq_len(length(data_in$name))),
            name = sanitise_col(mdb, data_in, 'name'),

            size = sanitise_col(mdb, data_in, 'size', default = c(NA)),

            vessel_type_id = sanitise_col(mdb, data_in, 'vessel_type', lookup = 'vessel_type', default = c(NA)),
            full_name = sanitise_col(mdb, data_in, 'full_name', default = c(NA)),
            length = sanitise_col(mdb, data_in, 'length', default = c(NA)),
            power = sanitise_col(mdb, data_in, 'power', default = c(NA)),
            tonnage = sanitise_col(mdb, data_in, 'tonnage', default = c(NA)),

            latitude = sanitise_col(mdb, data_in, 'latitude', default = c(NA)),
            longitude = sanitise_col(mdb, data_in, 'longitude', default = c(NA)),
            depth = sanitise_col(mdb, data_in, 'depth', default = c(NA)),

            description = sanitise_col(mdb, data_in, 'description', default = c("")),
            stringsAsFactors = FALSE),
        extra_cols = extra_cols)
    invisible(NULL)
}
mfdb_import_area <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'areacell', data_in)
mfdb_import_sampling_type <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'sampling_type', data_in)
mfdb_import_tow_taxonomy <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'tow', data_in)
mfdb_import_vessel_taxonomy <- function(mdb, data_in) mfdb_import_cs_taxonomy(mdb, 'vessel', data_in)

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
