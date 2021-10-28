# Join all possible taxonomies
all_cols <- c("$ALL")

# Fetch a dplyr version of any measurement table
mfdb_dplyr_table_inner <- function (mdb, table_name, include_cols = all_cols, prefix = c()) {
    # Join prefixes together without extra _
    prefix_join <- function (...) paste(c(...), collapse = "_")

    # Resolve all_cols using col_defs_for
    if (identical(include_cols, all_cols)) {
        include_cols <- names(col_defs_for(table_name))
    }
    # Empty string (i.e. the table name from outer step) is actually a reference to the name col
    include_cols <- gsub("^$", "name", include_cols)

    # Find column definitions, turn into matrix for easy access
    if (table_name %in% names(mfdb_measurement_table_defs)) {
        col_defs <- mfdb_measurement_table_defs[[table_name]]$cols
    } else if (table_name %in% names(mfdb_taxonomy_table_defs)) {
        # TODO: We should sort out these definitions
        col_defs <- c(
            "name", "VARCHAR(1024) UNIQUE NOT NULL", "Short name used in data files / output data (in ltree notation)",
            "t_group", paste0("VARCHAR(1024) NULL REFERENCES ", table_name, "(name)"), "Value grouping (short name)",
            mfdb_taxonomy_table_defs[[table_name]]$cols)
    } else {
        stop("Unknown table ", table_name)
    }
    col_defs <- t(matrix(col_defs, nrow = 3))

    select_cols <- c(paste0(table_name, '_id'))  # NB: Need ID for join to work
    join_tables <- as.environment(list())
    join_by <- as.environment(list())
    for (i in seq_len(nrow(col_defs))) {
        if (col_defs[i,1] != 't_group' && grepl('REFERENCES', col_defs[i,2], fixed = TRUE)) {
            fk <- col_def_get_foreign_key(col_defs[i,2])
            # NB: sub_prefix != fk$table, e.g. start_port
            sub_prefix <- gsub("_id$", "", col_defs[i,1])

            # Find all prefixed entries in include cols, remove prefix
            sub_include_cols <- grep(paste0('^', sub_prefix, '($|_)'), include_cols, value = TRUE)
            sub_include_cols <- gsub(paste0('^', sub_prefix, '($|_)'), '', sub_include_cols)

            if (length(sub_include_cols) > 0) {
                # Make sure we have the foreign key to join with
                select_cols <- c(select_cols, col_defs[i, 1])
                # Get the sub-table so we can join it later
                join_tables[[prefix_join(prefix, sub_prefix)]] <- mfdb_dplyr_table_inner(
                    mdb,
                    fk$table,
                    sub_include_cols,
                    prefix = prefix_join(prefix, sub_prefix))
                join_by[[prefix_join(prefix, sub_prefix)]] <- structure(
                    # i.e. final name of ID column in subtable
                    prefix_join(prefix, sub_prefix, fk$column),
                    # i.e. final name of ID column here
                    names = prefix_join(prefix, col_defs[i, 1]))
            }
        } else if (length(prefix) == 0) {
            # In the outerermost table we include all measurements
            select_cols <- c(select_cols, col_defs[i, 1])
        } else {
            select_cols <- c(select_cols, intersect(col_defs[i, 1], include_cols))
        }
    }

    # Select all requested columns
    dp_tbl <- dplyr::tbl(mdb$db, table_name)
    dp_tbl <- dplyr::select(dp_tbl, dplyr::any_of(select_cols))
    dp_tbl <- dplyr::rename_with(dp_tbl, function (names) {
        vapply(names, function (n) {
            # Add prefix to all columns, Rename name -> (prefix)
            ifelse(
                n == 'name',
                if (is.null(prefix)) table_name else prefix,
                prefix_join(prefix, n))
        }, character(1))
    })

    # Do the joins now the initial selection is done
    for (j in ls(join_tables)) {
        dp_tbl <- dplyr::left_join(dp_tbl, join_tables[[j]], by = join_by[[j]])
    }

    if (length(prefix) == 0) {
        # Outermost table, remove all _id cols now
        dp_tbl <- dplyr::select(dp_tbl, -dplyr::ends_with('_id'))
    }

    return(dp_tbl)
}

# Define one shortcut for each measurement table
mfdb_dplyr_table <- function (mdb, table_name, include_cols = all_cols) mfdb_dplyr_table_inner(mdb, table_name, include_cols)
mfdb_dplyr_survey_index <- function (mdb, include_cols = all_cols) mfdb_dplyr_table_inner(mdb, 'survey_index', include_cols)
mfdb_dplyr_division     <- function (mdb, include_cols = all_cols) mfdb_dplyr_table_inner(mdb, 'division', include_cols)
mfdb_dplyr_sample       <- function (mdb, include_cols = all_cols) mfdb_dplyr_table_inner(mdb, 'sample', include_cols)
mfdb_dplyr_predator     <- function (mdb, include_cols = all_cols) mfdb_dplyr_table_inner(mdb, 'predator', include_cols)
mfdb_dplyr_prey         <- function (mdb, include_cols = all_cols) mfdb_dplyr_table_inner(mdb, 'prey', include_cols)
