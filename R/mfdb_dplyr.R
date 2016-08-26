# Join all possible taxonomies
all_cols <- c("$ALL")

# Fetch a dplyr version of any measurement table
mfdb_dplyr_table <- function (mdb, tbl_name, include_cols) {
    # Given (name), (value), (name), (value), ..., make a list
    named_list <- function (...) {
        name_and_items <- c(...)
        structure(
            (name_and_items)[(seq_along(name_and_items) %% 2) == 1],
            names = (name_and_items)[(seq_along(name_and_items) %% 2) == 0])
    }

    # Turn mdb connection into dplyr & get table
    dp <- dplyr::src_sql("postgres", mdb$db, info = DBI::dbGetInfo(mdb$db))
    dp_tbl <- dplyr::tbl(dp, tbl_name)

    # Get all possible taxonomy columns in this table
    tx_cols <- intersect(
        paste0(c(mfdb_taxonomy, mfdb_cs_taxonomy), '_id'),
        colnames(dp_tbl))

    # Replace all_cols marker with real thing
    if (identical(include_cols, all_cols)) {
        include_cols <- c(mfdb_taxonomy, mfdb_cs_taxonomy)
    }

    # Left-join each desired taxonomy table
    for (tx_col in intersect(tx_cols, paste0(include_cols, '_id'))) {
        tx <- sub('_id$', '', tx_col)
        tx_tbl <- do.call(dplyr::select_, c(
            list(dplyr::tbl(dp, tx)),
            named_list("name", tx, tx_col, tx_col),
            NULL))
        dp_tbl <- dplyr::left_join(dp_tbl, tx_tbl, by = tx_col)
    }

    # Hide all taxonomy id fields in the output
    dp_tbl <- dplyr::select(dp_tbl, -one_of(tx_cols))

    return(dp_tbl)
}

# Define one shortcut for each measurement table
mfdb_dplyr_survey_index <- function (mdb, include_cols = all_cols) mfdb_dplyr_table(mdb, 'survey_index', include_cols)
mfdb_dplyr_division     <- function (mdb, include_cols = all_cols) mfdb_dplyr_table(mdb, 'division', include_cols)
mfdb_dplyr_sample       <- function (mdb, include_cols = all_cols) mfdb_dplyr_table(mdb, 'sample', include_cols)
mfdb_dplyr_predator     <- function (mdb, include_cols = all_cols) mfdb_dplyr_table(mdb, 'predator', include_cols)
mfdb_dplyr_prey         <- function (mdb, include_cols = all_cols) mfdb_dplyr_table(mdb, 'prey', include_cols)

# TODO: Tests
#mfdb_dplyr_sample(mdb, c('data_source', 'species'))
