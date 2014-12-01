# list(
#     list(camels = c(1,2), aardvarks = c(3,4)),
#     list(camels = c(2,2), aardvarks = c(4,4))
# )
mfdb_group <- function (...) {
    group <- structure(list(...),
            class = c("mfdb_group", "mfdb_aggregate"))

    invisible(group)
}

pre_query.mfdb_group <- function(mdb, x, outputname) {
    table_name <- paste0("temp_", outputname)
    group <- x
    datatype <- "INT"

    # Turn mfdb_group into a temporary table to join to
    if (is.null(group)) stop(paste("You must provide a mfdb_group for", table_name))
    #TODO: Assign random ID attribute to group, use this as table name or re-use table if it already has one
    # Remove the table if it exists, and recreate it
    tryCatch(mfdb_send(mdb,
        "DROP ",
        "TABLE ", table_name), error = function (e) {
            if (!is.null(mdb)) mdb$logger$debug(paste("Ignored", e))
        })
    mfdb_send(mdb, paste(
            "CREATE",
            (if (!is.null(mdb) && !mdb$save_temp_tables) "TEMPORARY"),
            "TABLE",
            table_name,
            "(sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value ", datatype,
            ")"))

    # Break down group into single table
    denormalized <- denormalize(group)

    if (table_name == 'temp_area') {
        # Decompose divisions into areacells first
        for (set in split(denormalized, list(denormalized$sample, denormalized$name))) {
            mfdb_send(mdb,
                "INSERT INTO ", table_name,
                " SELECT ", sql_quote(set[1, 'sample']), " AS sample",
                ", ", sql_quote(set[1, 'name']), " AS name",
                ", areacell_id AS value",
                " FROM division",
                " WHERE case_study_id = ", sql_quote(mdb$case_study_id),
                " AND division IN ", sql_quote(set[,'value'], always_bracket = TRUE))
        }
    } else {
        # Populate table based on denormalized group
        mfdb_insert(mdb, table_name, denormalized)
    }

    # Index the lookup table to speed up queries
    mfdb_send(mdb, sql_create_index(table_name, c('value', 'name', 'sample')))
}

select_clause.mfdb_group <- function(x, col, outputname) {
    paste0("temp_", outputname, ".name AS ", outputname)
}

from_clause.mfdb_group <- function(x, col, outputname) {
    paste0("temp_", outputname)
}

where_clause.mfdb_group <- function(x, col, outputname) {
    paste0(col, " = ", "temp_", outputname, ".value")
}

# Some default time groupings
mfdb_timestep_yearly <- mfdb_group('1' = 1:12)
mfdb_timestep_biannually <- mfdb_group('1' = 1:6, '2' = 7:12)
mfdb_timestep_quarterly <- mfdb_group('1' = 1:3, '2' = 4:6, '3' = 7:9, '4' = 10:12)

# Shortcut, return group of form prefix1 = c(1,2)
mfdb_group_numbered <- function (prefix, ...) {
    # Set label for items based on prefix
    items <- list(...)
    names(items) <- mapply(function (i) {
        paste0(prefix, i)
    }, seq_along(items))

    do.call(mfdb_group, items)
}

mfdb_bootstrap_group <- function (count, group) {
    if (!('mfdb_group' %in% class(group))) {
        stop("Second argument should be a mfdb_group")
    }
    if (count < 1) {
        stop("Count should be equal or greater than 1")
    }

    bs_group <- structure(
            lapply(1:count, function(i) { lapply(group, function (g) { if (length(g) == 1) g else sample(g, replace = TRUE) }) }),
            class = c("mfdb_bootstrap_group", "mfdb_group", "mfdb_aggregate"))
    invisible(bs_group)
}

# Denormalise the nesting and convert into a list of strings
denormalize <- function(group, samp_count = 0) UseMethod("denormalize")
# Break down each sample
denormalize.mfdb_bootstrap_group <- function (group, samp_count = 0) {
    do.call(rbind, lapply(1:length(group), function (i) {
        denormalize.mfdb_group(group[[i]], samp_count = i)
    }))
}
# Break down nested vectors into data.frame
denormalize.mfdb_group <- function (group, samp_count = 0) {
    do.call(rbind, lapply(1:length(group), function (i) {
        data.frame(
            sample = samp_count,
            name = names(group)[[i]],
            value = I(group[[i]]))
    }))
}
