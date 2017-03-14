# list(
#     list(camels = c(1,2), aardvarks = c(3,4)),
#     list(camels = c(2,2), aardvarks = c(4,4))
# )
mfdb_group <- function (...) {
    group <- structure(list(...),
            table_name = paste(
                "temp",
                paste0(sample(letters, 5, replace = TRUE), collapse=""),
                sep = "_"),
            class = c("mfdb_group", "mfdb_aggregate"))

    invisible(group)
}

pre_query.mfdb_group <- function(mdb, x, col) {
    group <- x
    lookup <- gsub('(.*\\.)|_id', '', col)
    datatype <- ifelse(lookup == "species", "BIGINT", ifelse(lookup == "age", "REAL", "INT"))

    # If the table already exists, nothing to do
    if (mfdb_table_exists(mdb, attr(x, 'table_name'))) {
        return(invisible(NULL))
    }

    mfdb_send(mdb, paste(
            "CREATE",
            (if (!mdb$save_temp_tables) "TEMPORARY"),
            "TABLE",
            attr(x, 'table_name'),
            "(sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value ", datatype,
            ")"))

    # Break down group into single table
    denormalized <- denormalize(group)

    if (lookup == 'areacell') {
        # Decompose divisions into areacells first
        for (set in split(denormalized, list(denormalized$sample, denormalized$name))) {
            # Can't insert 2 copies of a division at the same time, so insert
            # unique subsets of divisions until there's none left
            values <- set[,'value']
            while(length(values) > 0) {
                quoted_values <- sql_quote(unique(values), always_bracket = TRUE, always_quote = TRUE)
                mfdb_send(mdb,
                    "INSERT INTO ", attr(x, 'table_name'),
                    " SELECT ", sql_quote(set[1, 'sample']), " AS sample",
                    ", ", sql_quote(set[1, 'name']), " AS name",
                    ", ", lookup, "_id AS value",
                    " FROM division",
                    " WHERE division IN ", quoted_values)
                values <- values[duplicated(values)]
            }
        }
    } else if (lookup %in% mfdb_taxonomy || lookup %in% mfdb_cs_taxonomy) {
        mismatches <- mfdb_fetch(mdb,
            "SELECT d.name",
            " FROM UNNEST(ARRAY", sql_quote(unique(unlist(x)), always_bracket = TRUE, brackets = "[]"), ") AS d",
            " LEFT OUTER JOIN ", lookup, " x ON d.name = x.name",
            " WHERE x.name IS NULL")
        if (nrow(mismatches) > 0) {
            stop("Input data has items that don't match ", lookup, " vocabulary: ",
                paste(head(mismatches[,1], n = 50), collapse = ","),
                ifelse(nrow(mismatches) > 50, ', ...', ''),
                NULL)
        }

        # Decompose divisions into areacells first
        for (set in split(denormalized, list(denormalized$sample, denormalized$name))) {
            # Can't insert 2 copies of a division at the same time, so insert
            # unique subsets until there's none left
            values <- set[,'value']
            while(length(values) > 0) {
                quoted_values <- sql_quote(unique(values), always_bracket = TRUE, always_quote = TRUE)
                mfdb_send(mdb,
                    "INSERT INTO ", attr(x, 'table_name'),
                    " SELECT ", sql_quote(set[1, 'sample']), " AS sample",
                    ", ", sql_quote(set[1, 'name']), " AS name",
                    ", ", lookup, "_id AS value",
                    " FROM ", lookup,
                    " WHERE (",
                        "name IN ", quoted_values,
                        " OR t_group IN ", quoted_values,
                    ")")
                values <- values[duplicated(values)]
            }
        }
    } else {
        # Populate table based on denormalized group
        mfdb_insert(mdb, attr(x, 'table_name'), denormalized)
    }

    # Index the lookup table to speed up queries
    mfdb_send(mdb, sql_create_index(attr(x, 'table_name'), c('value', 'name', 'sample')))

    invisible(NULL)
}

select_clause.mfdb_group <- function(mdb, x, col, outputname) {
    paste0(attr(x, 'table_name'), ".name AS ", outputname)
}

from_clause.mfdb_group <- function(mdb, x, col, outputname) {
    paste0(attr(x, 'table_name'))
}

where_clause.mfdb_group <- function(mdb, x, col, outputname) {
    paste0(col, " = ", attr(x, 'table_name'), ".value")
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

mfdb_bootstrap_group <- function (count, group, seed = NULL) {
    if (!('mfdb_group' %in% class(group))) {
        stop("Second argument should be a mfdb_group")
    }
    if (count < 1) {
        stop("Count should be equal or greater than 1")
    }

    # Save PRNG state and set seed / reset it
    old_seed <- tryCatch(get(".Random.seed", pos = globalenv()), error = function (e) NULL)
    set.seed(seed, kind = "Mersenne-Twister")

    bs_group <- structure(
            lapply(1:count, function(i) { lapply(group, function (g) { if (length(g) == 1) g else sample(g, replace = TRUE) }) }),
            table_name = paste0(attr(group, 'table_name'), "_bs", count, seed),
            class = c("mfdb_bootstrap_group", "mfdb_group", "mfdb_aggregate"))

    # Restore PRNG to how it was before
    if (is.null(old_seed)) {
        set.seed(NULL)
    } else {
        assign(".Random.seed", old_seed, pos = globalenv())
    }
    invisible(bs_group)
}

sample_clause.mfdb_bootstrap_group <- function(mdb, x, col, outputname) {
    paste0(attr(x, 'table_name'), ".sample")
}

agg_summary.mfdb_bootstrap_group <- function(mdb, x, col, outputname, data, sample_num) {
    if (length(data$bssample) < 1) stop("Need some data to know which group was used")

    # Pick out the list for this sample
    as.list(x[[sample_num]])
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
