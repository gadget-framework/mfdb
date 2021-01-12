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
    # NB: NUMERIC precision is fairly arbitrary, small enough to hide rounding issues from REAL
    datatype <- ifelse(lookup == "species", "BIGINT", ifelse(lookup == "age", "NUMERIC(10,5)", "INT"))

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
    } else if (lookup %in% mfdb_taxonomy_tables) {
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

    # Fetch lookup content, can we represent this as a smallset? If so, convert
    if (length(unlist(x)) < 40) {
        lookup_content <- mfdb_fetch(mdb, paste0("SELECT sample, name, value FROM ", attr(x, 'table_name')))
        if (ncol(lookup_content) > 0 && anyDuplicated(lookup_content[,'value']) == 0) {
            class(x) <- c("mfdb_smallset", class(x))
            attr(x, 'lookup_content') <- lookup_content
            return(invisible(x))
        }
    }

    invisible(NULL)
}

select_clause.mfdb_group <- function(mdb, x, col, outputname, group_disabled = FALSE) {
    paste0(attr(x, 'table_name'), ".name AS ", outputname)
}

from_clause.mfdb_group <- function(mdb, x, col, outputname, group_disabled = FALSE) {
    paste0(attr(x, 'table_name'))
}

where_clause.mfdb_group <- function(mdb, x, col, outputname, group_disabled = FALSE) {
    lookup <- gsub('(.*\\.)|_id', '', col)

    cast <- if (lookup == 'age') "::NUMERIC(10,5)" else ""
    paste0(col, cast, " = ", attr(x, 'table_name'), ".value")
}

##### mfdb_group helpers

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

##### mfdb_smallset: mfdb_group implementation that inlines the group data into the query

pre_query.mfdb_smallset <- function(mdb, x, col) {
    # No need to do anything, pre_query.mfdb_group already added the lookup_content
}

select_clause.mfdb_smallset <- function(mdb, x, col, outputname, group_disabled = FALSE) {
    lookup <- gsub('(.*\\.)|_id', '', col)
    cast <- if (lookup == 'age') "::NUMERIC(10,5)" else ""

    groups <- names(sort(table(attr(x, 'lookup_content')$name)))
    if (length(groups) == 1) {
        # If the where clause matches, then it's in this group
        return(paste0(sql_quote(groups[[1]]), " AS ", outputname))
    }
    return(paste(c(
        "CASE",
        vapply(head(groups, -1), function (g) {
            paste0(
                " WHEN ", col, cast,
                " IN ", sql_quote(attr(x, 'lookup_content')[attr(x, 'lookup_content')$name == g, 'value'], always_bracket = TRUE),
                " THEN ", sql_quote(g))
        }, character(1)),
        " ELSE ", sql_quote(tail(groups, 1)), " END AS ", outputname), collapse = ""))
}

from_clause.mfdb_smallset <- function(mdb, x, col, outputname, group_disabled = FALSE) {
    # No need to add temporary table to query, it's embedded into it
    c()
}

where_clause.mfdb_smallset <- function(mdb, x, col, outputname, group_disabled = FALSE) {
    lookup <- gsub('(.*\\.)|_id', '', col)
    cast <- if (lookup == 'age') "::NUMERIC(10,5)" else ""

    return(paste0(col, cast, " IN ", sql_quote(attr(x, 'lookup_content')[,'value'], always_bracket = TRUE)))
}

agg_summary.mfdb_smallset <- function(mdb, x, col, outputname, data, sample_num) {
    attr(x, 'lookup_content') <- NULL
    class(x) <- class(x)[!(class(x) %in% 'mfdb_smallset')]
    return(x)
}

##### mfdb_bootstrap_group: Extends mfdb_group by offering random sampling from within

mfdb_bootstrap_group <- function (count, group, seed = NULL) {
    if (!('mfdb_group' %in% class(group))) {
        stop("Second argument should be a mfdb_group")
    }
    if (count < 1) {
        stop("Count should be equal or greater than 1")
    }

    # Save PRNG state and set seed / reset it
    old_seed <- tryCatch(get(".Random.seed", pos = globalenv()), error = function (e) NULL)
    # NB: 3.6.0 introduces sample.kind: https://github.com/wch/r-source/blob/7f6cc784523dfa69087958633f7deb309d9c8718/doc/NEWS.Rd#L150-L161
    tryCatch(
        set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding"),
        error = function (e) set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion"))

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

sample_clause.mfdb_bootstrap_group <- function(mdb, x, col, outputname, group_disabled = FALSE) {
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
