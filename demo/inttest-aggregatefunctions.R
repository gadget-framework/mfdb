# This script demonstrates the postgres aggregate functions we add
#
# NB: if you want to run this, you must be using mfdb-workspace, also be warned
# any data stored in your database will be destroyed.
library(unittest)
library(mfdb)
source('mfdb/tests/utils/helpers.R')
source('mfdb/tests/utils/inttest-helpers.R')

# Empty database & rebuild
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = FALSE)
mfdb:::mfdb_finish_import(mdb)  # We don't make any queries, so this doesn't get triggered

numeric_na <- c(NA,0)[[1]]
na_to_null <- function (x) ifelse(is.na(x), "NULL", x)

ok_group("weighted_mean", {
    db_weighted_mean <- function (vals, weights) {
        mfdb:::mfdb_fetch(mdb,
            "SELECT WEIGHTED_MEAN(val::integer,wgt) res",
            " FROM (VALUES ",
            paste0("(", na_to_null(vals), ",", na_to_null(weights), ")", collapse = ","),
            ") S(val,wgt);")[1,1]
    }
    ok(cmp(
        db_weighted_mean(c(1, 2), c(10, 20)),
        weighted.mean(c(1, 2), c(10, 20))), "Weighted mean with 2 values")
    ok(cmp(
        db_weighted_mean(c(1, 2, 8), c(10, 20, 400)),
        weighted.mean(c(1, 2, 8), c(10, 20, 400))), "Weighted mean with 3 values")
    ok(cmp(
        db_weighted_mean(c(1, 2, 8), c(10, 20, NA)),
        weighted.mean(c(1, 2, 8), c(10, 20, NA))), "Null weight produces NA")
    ok(cmp(
        db_weighted_mean(c(1, 2, NA), c(10, 20, 400)),
        weighted.mean(c(1, 2), c(10, 20))), "Null values not included in total")
    ok(cmp(
        db_weighted_mean(c(NA, NA, NA), c(10, 20, 400)),
        numeric_na), "All-null values results in NA")
})

ok_group("weighted_stddev", {
    db_weighted_stddev <- function (vals, weights) {
        mfdb:::mfdb_fetch(mdb,
            "SELECT WEIGHTED_STDDEV(val::integer,wgt) res",
            " FROM (VALUES ",
            paste0("(", na_to_null(vals), ",", na_to_null(weights), ")", collapse = ","),
            ") S(val,wgt);")[1,1]
    }
    weighted.stddev <- function(x, w) if (anyNA(w)) numeric_na else sd(rep(x, w))

    ok(cmp(
        db_weighted_stddev(c(1, 2, 33, 4), c(1, 1, 1, 1)),
        sd(c(1, 2, 33, 4))), "Equal weights means same as regular standard deviation")

    ok(cmp(
        db_weighted_stddev(c(1), c(1)),
        numeric_na), "A single value has no standard deviation")

    for (count in seq_len(20)) {
        x <- sample(1:100, 50, replace = TRUE)
        w <- sample(1:100, 50, replace = TRUE)
        ok(cmp(
            db_weighted_stddev(x, w),
            weighted.stddev(x, w)), "Functions match (50 random values/weights)")
    }

    ok(cmp(
        db_weighted_stddev(c(1, 2, NA, 4), c(1, 3, 5, 9)),
        weighted.stddev(c(1, 2, 4), c(1,3,9))), "Null values are ignored by standard deviation")
    ok(cmp(
        db_weighted_stddev(c(NA, NA, NA, NA), c(1, 3, 5, 9)),
        numeric_na), "All-null values results in NULL")
})
