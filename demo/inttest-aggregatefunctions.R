# This script demonstrates the postgres aggregate functions we add
#
# NB: if you want to run this, you must be using mfdb-workspace, also be warned
# any data stored in your database will be destroyed.
library(unittest)
library(mfdb)
source('mfdb/tests/utils/helpers.R')
source('mfdb/tests/utils/inttest-helpers.R')

# Empty database & rebuild
mfdb('', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = TRUE)

ok_group("weighted_mean", {
    db_weighted_mean <- function (vals, weights) {
        mfdb:::mfdb_fetch(mdb,
            "SELECT WEIGHTED_MEAN(val,wgt) res",
            " FROM (VALUES ",
            paste0("(", vals, ",", ifelse(is.na(weights), "NULL", weights), ")", collapse = ","),
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
})
