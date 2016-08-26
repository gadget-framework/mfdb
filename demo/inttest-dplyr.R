# Excerise dplyr interface
#
# ok and ok_group are there so we can run this code and verify it is correct,
# and not needed every-day use.
#
# NB: if you want to run this, you must be using mfdb-workspace, also be warned
# any data stored in your database will be destroyed.
library(unittest)
library(mfdb)
library(dplyr)
source('mfdb/tests/utils/helpers.R')
source('mfdb/tests/utils/inttest-helpers.R')

# Empty database & rebuild
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('Test', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = TRUE) # TODO:

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(10,200,400)))
mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G03')))

to_df <- function (dp) {
    as.data.frame(arrange(dp, sample_id))
}

ok_group("Dplyr table example", {
    mfdb_import_sampling_type(mdb, data.frame(id = 1:2, name = c("comm", "res")))

    # Import a survey for the data we are interested in
    mfdb_import_survey(mdb, data_source = "ds2000",
        table_string("
year    month   areacell        species sampling_type length  age     weight
2000    1       45G01           COD     comm          21      2       210
2000    1       45G02           COD     comm          34      3       220
2000    1       45G03           COD     comm          34      3       230
2000    1       45G01           HAD     comm          62      1       320
2000    1       45G02           HAD     comm          53      1       330
2000    1       45G03           HAD     comm          54      2       430
2000    1       45G01           COD     res           28      2       210
2000    1       45G02           COD     res           34      3       220
2000    1       45G03           COD     res           24      3       230
2000    1       45G01           HAD     res           12      1       320
2000    1       45G02           HAD     res           44      1       330
2000    1       45G03           HAD     res           14      2       430
        "))

    # Connect to the table, don't fetch any extra data
    ok(cmp(to_df(mfdb_dplyr_sample(mdb, c())), data.frame(
        sample_id = seq(12),
        year = as.integer(2000),
        month = as.integer(1),
        age = as.integer(c(2, 3, 3, 1, 1, 2, 2, 3, 3, 1, 1, 2)),
        length = c(21, 34, 34, 62, 53, 54, 28, 34, 24, 12, 44, 14),
        length_var = as.integer(NA),
        length_min = as.integer(NA),
        weight = c(210, 220, 230, 320, 330, 430, 210, 220, 230, 320, 330, 430),
        weight_var = as.integer(NA),
        count = c(1),
        stringsAsFactors = FALSE)), "No taxonomy tables")

    ok(cmp(to_df(mfdb_dplyr_sample(mdb, c('sampling_type', 'species'))), data.frame(
        sample_id = seq(12),
        year = as.integer(2000),
        month = as.integer(1),
        age = as.integer(c(2, 3, 3, 1, 1, 2, 2, 3, 3, 1, 1, 2)),
        length = c(21, 34, 34, 62, 53, 54, 28, 34, 24, 12, 44, 14),
        length_var = as.integer(NA),
        length_min = as.integer(NA),
        weight = c(210, 220, 230, 320, 330, 430, 210, 220, 230, 320, 330, 430),
        weight_var = as.integer(NA),
        count = c(1),
        species = rep(c('COD', 'HAD'), each = 3, times = 2),
        sampling_type = rep(c('comm', 'res'), each = 6),
        stringsAsFactors = FALSE)), "Select taxonomy tables")

    ok(cmp(to_df(mfdb_dplyr_sample(mdb)), data.frame(
        sample_id = seq(12),
        year = as.integer(2000),
        month = as.integer(1),
        age = as.integer(c(2, 3, 3, 1, 1, 2, 2, 3, 3, 1, 1, 2)),
        length = c(21, 34, 34, 62, 53, 54, 28, 34, 24, 12, 44, 14),
        length_var = as.numeric(NA),
        length_min = as.integer(NA),
        weight = c(210, 220, 230, 320, 330, 430, 210, 220, 230, 320, 330, 430),
        weight_var = as.numeric(NA),
        count = c(1),
        institute = as.character(NA),
        gear = as.character(NA),
        sex = as.character(NA),
        maturity_stage = as.character(NA),
        species = rep(c('COD', 'HAD'), each = 3, times = 2),
        areacell = rep(c("45G01", "45G02", "45G03"), times = 4),
        sampling_type = rep(c('comm', 'res'), each = 6),
        data_source = c("ds2000"),
        tow = as.character(NA),
        vessel = as.character(NA),
        stringsAsFactors = FALSE)), "All taxonomy tables")
})
