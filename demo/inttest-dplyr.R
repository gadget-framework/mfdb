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
source('tests/utils/helpers.R')
source('tests/utils/inttest-helpers.R')

# Empty database & rebuild
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb(gsub("inttest", "inttest-dplyr", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb(gsub("inttest", "inttest-dplyr", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, save_temp_tables = TRUE) # TODO:

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(
    name = c('45G01', '45G02', '45G03'),
    division = c('divA', 'divA', 'divB'),
    size = c(10,200,400)))

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
        year = as.integer(2000),
        month = as.integer(1),
        age = as.numeric(c(2, 3, 3, 1, 1, 2, 2, 3, 3, 1, 1, 2)),
        length = c(21, 34, 34, 62, 53, 54, 28, 34, 24, 12, 44, 14),
        length_var = as.integer(NA),
        length_min = as.integer(NA),
        weight = c(210, 220, 230, 320, 330, 430, 210, 220, 230, 320, 330, 430),
        weight_var = as.numeric(NA),
        liver_weight = as.numeric(NA),
        liver_weight_var = as.numeric(NA),
        gonad_weight = as.numeric(NA),
        gonad_weight_var = as.numeric(NA),
        stomach_weight = as.numeric(NA),
        stomach_weight_var = as.numeric(NA),
        gutted_weight = as.numeric(NA),
        gutted_weight_var = as.numeric(NA),
        count = c(1),
        stringsAsFactors = FALSE)), "No taxonomy tables")

    ok(cmp(to_df(mfdb_dplyr_sample(mdb, c('sampling_type', 'length', 'species'))), data.frame(
        year = as.integer(2000),
        month = as.integer(1),
        age = as.numeric(c(2, 3, 3, 1, 1, 2, 2, 3, 3, 1, 1, 2)),
        length = c(21, 34, 34, 62, 53, 54, 28, 34, 24, 12, 44, 14),
        length_var = as.integer(NA),
        length_min = as.integer(NA),
        weight = c(210, 220, 230, 320, 330, 430, 210, 220, 230, 320, 330, 430),
        weight_var = as.numeric(NA),
        liver_weight = as.numeric(NA),
        liver_weight_var = as.numeric(NA),
        gonad_weight = as.numeric(NA),
        gonad_weight_var = as.numeric(NA),
        stomach_weight = as.numeric(NA),
        stomach_weight_var = as.numeric(NA),
        gutted_weight = as.numeric(NA),
        gutted_weight_var = as.numeric(NA),
        count = c(1),
        sampling_type = rep(c('comm', 'res'), each = 6),
        species = rep(c('COD', 'HAD'), each = 3, times = 2),
        stringsAsFactors = FALSE)), "Select taxonomy tables")

    df <- to_df(mfdb_dplyr_sample(mdb))
    if (mfdb:::mfdb_is_sqlite(mdb)) df$age <- as.numeric(df$age)
    ok(ut_cmp_identical(
       df[c('year', 'month', 'age', 'weight', 'areacell', 'areacell_size', 'sampling_type', 'species')],
       data.frame(
           year = as.integer(2000),
           month = as.integer(1),
           age = as.numeric(c(2, 3, 3, 1, 1, 2, 2, 3, 3, 1, 1, 2)),
           weight = c(210, 220, 230, 320, 330, 430, 210, 220, 230, 320, 330, 430),
           areacell = rep(c("45G01", "45G02", "45G03"), times = 4),
           areacell_size = rep(c(10, 200, 400), times = 4),
           sampling_type = rep(c('comm', 'res'), each = 6),
           species = rep(c('COD', 'HAD'), each = 3, times = 2),
           stringsAsFactors = FALSE)), "All available, non-NA data matches")
    ok(setequal(names(df), c(
        "year", "month", "age", "length", "length_var", "length_min",
        "weight", "weight_var", "liver_weight", "liver_weight_var", "gonad_weight",
        "gonad_weight_var", "stomach_weight", "stomach_weight_var", "gutted_weight",
        "gutted_weight_var", "count", "areacell", "areacell_size", "areacell_depth",
        "data_source", "gear", "gear_mesh_size", "gear_mesh_size_min",
        "gear_mesh_size_max", "institute", "maturity_stage", "population",
        "sampling_type", "sex", "species", "tow", "tow_latitude", "tow_longitude",
        "tow_end_latitude", "tow_end_longitude", "tow_start", "tow_depth",
        "tow_length", "tow_duration", "tow_hook_count", "tow_net_count",
        "tow_mesh_size", "tow_bait_type", "tow_net_type", "trip", "trip_start_date",
        "trip_end_date", "trip_crew", "trip_oil_consumption", "trip_end_port",
        "trip_end_port_latitude", "trip_end_port_longitude", "trip_end_port_institute",
        "trip_start_port", "trip_start_port_latitude", "trip_start_port_longitude",
        "trip_start_port_institute", "vessel", "vessel_full_name", "vessel_length",
        "vessel_power", "vessel_tonnage", "vessel_vessel_owner_full_name",
        NULL)), "All available, rest of column names match")
})

mfdb_disconnect(mdb)
