# This script demonstrates generating an acoustic index
#
# ok and ok_group are there so we can run this code and verify it is correct,
# and not needed every-day use.
#
# NB: if you want to run this, you must be using mfdb-workspace, also be warned
# any data stored in your database will be destroyed.
library(unittest)
library(mfdb)
source('tests/utils/helpers.R')
source('tests/utils/inttest-helpers.R')

avg <- function (...) {
    mean(c(...))
}

# Empty database
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb(gsub("inttest", "inttest-acousticindex", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb(gsub("inttest", "inttest-acousticindex", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, save_temp_tables = FALSE)

# Gadget directory
gd <- gadget_directory(tempfile())

ok_group("Generating an acoustic index likelihood component", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(
        name = c('45G01', '45G02', '45G03'),
        division = c('divA', 'divA', 'divB'),
        size = c(10,200,400)))

    # Create some index types
    mfdb_import_cs_taxonomy(mdb, 'index_type', data.frame(name = c(
        "acoustic",
        "effort",
        NULL)))

    # Import some data for an acoustic index
    mfdb_import_survey_index(mdb, data_source = 'acoustic_index1', data.frame(
        index_type = 'acoustic',
        year = '1998',
        month = 1:12,
        areacell = '45G01',
        #              -----Q1----- -----Q2----- -----Q3----- -----Q4-----
        value =      c(12, 11, 10,  9, 8, 7,     6, 5, 4,     3, 2, 1     )))
    mfdb_import_survey_index(mdb, data_source = 'acoustic_index2', data.frame(
        index_type = 'acoustic',
        year = '1998',
        month = 1:12,
        areacell = '45G02',
        #              -----Q1----- -----Q2----- -----Q3----- -----Q4-----
        value =      c(24, 23, 22,  21, 20, 19,  18, 17, 16,  15, 14, 13  )))
    mfdb_import_survey_index(mdb, data_source = 'effort_index3', data.frame(
        index_type = 'effort',
        year = '1998',
        month = 1:12,
        areacell = '45G02',
        #              -----Q1----- -----Q2----- -----Q3----- -----Q4-----
        value =      c(94, 93, 92,  91, 29, 99,  19, 97, 96,  95, 19, 19  )))

    # Query out mean for acoustic index by area
    area_group <- mfdb_group(divA = c("divA"))
    agg <- mfdb_survey_index_mean(mdb, cols = c('data_source'), list(
            index_type = 'acoustic',
            year = 1998,
            area = area_group,
            data_source = mfdb_unaggregated(),
            timestep = mfdb_timestep_quarterly))
    ok(cmp(agg[[1]][,names(agg[[1]])], data.frame(
        year = 1998,
        step = rep(as.character(1:4), each = 2),
        area = "divA",
        data_source = rep(c('acoustic_index1', 'acoustic_index2'), times = 2),
        mean = c(
            mean(c(12:10)), mean(c(24:22)),
            mean(c( 9: 7)), mean(c(21:19)),
            mean(c( 6: 4)), mean(c(18:16)),
            mean(c( 3: 1)), mean(c(15:13))),
        stringsAsFactors = FALSE)), "Got averaged readings")

    # Put this into a likelihood component
    gadget_dir_write(gd, gadget_likelihood_component(
        'surveyindices',
        sitype = 'acoustic',
        fittype = 'loglinearfit',
        stocknames = c('cuthbert', 'dibble'),
        surveynames = c('emily', 'arnold'),
        data = agg[[1]]))
    ok(cmp_file(gd, "likelihood",
        ver_string,
        "; ",
        "[component]",
        "name\tsurveyindices",
        "weight\t0",
        "type\tsurveyindices",
        "datafile\tData/surveyindices.surveyindices.acoustic",
        "sitype\tacoustic",
        "biomass\t0",
        "areaaggfile\tAggfiles/surveyindices.surveyindices.area.agg",
        "surveynames\temily\tarnold",
        "stocknames\tcuthbert\tdibble",
        "fittype\tloglinearfit",
        NULL), "Wrote likelihood component")
    ok(cmp_file(gd, "Data/surveyindices.surveyindices.acoustic",
        ver_string,
        "; -- data --",
        "; year\tstep\tarea\tdata_source\tmean",
        "1998\t1\tdivA\tacoustic_index1\t11",
        "1998\t1\tdivA\tacoustic_index2\t23",
        "1998\t2\tdivA\tacoustic_index1\t8",
        "1998\t2\tdivA\tacoustic_index2\t20",
        "1998\t3\tdivA\tacoustic_index1\t5",
        "1998\t3\tdivA\tacoustic_index2\t17",
        "1998\t4\tdivA\tacoustic_index1\t2",
        "1998\t4\tdivA\tacoustic_index2\t14",
        NULL), "Wrote likelihood component")

    # Can also query total
    area_group <- mfdb_group(divA = c("divA"))
    agg <- mfdb_survey_index_total(mdb, cols = c('data_source'), list(
            index_type = 'acoustic',
            year = 1998,
            area = area_group,
            data_source = mfdb_unaggregated(),
            timestep = mfdb_timestep_quarterly))
    ok(cmp(agg[[1]][,names(agg[[1]])], data.frame(
        year = 1998,
        step = rep(as.character(1:4), each = 2),
        area = "divA",
        data_source = rep(c('acoustic_index1', 'acoustic_index2'), times = 2),
        total = c(
            sum(c(12:10)), sum(c(24:22)),
            sum(c( 9: 7)), sum(c(21:19)),
            sum(c( 6: 4)), sum(c(18:16)),
            sum(c( 3: 1)), sum(c(15:13))),
        stringsAsFactors = FALSE)), "Got averaged readings")

    # Can scale by area_size
    agg_data <- mfdb_survey_index_mean(mdb, cols = c(), list(
            index_type = 'acoustic',
            year = 1998,
            area = area_group,
            timestep = mfdb_timestep_quarterly), scale_index = 'area_size')
    ok(ut_cmp_equal(unattr(agg_data[[1]]), table_string('
year step area   mean
# weighted.mean(c(12,11,10, 24,23,22), c(rep(10, 3), rep(200, 3)))
1998    1 divA 22.42857
# weighted.mean(c(9,8,7, 21,20,19), c(rep(10, 3), rep(200, 3)))
1998    2 divA 19.42857
1998    3 divA 16.42857
1998    4 divA 13.42857
    ', colClasses = c(NA, 'character', NA, NA)), tolerance = 1e-7), "Index scaled by area size (mean)")
    agg_data <- mfdb_survey_index_total(mdb, cols = c(), list(
            index_type = 'acoustic',
            year = 1998,
            area = area_group,
            timestep = mfdb_timestep_quarterly), scale_index = 'area_size')
    ok(ut_cmp_equal(unattr(agg_data[[1]]), table_string('
year step area total
# sum(c(c(12,11,10) * 10, c(24,23,22) * 200))
1998    1 divA 14130
# sum(c(c(9,8,7) * 10, c(21,20,19) * 200))
1998    2 divA 12240
1998    3 divA 10350
1998    4 divA 8460
    ', colClasses = c(NA, 'character', NA, NA))), "Index scaled by area size (total)")

    # Can scale by another index
    # NB: The fact that effort is only available for G02 restricts what data is returned
    agg_data <- mfdb_survey_index_total(mdb, cols = c(), list(
            index_type = 'acoustic',
            year = 1998,
            area = area_group,
            timestep = mfdb_timestep_quarterly), scale_index = 'effort')
    ok(ut_cmp_equal(unattr(agg_data[[1]]), table_string('
year step area total
# 24 * 94 + 23 * 93 + 22 * 92
1998    1 divA  6419
# 21 * 91 + 20 * 29 + 19 * 99
1998    2 divA  4372
1998    3 divA  3527
1998    4 divA  1938
    ', colClasses = c(NA, 'character', NA, NA))), "Index scaled by effort")
    agg_data <- mfdb_survey_index_mean(mdb, cols = c(), list(
            index_type = 'effort',
            year = 1998,
            area = area_group,
            timestep = mfdb_timestep_quarterly), scale_index = 'acoustic')
    ok(ut_cmp_equal(unattr(agg_data[[1]]), table_string('
year step area mean
# weighted.mean(c(94, 93, 92), c(24, 23, 22))
1998    1 divA  93.02899
1998    2 divA  72.86667
1998    3 divA  69.15686
1998    4 divA  46.14286
    ', colClasses = c(NA, 'character', NA, NA)), tolerance = 1e-7), "Index scaled by acoustic")
})

ok_group("Can remove survey_index", {
    mfdb_import_survey_index(mdb, data_source = 'acoustic_index1', data.frame())

    # Query out mean for acoustic index by area
    area_group <- mfdb_group(divA = c("divA"))
    agg <- mfdb_survey_index_mean(mdb, cols = c('data_source'), list(
            index_type = 'acoustic',
            year = 1998,
            area = area_group,
            data_source = mfdb_unaggregated(),
            timestep = mfdb_timestep_quarterly))
    ok(cmp(agg[[1]][,names(agg[[1]])], data.frame(
        year = as.integer(1998),
        step = as.character(1:4),
        area = "divA",
        data_source = c('acoustic_index2'),
        mean = c(
            mean(c(24:22)),
            mean(c(21:19)),
            mean(c(18:16)),
            mean(c(15:13))),
        stringsAsFactors = FALSE)), "acoustic_index1 no longer there")
})
