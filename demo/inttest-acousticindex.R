# This script demonstrates generating an acoustic index
#
# ok and ok_group are there so we can run this code and verify it is correct,
# and not needed every-day use.
#
# NB: if you want to run this, you must be using mfdb-workspace, also be warned
# any data stored in your database will be destroyed.
library(unittest)
library(mfdb)
source('mfdb/tests/utils/helpers.R')
source('mfdb/tests/utils/inttest-helpers.R')

avg <- function (...) {
    mean(c(...))
}

# Empty database
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('', db_params = db_params, destroy_schema = TRUE)

# Rebuild database, taxonomy got populated
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = FALSE)
ok(all(mfdb:::mfdb_fetch(mdb, "SELECT name, description FROM species WHERE species_id = 9999999999")[1,] == 
  mfdb::species[mfdb::species$name == 'TBX', c('name', 'description')]), "Entry for 9999999999 matches package")
ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM species")[1,1], nrow(mfdb::species)), "Species has right number of entries")

# Gadget directory
gd <- gadget_directory(tempfile())

ok_group("Generating an acoustic index likelihood component", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(10,200,400)))
    mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G03')))

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
