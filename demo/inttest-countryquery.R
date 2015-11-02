# This script demonstrates how to query by an entire country
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

# Empty database & rebuild
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = FALSE)

mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(5)))
mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G01')))

# Import surveys for Norway as well as institutes within
mfdb_import_survey(mdb,
    data_source = 'survey_IMR',
    data.frame(
        year = c('1998'),
        month = c(1:6),
        areacell = c('45G01'),
        species = c('COD'),
        institute = c('IMR'),
        length = c( 10, 50, 30, 10, 35, 46)))
mfdb_import_survey(mdb,
    data_source = 'survey_UiB',
    data.frame(
        year = c('1998'),
        month = c(1:6),
        areacell = c('45G01'),
        species = c('COD'),
        institute = c('UiB'),
        length = c( 20, 40, 70, 40, 65, 96)))
mfdb_import_survey(mdb,
    data_source = 'survey_NOR',
    data.frame(
        year = c('1998'),
        month = c(1:6),
        areacell = c('45G01'),
        species = c('COD'),
        institute = c('NOR'),
        length = c(350,360,330,330,535,387)))
# Import for a few other countries
mfdb_import_survey(mdb,
    data_source = 'survey_GBR',
    data.frame(
        year = c('1998'),
        month = c(1:6),
        areacell = c('45G01'),
        species = c('COD'),
        institute = c('GBR'),
        length = c(420,400,470,440,485,476)))
mfdb_import_survey(mdb,
    data_source = 'survey_SWE',
    data.frame(
        year = c('1998'),
        month = c(1:6),
        areacell = c('45G01'),
        species = c('COD'),
        institute = c('SWE'),
        length = c(520,500,550,540,585,576)))

ok(cmp(
    unattr(mfdb_sample_meanlength(mdb, c(), list(
        year = 1998:2000,
        institute = c('UiB'),
        timestep = mfdb_timestep_yearly
    ))[["0.0.0"]]), data.frame(
        year = as.integer(1998),
        step = "1",
        area = "all",
        number = 6,
        mean = mean(c(
            c( 20, 40, 70, 40, 65, 96))),
        stringsAsFactors = FALSE
    )), "Querying for UiB gives us all 1 institute")

ok(cmp(
    unattr(mfdb_sample_meanlength(mdb, c(), list(
        year = 1998:2000,
        institute = c('NOR'),
        timestep = mfdb_timestep_yearly
    ))[["0.0.0"]]), data.frame(
        year = as.integer(1998),
        step = "1",
        area = "all",
        number = 3 * 6,
        mean = mean(c(
            c( 10, 50, 30, 10, 35, 46),
            c( 20, 40, 70, 40, 65, 96),
            c(350,360,330,330,535,387))),
        stringsAsFactors = FALSE
    )), "Querying for NOR gives us all 3 institutes")

ok(cmp(
    unattr(mfdb_sample_meanlength(mdb, c('institute'), list(
        year = 1998:2000,
        institute = mfdb_group(nordic = c('NOR', 'SWE'), mixup = c('UiB', 'GBR')),
        timestep = mfdb_timestep_yearly
    ))[["0.0.0.0"]]), data.frame(
        year = as.integer(1998),
        step = "1",
        area = "all",
        institute = c("mixup", "nordic"),
        number = c(2 * 6, 4 * 6),
        mean = c(
            mean(c(c( 20, 40, 70, 40, 65, 96), c(420,400,470,440,485,476))),
            mean(c(c( 10, 50, 30, 10, 35, 46), c( 20, 40, 70, 40, 65, 96), c(350,360,330,330,535,387), c(520,500,550,540,585,576))),
            NULL),
        stringsAsFactors = FALSE
    )), "Can also group together both countries and institutes")
