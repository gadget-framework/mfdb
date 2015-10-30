# This script makes sure we haven't broken deprecated names
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

# Empty database
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('', db_params = db_params, destroy_schema = TRUE)

mdb <- mfdb('Test', db_params = db_params, save_temp_tables = FALSE)

ok_group("Old vessel_type name", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(5)))
    mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G01')))

    mfdb_import_survey(mdb,
        data_source = 'survey1',
        data.frame(
            institute = 'MRI',
            gear = 'GIL',
            vessel = '1.RSH',
            year = c('1998'),
            month = c(1:12),
            areacell = c('45G01'),
            species = c('COD'),
            age =    c(  1,  2,  1,  2,  1,  2,   1,  2,  1,  2,  1,  2),
            sex =    c('M','F','X','M','F','X', 'M','F','X','M','F','X'),
            length = c( 10, 50, 30, 10, 35, 46,  65, 62, 36, 35, 34, 22),
            weight = c(100,500,300,100,350,460, 650,320,360,350,340,220)))

    ok(cmp(
        mfdb_sample_meanlength(mdb, c("vessel"), list(
            vessel = mfdb_group("ship" = c('1.RSH')),
            area = mfdb_group(divA = c("divA")),
            timestep = mfdb_timestep_biannually,
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[['0.0.0.0']][,c("vessel_type", "step", "number", "mean")],
        data.frame(
            vessel_type = c("ship"),
            step = c("1", "2"),
            number = c(6, 6),
            mean = c(
                mean(c(10,50,30,10,35,46)),
                mean(c(65,62,36,35,34,22))),
            stringsAsFactors = FALSE)),
       "Can still use ")
})
