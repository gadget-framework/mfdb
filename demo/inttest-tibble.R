# This script demonstrates various ways you can use the data sampling functions
#
# ok and ok_group are there so we can run this code and verify it is correct,
# and not needed every-day use.
#
# NB: if you want to run this, you must be using mfdb-workspace, also be warned
# any data stored in your database will be destroyed.
library(unittest)
library(mfdb)
library(tibble)
source('tests/utils/helpers.R')
source('tests/utils/inttest-helpers.R')

# Empty database
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('inttest-tibble', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('inttest-tibble', db_params = db_params, save_temp_tables = FALSE)

ok_group("Import tibble", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(
        name = c('45G01', '45G02', '45G03'),
        division = c('divA', 'divA', 'divB'),
        size = c(5)))

    # Set up the vessels we use in this example
    mfdb_import_vessel_taxonomy(mdb, data.frame(
        name = c('1.RSH', '2.RSH'),
        vessel_type = c('1.RSH', '2.RSH'),
        stringsAsFactors = FALSE
    ))

    # Import a survey as a tibble, not data.table
    mfdb_import_survey(mdb,
        data_source = 'surveytibble',
        tibble(
            year = c('1998'),
            month = c(1:12),
            areacell = c('45G01'),
            species = c('COD'),
            age =    c(  1,2.1,  1,2.2,  1,2.1,   1,2.2,  1,2.1,  1,2.2),
            length = c( 10, 50, 30, 10, 35, 46,  65, 62, 36, 35, 34, 22),
            weight = c(100,500,300,100,350,460, 650,320,360,350,340,220)))

    # Aggregate lengths
    area_group <- mfdb_group(divA = c("divA"))
    length_group <- mfdb_interval("len", seq(0, 50, by = 5))
    ok(cmp(
        mfdb_sample_meanlength(mdb, c('age'), list(
            year = 1998:2000,
            area = area_group,
            timestep = mfdb_timestep_biannually,
            length = length_group)),
        list("0.0.0.0" = structure(
            data.frame(
                year = c(1998:1998),
                step = c("1", "2"),
                area = c("divA"),
                age = c("all"),
                number = c(5, 4),
                mean = c(26.2, 31.75),
                stringsAsFactors = FALSE),
            year = as.list(structure(1998:2000, names = 1998:2000)),
            step = mfdb_timestep_biannually,
            area = area_group,
            age = list(all = c(1, 2.2)),
            generator = "mfdb_sample_meanlength"))),
       "Aggregated length data")
})