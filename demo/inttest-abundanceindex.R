# This script demonstrates using an abundance index
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

unattr <- function (obj) {
    attr(obj, "year") <- NULL
    attr(obj, "step") <- NULL
    attr(obj, "area") <- NULL
    attr(obj, "length") <- NULL
    attr(obj, "generator") <- NULL
    obj
}
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

ok_group("Using a survey_index as measure of abundance", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(10,200,400)))
    mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G03')))

    # Import data for each areacell
    mfdb_import_survey(mdb,
        data_source = 'cell1',
        data.frame(
            year = c('1998'),
            month = c(1:12),
            areacell = c('45G01'),
            species = c('COD'),
            #          -----Q1----- -----Q2----- -----Q3----- -----Q4-----
            length = c(111,251,331, 111,231,341, 161,261,331, 231,231,121 ),
            count =  c(  2,  1,  2,   1,  2,  1,   1,  2,  1,   2,  1,  2 )))
    mfdb_import_survey(mdb,
        data_source = 'cell2',
        data.frame(
            year = c('1998'),
            month = c(1:12),
            areacell = c('45G02'),
            species = c('COD'),
            #          -----Q1----- -----Q2----- -----Q3----- -----Q4-----
            length = c(112,352,332, 212,232,242, 362,262,132, 232,232,122 ),
            count =  c(  2,  1,  2,   1,  2,  1,   1,  2,  1,   2,  1,  2 )))
    mfdb_import_survey(mdb,
        data_source = 'cell3',
        data.frame(
            year = c('1998'),
            month = c(1:12),
            areacell = c('45G03'),
            species = c('COD'),
            #          -----Q1----- -----Q2----- -----Q3----- -----Q4-----
            length = c(213,253,333, 313,133,343, 163,363,233, 333,133,323 ),
            count =  c(  2,  1,  2,   1,  2,  1,   1,  2,  1,   2,  1,  2 )))

    # Create some index types
    mfdb_import_cs_taxonomy(mdb, 'index_type', data.frame(name = c(
        "acoustic",
        "guesswork",
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

    # Import some data for our guesswork index
    mfdb_import_survey_index(mdb, data_source = 'guesswork_index1', data.frame(
        index_type = 'guesswork',
        year = '1998',
        month = 1:12,
        areacell = '45G02',
        #              -----Q1----- -----Q2----- -----Q3----- -----Q4-----
        value =      c(112,111,110, 109,108,107, 106,105,104, 103,102,101 )))
    mfdb_import_survey_index(mdb, data_source = 'guesswork_index2', data.frame(
        index_type = 'guesswork',
        year = '1998',
        month = 1:12,
        areacell = '45G03',
        #              -----Q1----- -----Q2----- -----Q3----- -----Q4-----
        value =      c(124,123,122, 121,120,119, 118,117,116, 115,114,113 )))

    # Without setting an scale_index, we use the count from the raw data for totals and means
    area_group <- mfdb_group(divA = c("divA"))
    length_group <- mfdb_interval("len", seq(0, 500, by = 100))
    ok(cmp(
        unattr(mfdb_sample_meanlength(mdb, c('length'), list(
            year = 1998,
            area = area_group,
            timestep = mfdb_timestep_quarterly,
            length = length_group))[[1]]),
        data.frame(
                year = c(1998),
                step = as.character(c(1,1,1,2,2,2,3,3,3,4,4)),
                area = c("divA"),
                length = c(
                    "len100", "len200",         "len300",
                    "len100", "len200",         "len300",
                    "len100", "len200",         "len300",
                    "len100", "len200",
                    NULL),
                number = c(
                    2 + 2,    1,                2 + 1 + 2,
                    1,        2 + 1 + 2 + 1,    1,
                    1 + 1,    2 + 2,            1 + 1,
                    2 + 2,    2 + 1 + 2 + 1,
                    NULL),
                mean = c(
                    weighted.mean(c(111, 112), c(2, 2)), # len100 results in Q1
                    weighted.mean(c(251), c(1)), # len200 results in Q1
                    weighted.mean(c(331, 352, 332), c(2, 1, 2)), # len300 results in Q1

                    weighted.mean(c(111), c(1)), # len100 results in Q2 . . .
                    weighted.mean(c(231, 212, 232, 242), c(2, 1, 2, 1)),
                    weighted.mean(c(341), c(1)),

                    weighted.mean(c(161, 132), c(1, 1)),
                    weighted.mean(c(261, 262), c(2, 2)),
                    weighted.mean(c(331, 362), c(1, 1)),

                    weighted.mean(c(121, 122), c(2, 2)),
                    weighted.mean(c(231, 231, 232, 232), c(2, 1, 2, 1)),
                    NULL),
                stringsAsFactors = FALSE)), "Used acoustic index instead of count")

    # Use our 'acoustic' index instead of count
    area_group <- mfdb_group(divA = c("divA"))
    length_group <- mfdb_interval("len", seq(0, 500, by = 100))
    ok(cmp(
        unattr(mfdb_sample_meanlength(mdb, c('length'), list(
            year = 1998,
            area = area_group,
            timestep = mfdb_timestep_quarterly,
            length = length_group), scale_index = 'acoustic')[[1]]),
        data.frame(
                year = c(1998),
                step = as.character(c(1,1,1,2,2,2,3,3,3,4,4)),
                area = c("divA"),
                length = c(
                    "len100", "len200",         "len300",
                    "len100", "len200",         "len300",
                    "len100", "len200",         "len300",
                    "len100", "len200",
                    NULL),
                number = c(
                    2*12 + 2*24,  1*11,                     2*10 + 1*23 + 2*22,
                    1*9,          2*8 + 1*21 + 2*20 + 1*19, 1*7,
                    1*6 + 1*16,   2*5 + 2*17,               1*4 + 1*18,
                    2*1 + 2*13,   1*2 + 2*3 + 1*14 + 2*15,
                    NULL),
                mean = c(
                    weighted.mean(c(111, 112), c(2*12, 2*24)), # len100 results in Q1
                    weighted.mean(c(251), c(1*11)), # len200 results in Q1
                    weighted.mean(c(331, 352, 332), c(2*10, 1*23, 2*22)), # len300 results in Q1

                    weighted.mean(c(111), c(1*9)), # len100 results in Q2 . . .
                    weighted.mean(c(231, 212, 232, 242), c(2*8, 1*21, 2*20, 1*19)),
                    weighted.mean(c(341), c(1*7)),

                    weighted.mean(c(161, 132), c(1*6, 1*16)),
                    weighted.mean(c(261, 262), c(2*5, 2*17)),
                    weighted.mean(c(331, 362), c(1*4, 1*18)),

                    weighted.mean(c(121, 122), c(2*1, 2*13)),
                    weighted.mean(c(231, 231, 232, 232), c(1*2, 2*3, 1*14, 2*15)),
                    NULL),
                stringsAsFactors = FALSE)), "Used acoustic index instead of count")

    # Add another set of values for the same area. We use the mean of these values as abundance
    mfdb_import_survey_index(mdb, data_source = 'acoustic_index1_1', data.frame(
        index_type = 'acoustic',
        year = '1998',
        month = 1:12,
        areacell = '45G01',
        #              -----Q1----- -----Q2----- -----Q3----- -----Q4-----
        value =      c(52, 51, 50,  59, 58, 57,  56, 55, 54,  53, 52, 51     )))
    ok(cmp(
        unattr(mfdb_sample_meanlength(mdb, c('length'), list(
            year = 1998,
            area = area_group,
            timestep = mfdb_timestep_quarterly,
            length = length_group), scale_index = 'acoustic')[[1]]),
        data.frame(
                year = c(1998),
                step = as.character(c(1,1,1,2,2,2,3,3,3,4,4)),
                area = c("divA"),
                length = c(
                    "len100", "len200",         "len300",
                    "len100", "len200",         "len300",
                    "len100", "len200",         "len300",
                    "len100", "len200",
                    NULL),
                number = c(
                    2*avg(12,52) + 2*24,  1*avg(11,51),               2*avg(10,50) + 1*23 + 2*22,
                    1*avg(9,59),          2*avg(8,58) + 1*21 + 2*20 + 1*19, 1*avg(7,57),
                    1*avg(6,56) + 1*16,   2*avg(5,55) + 2*17,           1*avg(4,54) + 1*18,
                    2*avg(1,51) + 2*13,   1*avg(2,52) + 2*avg(3,53) + 1*14 + 2*15,
                    NULL),
                mean = c(
                    weighted.mean(c(111, 112), c(2*avg(12,52), 2*24)),
                    weighted.mean(c(251), c(1*avg(11,51))),
                    weighted.mean(c(331, 352, 332), c(2*avg(10,50), 1*23, 2*22)),

                    weighted.mean(c(111), c(1*avg(9,59))),
                    weighted.mean(c(231, 212, 232, 242), c(2*avg(8,58), 1*21, 2*20, 1*19)),
                    weighted.mean(c(341), c(1*avg(7,57))),

                    weighted.mean(c(161, 132), c(1*avg(6,56), 1*16)),
                    weighted.mean(c(261, 262), c(2*avg(5,55), 2*17)),
                    weighted.mean(c(331, 362), c(1*avg(4,54), 1*18)),

                    weighted.mean(c(121, 122), c(2*avg(1,51), 2*13)),
                    weighted.mean(c(231, 231, 232, 232), c(1*avg(2,52), 2*avg(3,53), 1*14, 2*15)),
                    NULL),
                stringsAsFactors = FALSE)), "Took mean of multiple index values")
})

ok_group("Missing values in a survey_index", {
    # Put holes in acoustic_index2, we then get holes in the return data
    mfdb_import_survey_index(mdb, data_source = 'acoustic_index2', data.frame(
        index_type = 'acoustic',
        year = '1998',
        areacell = '45G02',
        #              -----Q1----- -----Q2----- -----Q3-----
        month =      c( 1,  2,  3,   4,  5,       7,      9  ),
        value =      c(24, 23, 22,  21, 20,      18,     16  )))
    ok(cmp(
        unattr(mfdb_sample_meanlength(mdb, c('length'), list(
            year = 1998,
            area = area_group,
            timestep = mfdb_timestep_quarterly,
            length = length_group), scale_index = 'acoustic')[[1]]),
        data.frame(
                year = c(1998),
                step = as.character(c(1,1,1,2,2,2,3,3,3,4,4)),
                area = c("divA"),
                length = c(
                    "len100", "len200",         "len300",
                    "len100", "len200",         "len300",
                    "len100", "len200",         "len300",
                    "len100", "len200",
                    NULL),
                number = c(
                    2*avg(12,52) + 2*24, 1*avg(11,51),               2*avg(10,50) + 1*23 + 2*22,
                    1*avg(9,59),         2*avg(8,58) + 1*21 + 2*20,  1*avg(7,57),
                    1*avg(6,56) + 1*16,  2*avg(5,55),                1*avg(4,54) + 1*18,
                    2*avg(1,51),         1*avg(2,52) + 2*avg(3,53),
                    NULL),
                mean = c(
                    weighted.mean(c(111, 112), c(2*avg(12,52), 2*24)),
                    weighted.mean(c(251), c(1*avg(11,51))),
                    weighted.mean(c(331, 352, 332), c(2*avg(10,50), 1*23, 2*22)),

                    weighted.mean(c(111), c(1*avg(9,59))),
                    weighted.mean(c(231, 212, 232), c(2*avg(8,58), 1*21, 2*20)),
                    weighted.mean(c(341), c(1*avg(7,57))),

                    weighted.mean(c(161, 132), c(1*avg(6,56), 1*16)),
                    weighted.mean(c(261), c(2*avg(5,55))),
                    weighted.mean(c(331, 362), c(1*avg(4,54), 1*18)),

                    weighted.mean(c(121), c(2*avg(1,51))),
                    weighted.mean(c(231, 231), c(1*avg(2,52), 2*avg(3,53))),
                    NULL),
                stringsAsFactors = FALSE)), "Took mean of multiple index values")
})

ok_group("Importing unknown indices", {
    # Create some index types
    mfdb_import_cs_taxonomy(mdb, 'index_type', data.frame(name = c(
        "acoustic",
        "guesswork",
        NULL)))

    # Can't import an index that we haven't predefined
    ok(cmp_error(mfdb_import_survey_index(mdb, data_source = 'acoustic_index1', data.frame(
        index_type = 'magic',
        year = '1998',
        month = 1:12,
        areacell = '45G01',
        value = c(1),
        stringsAsFactors = FALSE
    )), "index_type"), "Noticed we used a made-up index")
})
