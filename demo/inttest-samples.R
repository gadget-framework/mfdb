# This script demonstrates various ways you can use the data sampling functions
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
mfdb('', destroy_schema = TRUE)

#TODO: Connecting to empty database without ability to populate fails

# Rebuild database, taxonomy got populated
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = TRUE)
ok(all(mfdb:::mfdb_fetch(mdb, "SELECT name, description FROM species WHERE species_id = 9999999999")[1,] == 
  mfdb::species[mfdb::species$name == 'TBX', c('name', 'description')]), "Entry for 9999999999 matches package")
ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM species")[1,1], nrow(mfdb::species)), "Species has right number of entries")

ok_group("Unaggregated length / weight / age samples", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(5)))
    mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G01')))

    # Import a survey
    mfdb_import_survey(mdb,
        data_source = 'survey1',
        data.frame(
            Year = c('1998'),  # NB: Not case-sensitive
            mOnth = c(1:12),
            areacell = c('45G01'),
            species = c('COD'),
            age =    c(  1,  2,  1,  2,  1,  2,   1,  2,  1,  2,  1,  2),
            length = c( 10, 50, 30, 10, 35, 46,  65, 62, 36, 35, 34, 22),
            weight = c(100,500,300,100,350,460, 650,320,360,350,340,220)))

    # Aggregate lengths
    area_group <- mfdb_group(divA = c("divA"))
    age_group <- mfdb_group(all = 1:1000)
    length_group <- mfdb_interval("len", seq(0, 50, by = 5))
    ok(cmp(
        mfdb_sample_meanlength(mdb, c('age'), list(
            year = 1998:2000,
            area = area_group,
            timestep = mfdb_timestep_biannually,
            age = age_group,
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
            year = 1998:2000,
            timestep = mfdb_timestep_biannually,
            area = area_group,
            age = age_group,
            generator = "mfdb_sample_meanlength"))),
       "Aggregated length data")

    # mfdb_sample_meanlength_stddev is the same, but with an extra column
    ok(cmp(
        mfdb_sample_meanlength_stddev(mdb, c('age'), list(
            year = 1998:2000,
            area = area_group,
            timestep = mfdb_timestep_biannually,
            age = age_group,
            length = length_group)),
        list("0.0.0.0" = structure(
            data.frame(
                year = c(1998:1998),
                step = c("1", "2"),
                area = c("divA"),
                age = c("all"),
                number = c(5, 4),
                mean = c(26.2, 31.75),
                stddev = c(0), # TODO:
                stringsAsFactors = FALSE),
            year = 1998:2000,
            timestep = mfdb_timestep_biannually,
            area = area_group,
            age = age_group,
            generator = "mfdb_sample_meanlength_stddev"))),
       "Aggregated length data (with stddev)")

    # mfdb_sample_meanweight aggregates weight, but still use length as a filter
    length_group <- mfdb_interval("len", seq(50, 100, by = 5))
    ok(cmp(
        mfdb_sample_meanweight(mdb, c('age'), list(
            year = 1998:2000,
            area = area_group,
            timestep = mfdb_timestep_biannually,
            age = age_group,
            length = length_group)),
        list("0.0.0.0" = structure(
            data.frame(
                year = c(1998:1998),
                step = c("1", "2"),
                area = c("divA"),
                age = c("all"),
                number = c(1, 2),
                mean = c(500, 485),
                stringsAsFactors = FALSE),
            year = 1998:2000,
            timestep = mfdb_timestep_biannually,
            area = area_group,
            age = age_group,
            generator = "mfdb_sample_meanweight"))),
       "Aggregated weight data")

    # mfdb_sample_meanweight_stddev works the same, but with stddev
    length_group <- mfdb_interval("len", seq(50, 100, by = 5))
    ok(cmp(
        mfdb_sample_meanweight_stddev(mdb, c('age'), list(
            year = 1998:2000,
            area = area_group,
            timestep = mfdb_timestep_biannually,
            age = age_group,
            length = length_group)),
        list("0.0.0.0" = structure(
            data.frame(
                year = c(1998:1998),
                step = c("1", "2"),
                area = c("divA"),
                age = c("all"),
                number = c(1, 2),
                mean = c(500, 485),
                stddev = c(0), # TODO:
                stringsAsFactors = FALSE),
            year = 1998:2000,
            timestep = mfdb_timestep_biannually,
            area = area_group,
            age = age_group,
            generator = "mfdb_sample_meanweight_stddev"))),
       "Aggregated weight data (with stddev)")

    # Can use step interval to produce same result
    ok(cmp(
        mfdb_sample_meanweight_stddev(mdb, c('age'), list(
            year = 1998:2000,
            area = area_group,
            timestep = mfdb_timestep_biannually,
            age = age_group,
            length = mfdb_interval("len", seq(50, 100, by = 5)))),
        mfdb_sample_meanweight_stddev(mdb, c('age'), list(
            year = 1998:2000,
            area = area_group,
            timestep = mfdb_timestep_biannually,
            age = age_group,
            length = mfdb_step_interval("len", from = 50, to = 100, by = 5)))),
        "Can use either mfdb_interval or mfdb_step_interval")

    # Age / length splits by age
    length_group <- mfdb_interval("len", seq(50, 100, by = 5))
    age_group <- mfdb_group(age1 = c(1), age2 = c(2))
    ok(cmp(
        mfdb_sample_count(mdb, c('age', 'length'), list(
            year = 1998:2000,
            area = area_group,
            timestep = mfdb_timestep_biannually,
            age = age_group,
            length = length_group)),
        list("0.0.0.0.0" = structure(
            data.frame(
                year = c(1998, 1998, 1998),
                step = c("1", "2", "2"),
                area = c("divA", "divA", "divA"),
                age = c("age2", "age1", "age2"),
                length = c("len50", "len65", "len60"),
                number = c(1, 1, 1),
                stringsAsFactors = FALSE),
            year = 1998:2000,
            timestep = mfdb_timestep_biannually,
            area = area_group,
            age = age_group,
            length = length_group,
            generator = "mfdb_sample_count"))),
       "Aggregated agelength data")
})

ok_group("Filtering of samples", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(5)))
    mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G01')))

    # Set up sampling types
    mfdb_import_sampling_type(mdb, data.frame(id = 1:2, name = c("SEA", "MOO")))

    # Import several surveys with different metadata set
    mfdb_import_survey(mdb,
        data_source = 'survey1',
        institute = 'MRI',
        gear = 'GIL',
        vessel = '1.RSH',
        sampling_type = 'SEA',
        data.frame(
            year = c('1998'),
            month = c(1:12),
            areacell = c('45G01'),
            species = c('COD'),
            age =    c(  1,  2,  1,  2,  1,  2,   1,  2,  1,  2,  1,  2),
            sex =    c('M','F','X','M','F','X', 'M','F','X','M','F','X'),
            length = c( 10, 50, 30, 10, 35, 46,  65, 62, 36, 35, 34, 22),
            weight = c(100,500,300,100,350,460, 650,320,360,350,340,220)))
    mfdb_import_survey(mdb,
        data_source = 'survey2',
        institute = 'ICES',
        gear = 'DSE',
        vessel = '2.RSH',
        sampling_type = 'SEA',
        data.frame(
            year = c('1998'),
            month = c(1:12),
            areacell = c('45G01'),
            species = c('HAD'),
            age =    c(  1,  2,  1,  2,  1,  2,   1,  2,  1,  2,  1,  2),
            sex =    c('M','F','X','M','F','X', 'M','F','X','M','F','X'),
            length = c( 35, 64, 23, 13, 99, 83,  54, 23, 65, 12, 22,  9),
            weight = c(110,510,310,110,310,410, 610,310,310,310,310,230)))

    # MOO sampling_type has no dat
    ok(cmp(
        mfdb_sample_meanlength(mdb, c(), list(
            sampling_type = 'MOO',
            area = mfdb_group(divA = c("divA")),
            timestep = mfdb_timestep_biannually,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000)),
            list()),
        "sampling_type MOO empty")

    # Without any aggregation on, we get the whole lot
    ok(cmp(
        mfdb_sample_meanlength(mdb, c(), list(
            area = mfdb_group(divA = c("divA")),
            timestep = mfdb_timestep_biannually,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number", "mean")],
        data.frame(
            step = c("1", "2"),
            number = c(12, 12),
            mean = c(
                mean(c(10,50,30,10,35,46, 35,64,23,13,99,83)),
                mean(c(65,62,36,35,34,22, 54,23,65,12,22,9))),
            stringsAsFactors = FALSE)),
       "No filtering, got everything")
    ok(cmp(
        mfdb_sample_meanlength(mdb, c(), list(
            sampling_type = 'SEA',
            area = mfdb_group(divA = c("divA")),
            timestep = mfdb_timestep_biannually,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number", "mean")],
        data.frame(
            step = c("1", "2"),
            number = c(12, 12),
            mean = c(
                mean(c(10,50,30,10,35,46, 35,64,23,13,99,83)),
                mean(c(65,62,36,35,34,22, 54,23,65,12,22,9))),
            stringsAsFactors = FALSE)),
       "SEA in both, got everything")

    # Filtering by gear,institute, vessel or sampling_type will break down to one or other
    ok(cmp(
        mfdb_sample_meanlength(mdb, c(), list(
            gear = 'GIL',
            area = mfdb_group(divA = c("divA")),
            timestep = mfdb_timestep_biannually,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number", "mean")],
        data.frame(
            step = c("1", "2"),
            number = c(6, 6),
            mean = c(
                mean(c(10,50,30,10,35,46)),
                mean(c(65,62,36,35,34,22))),
            stringsAsFactors = FALSE)),
       "GIL means survey1")
    ok(cmp(
        mfdb_sample_meanlength(mdb, c(), list(
            vessel = '1.RSH',
            area = mfdb_group(divA = c("divA")),
            timestep = mfdb_timestep_biannually,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number", "mean")],
        data.frame(
            step = c("1", "2"),
            number = c(6, 6),
            mean = c(
                mean(c(10,50,30,10,35,46)),
                mean(c(65,62,36,35,34,22))),
            stringsAsFactors = FALSE)),
       "1.RSH means survey1")
    ok(cmp(
        mfdb_sample_meanlength(mdb, c(), list(
            institute = 'ICES',
            area = mfdb_group(divA = c("divA")),
            timestep = mfdb_timestep_biannually,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number", "mean")],
        data.frame(
            step = c("1", "2"),
            number = c(6, 6),
            mean = c(
                mean(c(35,64,23,13,99,83)),
                mean(c(54,23,65,12,22,9))),
            stringsAsFactors = FALSE)),
       "ICES means survey2")
    ok(cmp(
        mfdb_sample_meanlength(mdb, c(), list(
            vessel = '2.RSH',
            area = mfdb_group(divA = c("divA")),
            timestep = mfdb_timestep_biannually,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number", "mean")],
        data.frame(
            step = c("1", "2"),
            number = c(6, 6),
            mean = c(
                mean(c(35,64,23,13,99,83)),
                mean(c(54,23,65,12,22,9))),
            stringsAsFactors = FALSE)),
       "2.RSH means survey2")

    # Intersection gives nothing
    ok(cmp(
        mfdb_sample_meanlength(mdb, c(), list(
            gear = 'GIL',
            vessel = '2.RSH',
            area = mfdb_group(divA = c("divA")),
            timestep = mfdb_timestep_biannually,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number", "mean")],
        NULL),
       "GIL & 2.RSH returns nothing")

    # Should be able to re-import sampling types
    mfdb_import_sampling_type(mdb, data.frame(name = c("SEA", "MOO"), description = c("Sea", "Seacow")))
    ok(cmp(
        mfdb_sample_meanlength(mdb, c(), list(
            vessel = '2.RSH',
            area = mfdb_group(divA = c("divA")),
            timestep = mfdb_timestep_biannually,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number", "mean")],
        data.frame(
            step = c("1", "2"),
            number = c(6, 6),
            mean = c(
                mean(c(35,64,23,13,99,83)),
                mean(c(54,23,65,12,22,9))),
            stringsAsFactors = FALSE)),
       "Data still exists")
})
