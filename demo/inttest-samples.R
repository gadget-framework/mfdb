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
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('Test', db_params = db_params, destroy_schema = TRUE)

#TODO: Connecting to empty database without ability to populate fails

# Rebuild database, taxonomy got populated
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = FALSE)
ok(all(mfdb:::mfdb_fetch(mdb, "SELECT name, description FROM species WHERE species_id = 9999999999")[1,] == 
  mfdb::species[mfdb::species$name == 'TBX', c('name', 'description')]), "Entry for 9999999999 matches package")
ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM species")[1,1], nrow(mfdb::species)), "Species has right number of entries")

ok_group("Unaggregated length / weight / age samples", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(5)))
    mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G01')))

    # Set up the vessels we use in this example
    mfdb_import_vessel_taxonomy(mdb, data.frame(
        name = c('1.RSH', '2.RSH'),
        vessel_type = c('1.RSH', '2.RSH'),
        stringsAsFactors = FALSE
    ))

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
            data_source = "survey1",
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
            year = as.list(structure(1998:2000, names = 1998:2000)),
            step = mfdb_timestep_biannually,
            area = area_group,
            age = age_group,
            generator = "mfdb_sample_meanlength"))),
       "Aggregated length data")

    ok(cmp(
        mfdb_sample_rawdata(mdb, c('age'), list(
            year = 1998:2000,
            area = area_group,
            timestep = mfdb_timestep_biannually,
            data_source = "survey1",
            age = mfdb_group(all = 1),
            length = length_group))[[1]][,'weight'],
        c(100,300,350,360,340)), "Fetched raw weight data for age 1, length less than 50")

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
                stddev = c(15.880806, 6.551081),
                stringsAsFactors = FALSE),
            year = as.list(structure(1998:2000, names = 1998:2000)),
            step = mfdb_timestep_biannually,
            area = area_group,
            age = age_group,
            generator = "mfdb_sample_meanlength_stddev"))),
       "Aggregated length data (with stddev)")

    # mfdb_sample_totalweight aggregates weight, but still use length as a filter
    length_group <- mfdb_interval("len", seq(50, 100, by = 5))
    ok(cmp(
        mfdb_sample_totalweight(mdb, c('age'), list(
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
                total_weight = c(500, 970),
                stringsAsFactors = FALSE),
            year = as.list(structure(1998:2000, names = 1998:2000)),
            step = mfdb_timestep_biannually,
            area = area_group,
            age = age_group,
            generator = "mfdb_sample_totalweight"))),
       "Aggregated weight and got total")

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
            year = as.list(structure(1998:2000, names = 1998:2000)),
            step = mfdb_timestep_biannually,
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
                stddev = c(NA, 233.3452),
                stringsAsFactors = FALSE),
            year = as.list(structure(1998:2000, names = 1998:2000)),
            step = mfdb_timestep_biannually,
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
            step = mfdb_timestep_biannually,
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
            year = as.list(structure(1998:2000, names = 1998:2000)),
            step = mfdb_timestep_biannually,
            area = area_group,
            age = age_group,
            length = agg_summary(mdb, length_group, 'col', 'out', data.frame(), 0),
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
        data.frame(
            institute = 'MRI',
            gear = 'GIL',
            vessel = '1.RSH',
            sampling_type = 'SEA',
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
        data.frame(
            institute = 'ICES',
            gear = 'DSE',
            vessel = '2.RSH',
            sampling_type = 'SEA',
            year = c('1998'),
            month = c(1:12),
            areacell = c('45G01'),
            species = c('HAD'),
            age =    c(  1,  2,  1,  2,  1,  2,   1,  2,  1,  2,  1,  2),
            sex =    c('M','F','X','M','F','X', 'M','F','X','M','F','X'),
            length = c( 35, 64, 23, 13, 99, 83,  54, 23, 65, 12, 22,  9),
            weight = c(110,510,310,110,310,410, 610,310,310,310,310,230)))

    # MOO sampling_type has no dat
    area_group <- mfdb_group(divA = c("divA"))
    ok(cmp(
        mfdb_sample_meanlength(mdb, c(), list(
            sampling_type = 'MOO',
            area = area_group,
            timestep = mfdb_timestep_biannually,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000)),
            list("0.0.0" = structure(
                data.frame(),
                year = list("1998" = 1998, "1999" = 1999, "2000" = 2000),
                step = mfdb_timestep_biannually,
                area = area_group,
                generator = "mfdb_sample_meanlength"))),
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
        nrow(mfdb_sample_meanlength(mdb, c(), list(
            gear = 'GIL',
            vessel = '2.RSH',
            area = mfdb_group(divA = c("divA")),
            timestep = mfdb_timestep_biannually,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]]),
        0),
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

    # Should be able to group by species
    ok(cmp(
        mfdb_sample_meanlength(mdb, c('species'), list(
            species = c('COD', 'HAD'),
            year = 1998:2000))[["0.0.0.0"]][,c("year", "step", "area", "species", "number", "mean")],
        data.frame(
            year = c(1998, 1998),
            step = c("all", "all"),
            area = c("all", "all"),
            species = c("COD", "HAD"),
            number = c(12, 12),
            mean = c(
                mean(c( 10, 50, 30, 10, 35, 46,  65, 62, 36, 35, 34, 22)),
                mean(c( 35, 64, 23, 13, 99, 83,  54, 23, 65, 12, 22,  9))),
            stringsAsFactors = FALSE)),
       "Can group by species (COD & HAD)")
    ok(cmp(
        mfdb_sample_meanlength(mdb, c('species'), list(
            species = c('COD'),
            year = 1998:2000))[["0.0.0.0"]][,c("year", "step", "area", "species", "number", "mean")],
        data.frame(
            year = c(1998),
            step = c("all"),
            area = c("all"),
            species = c("COD"),
            number = c(12),
            mean = c(
                mean(c( 10, 50, 30, 10, 35, 46,  65, 62, 36, 35, 34, 22))),
            stringsAsFactors = FALSE)),
       "Can group by species (just COD)")
    ok(cmp(
        mfdb_sample_meanlength(mdb, c('species'), list(
            species = mfdb_group(codhad = c('COD', 'HAD')),
            year = 1998:2000))[["0.0.0.0"]][,c("year", "step", "area", "species", "number", "mean")],
        data.frame(
            year = c(1998),
            step = c("all"),
            area = c("all"),
            species = c("codhad"),
            number = c(24),
            mean = mean(c(
                    c( 10, 50, 30, 10, 35, 46,  65, 62, 36, 35, 34, 22),
                    c( 35, 64, 23, 13, 99, 83,  54, 23, 65, 12, 22,  9),
                    NULL)),
            stringsAsFactors = FALSE)),
       "Can group by species (COD & HAD combined)")
    ok(cmp_error(
        mfdb_sample_meanlength(mdb, c('species'), list(
            species = mfdb_group(codhad = c('COD', 'HAD'), hand = c('HAND')),
            year = 1998:2000)), "HAND"), "Notice and report items not in vocabulary")
})

ok_group("Invalid parameters", {
    ok(cmp_error(mfdb_sample_count(mdb, c(), "camel"), "params"), "Complained about non-list params")
    ok(cmp(
        mfdb_sample_count(mdb, c(), list(
            camelcamelcamel = "No thanks",
            vessel = '2.RSH',
            area = mfdb_group(divA = c("divA")),
            timestep = mfdb_timestep_biannually,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number")],
        data.frame(
            step = c("1", "2"),
            number = c(6, 6),
            stringsAsFactors = FALSE)),
       "Useless camelcamelcamel parameter ignored")

    ok(cmp_error(
        mfdb_sample_count(mdb, c("camelcamelcamel"), list(
            camelcamelcamel = "No thanks",
            vessel = '2.RSH',
            area = mfdb_group(divA = c("divA")),
            timestep = mfdb_timestep_biannually,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number")],
        "camelcamelcamel"),
       "Cannot use camelcamelcamel as a column")
})

ok_group("Invalid import", {
    ok(cmp_error(
        mfdb_import_survey(mdb,
            data_source = 'survey1',
            data.frame(
                Year = c('1998'),  # NB: Not case-sensitive
                month = c(1:6, 13, 28, 9, 10, 0, 12),
                areacell = c('45G01'),
                species = c('COD'),
                age =    c(  1,  2,  1,  2,  1,  2,   1,  2,  1,  2,  1,  2),
                length = c( 10, 50, 30, 10, 35, 46,  65, 62, 36, 35, 34, 22),
                weight = c(101,500,300,100,350,460, 650,320,360,350,340,220))),
        "13,28,0"), "Notice when month isn't within 1..12")
})

ok_group("Removing an import", {
    # Can insert nothing without an error
    mfdb_import_survey(mdb, data_source = 'empty_survey', data.frame())
    ok(cmp(
        unattr(mfdb_sample_count(mdb, c(), list(year = c('1998'), data_source = 'empty_survey'))[['0.0.0']]),
        data.frame()),
       "No data available")

    # Importing data still works
    mfdb_import_survey(mdb,
        data_source = 'empty_survey',
        data.frame(
            Year = c('1998'),
            month = c(1:12),
            areacell = c('45G01'),
            species = c('COD'),
            age =    c(  1,  2,  1,  2,  1,  2,   1,  2,  1,  2,  1,  2),
            length = c( 10, 50, 30, 10, 35, 46,  65, 62, 36, 35, 34, 22),
            weight = c(100,500,300,100,350,460, 650,320,360,350,340,220)))
    ok(cmp(
        unattr(mfdb_sample_count(mdb, c(), list(year = c('1998'), data_source = 'empty_survey'))[['0.0.0']]),
        data.frame(year = as.integer(1998), step = 'all', area = 'all', number = 12, stringsAsFactors = FALSE)),
       "empty_survey content imported")

    # Gone again
    mfdb_import_survey(mdb, data_source = 'empty_survey', data.frame())
    ok(cmp(
        unattr(mfdb_sample_count(mdb, c(), list(year = c('1998'), data_source = 'empty_survey'))[['0.0.0']]),
        data.frame()),
       "Data removed again")
})
