# Script to exercise mfdb
# NB: To use this you *must* be using mfdb-workspace and not mind loosing data
library(unittest)
library(mfdb)
source('mfdb/tests/utils/helpers.R')
source('mfdb/tests/utils/inttest-helpers.R')

# Empty database
conn <- db_connection()
remove_mfdb_tables(conn)

#TODO: Connecting to empty database without ability to populate fails

# Rebuild database, taxonomy got populated
mdb <- mfdb('Iceland', db_params = db_params, save_temp_tables = TRUE, create_schema = TRUE)
ok(all(mfdb:::mfdb_fetch(mdb, "SELECT name, description FROM species WHERE species_id = 9999999999")[1,] == 
  mfdb::species[mfdb::species$name == 'TBX', c('name', 'description')]), "Entry for 9999999999 matches package")
ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM species")[1,1], nrow(mfdb::species)), "Species has right number of entries")

section("Unaggregated length / weight / age samples", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(5)))
    mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G01')))

    # Import a survey
    mfdb_import_survey(mdb,
        data_source = 'survey1',
        data.frame(
            year = c('1998'),
            month = c(1:12),
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
        mfdb_meanlength(mdb, list(
            year = 1998:2000,
            area = area_group,
            timestep = step_halves,
            age = age_group,
            length = length_group)),
        list("0.0.0" = structure(
            data.frame(
                year = c(1998:1998),
                step = c("h1", "h2"),
                area = c("divA"),
                age = c("all"),
                number = c(5, 4),
                mean = c(26.2, 31.75),
                stringsAsFactors = FALSE),
            timestep = step_halves,
            area = area_group,
            age = age_group,
            generator = "mfdb_meanlength"))),
       "Aggregated length data")

    # mfdb_meanlength_stddev is the same, but with an extra column
    ok(cmp(
        mfdb_meanlength_stddev(mdb, list(
            year = 1998:2000,
            area = area_group,
            timestep = step_halves,
            age = age_group,
            length = length_group)),
        list("0.0.0" = structure(
            data.frame(
                year = c(1998:1998),
                step = c("h1", "h2"),
                area = c("divA"),
                age = c("all"),
                number = c(5, 4),
                mean = c(26.2, 31.75),
                stddev = c(0), # TODO:
                stringsAsFactors = FALSE),
            timestep = step_halves,
            area = area_group,
            age = age_group,
            generator = "mfdb_meanlength_stddev"))),
       "Aggregated length data (with stddev)")

    # mfdb_meanweight aggregates weight, but still use length as a filter
    length_group <- mfdb_interval("len", seq(50, 100, by = 5))
    ok(cmp(
        mfdb_meanweight(mdb, list(
            year = 1998:2000,
            area = area_group,
            timestep = step_halves,
            age = age_group,
            length = length_group)),
        list("0.0.0" = structure(
            data.frame(
                year = c(1998:1998),
                step = c("h1", "h2"),
                area = c("divA"),
                age = c("all"),
                number = c(1, 2),
                mean = c(500, 485),
                stringsAsFactors = FALSE),
            timestep = step_halves,
            area = area_group,
            age = age_group,
            generator = "mfdb_meanweight"))),
       "Aggregated weight data")

    # mfdb_meanweight_stddev works the same, but with stddev
    length_group <- mfdb_interval("len", seq(50, 100, by = 5))
    ok(cmp(
        mfdb_meanweight_stddev(mdb, list(
            year = 1998:2000,
            area = area_group,
            timestep = step_halves,
            age = age_group,
            length = length_group)),
        list("0.0.0" = structure(
            data.frame(
                year = c(1998:1998),
                step = c("h1", "h2"),
                area = c("divA"),
                age = c("all"),
                number = c(1, 2),
                mean = c(500, 485),
                stddev = c(0), # TODO:
                stringsAsFactors = FALSE),
            timestep = step_halves,
            area = area_group,
            age = age_group,
            generator = "mfdb_meanweight_stddev"))),
       "Aggregated weight data (with stddev)")

    # Age / length splits by age
    length_group <- mfdb_interval("len", seq(50, 100, by = 5))
    age_group <- mfdb_group(age1 = c(1), age2 = c(2))
    ok(cmp(
        mfdb_agelength(mdb, list(
            year = 1998:2000,
            area = area_group,
            timestep = step_halves,
            age = age_group,
            length = length_group)),
        list("0.0.0" = structure(
            data.frame(
                year = c(1998, 1998, 1998),
                step = c("h1", "h2", "h2"),
                area = c("divA", "divA", "divA"),
                age = c("age2", "age1", "age2"),
                length = c("len50", "len65", "len60"),
                number = c(1, 1, 1),
                stringsAsFactors = FALSE),
            timestep = step_halves,
            area = area_group,
            age = age_group,
            length = length_group,
            generator = "mfdb_agelength"))),
       "Aggregated agelength data")
})

section("Filtering of samples", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(5)))
    mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G01')))

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


    # Without any aggregation on, we get the whole lot
    ok(cmp(
        mfdb_meanlength(mdb, list(
            area = mfdb_group(divA = c("divA")),
            timestep = step_halves,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number", "mean")],
        data.frame(
            step = c("h1", "h2"),
            number = c(12, 12),
            mean = c(
                mean(c(10,50,30,10,35,46, 35,64,23,13,99,83)),
                mean(c(65,62,36,35,34,22, 54,23,65,12,22,9))),
            stringsAsFactors = FALSE)),
       "No filtering, got everything")
    ok(cmp(
        mfdb_meanlength(mdb, list(
            sampling_type = 'SEA',
            area = mfdb_group(divA = c("divA")),
            timestep = step_halves,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number", "mean")],
        data.frame(
            step = c("h1", "h2"),
            number = c(12, 12),
            mean = c(
                mean(c(10,50,30,10,35,46, 35,64,23,13,99,83)),
                mean(c(65,62,36,35,34,22, 54,23,65,12,22,9))),
            stringsAsFactors = FALSE)),
       "SEA in both, got everything")

    # Filtering by gear,institute, vessel or sampling_type will break down to one or other
    ok(cmp(
        mfdb_meanlength(mdb, list(
            gear = 'GIL',
            area = mfdb_group(divA = c("divA")),
            timestep = step_halves,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number", "mean")],
        data.frame(
            step = c("h1", "h2"),
            number = c(6, 6),
            mean = c(
                mean(c(10,50,30,10,35,46)),
                mean(c(65,62,36,35,34,22))),
            stringsAsFactors = FALSE)),
       "GIL means survey1")
    ok(cmp(
        mfdb_meanlength(mdb, list(
            vessel = '1.RSH',
            area = mfdb_group(divA = c("divA")),
            timestep = step_halves,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number", "mean")],
        data.frame(
            step = c("h1", "h2"),
            number = c(6, 6),
            mean = c(
                mean(c(10,50,30,10,35,46)),
                mean(c(65,62,36,35,34,22))),
            stringsAsFactors = FALSE)),
       "1.RSH means survey1")
    ok(cmp(
        mfdb_meanlength(mdb, list(
            institute = 'ICES',
            area = mfdb_group(divA = c("divA")),
            timestep = step_halves,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number", "mean")],
        data.frame(
            step = c("h1", "h2"),
            number = c(6, 6),
            mean = c(
                mean(c(35,64,23,13,99,83)),
                mean(c(54,23,65,12,22,9))),
            stringsAsFactors = FALSE)),
       "ICES means survey2")
    ok(cmp(
        mfdb_meanlength(mdb, list(
            vessel = '2.RSH',
            area = mfdb_group(divA = c("divA")),
            timestep = step_halves,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number", "mean")],
        data.frame(
            step = c("h1", "h2"),
            number = c(6, 6),
            mean = c(
                mean(c(35,64,23,13,99,83)),
                mean(c(54,23,65,12,22,9))),
            stringsAsFactors = FALSE)),
       "2.RSH means survey2")

    # Intersection gives nothing
    ok(cmp(
        mfdb_meanlength(mdb, list(
            gear = 'GIL',
            vessel = '2.RSH',
            area = mfdb_group(divA = c("divA")),
            timestep = step_halves,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 100, by = 10)),
            year = 1998:2000))[["0.0.0"]][,c("step", "number", "mean")],
        NULL),
       "GIL & 2.RSH returns nothing")
})
