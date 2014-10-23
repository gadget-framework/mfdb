# Script to exercise mfdb
# NB: To use this you *must* be using mfdb-workspace and not mind loosing data
library(unittest)
library(mfdb)
source('mfdb/tests/utils/helpers.R')
source('mfdb/tests/utils/inttest-helpers.R')

# Make some obvious time groupings
step_year <- mfdb_group(year = 1:12)
step_halves <- mfdb_group(h1 = 1:6, h2 = 7:12)
step_quarters <- mfdb_group(q1 = 1:3, q2 = 4:6, q3 = 7:9, q4 = 10:12)

# Empty database
conn <- db_connection()
remove_mfdb_tables(conn)

# Rebuild database, taxonomy got populated
mdb <- mfdb('Iceland', db_params = db_params, save_temp_tables = TRUE)
ok(all(mfdb:::mfdb_fetch(mdb, "SELECT name, description FROM species WHERE species_id = 9999999999")[1,] == 
  mfdb::species[mfdb::species$name == 'TBX', c('name', 'description')]), "Entry for 9999999999 matches package")
ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM species")[1,1], nrow(mfdb::species)), "Species has right number of entries")

# Fiddle about with entry
mfdb:::mfdb_import_taxonomy(mdb, 'species', data.frame(id = c('9999999999'), name = c('XXX'), description = c('Wormy Worms')))
ok(all(mfdb:::mfdb_fetch(mdb, "SELECT name, description FROM species WHERE species_id = 9999999999")[1,] == 
  c('XXX', 'Wormy Worms')), "Entry for 9999999999 was updated")

# Connect as a different case study. shouldn't recreate tables, but should have fixed taxonomy
mdb2 <- mfdb('Baltic', db_params = db_params, save_temp_tables = TRUE)
ok(all(mfdb:::mfdb_fetch(mdb, "SELECT name, description FROM species WHERE species_id = 9999999999")[1,] == 
  mfdb::species[mfdb::species$name == 'TBX', c('name', 'description')]), "Entry for 9999999999 matches package")

section("Areacell/divisions", {
    # Can't populate divisions yet, no areacells defined
    ok(cmp_error(mfdb_import_division(mdb, list(divA = c('45G01', '45G02', '45G03'))), 'areacell vocabulary'), "Areacell not populated yet")

    # So define some, so it should work
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(5)))
    mfdb_import_division(mdb, list(divA = c('45G01', '45G02', '45G03')))
    ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM division")[1,1], 3), "Inserted 3 rows into division")

    # Can't populate with invalid areacells, divC was rolled back so still 3 rows
    ok(cmp_error(mfdb_import_division(mdb, list(divB = c('45G01', '45G02', '45G08'), divC = c('45G01'))), 'areacell vocabulary'), "Invalid areacell values")
    ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM division")[1,1], 3), "Inserted 3 rows into division")

    # Worked this time
    mfdb_import_division(mdb, list(divB = c('45G01', '45G02'), divC = c('45G01')))
    ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM division")[1,1], 6), "Inserted 6 rows into division")

    # Do similar to mdb2, to show case study data is isolated
    mfdb_import_area(mdb2, data.frame(id = c(1,2,3), name = c('45G03', '45G04', '45G05'), size = c(10, 11, 12)))
    ok(cmp_error(mfdb_import_division(mdb2, list(divB = c('45G01', '45G02', '45G03'), divC = c('45G01'))), 'areacell vocabulary'), "areacell values not for this case-study")
    mfdb_import_division(mdb2, list(divA = c('45G03', '45G04'), divD = c('45G04', '45G05')))

    # Can't make a report without an area grouping
    ok(cmp_error(mfdb_area_size(mdb, list()), "area"), "Noticed lack of area grouping")

    # Finally, we can make a report out of this
    area_group <- mfdb_group(divA = c("divA"), divB = c("divB"), divAB = c("divA", "divB"))
    ok(cmp(mfdb_area_size(mdb, list(area = area_group)),
        list("0" = structure(
            data.frame(area = c("divA", "divAB", "divB"), size = c(15, 25, 10), stringsAsFactors = FALSE),
            area = area_group,
            generator = "mfdb_area_size"))),
        "Can combine divA & B and get combined size")

    # And a different report for mdb2
    area_group <- mfdb_group(divA = c("divA"), divAll = c("divA", "divB", "divC", "divD"))
    ok(cmp(mfdb_area_size(mdb2, list(area = area_group)),
        list("0" = structure(
            #TODO: divA and divB overlap, so divAll contains 45G04 twice. Probably bad for size(?)
            data.frame(area = c("divA", "divAll"), size = c(21, 44), stringsAsFactors = FALSE),
            area = area_group,
            generator = "mfdb_area_size"))),
        "Can combine divA & B and get combined size")
})

section("Temperature import", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(5)))
    mfdb_import_division(mdb, list(divB = c('45G01', '45G02'), divC = c('45G01')))

    # Notice missing columns
    ok(cmp_error(mfdb_import_temperature(mdb, data.frame(
        year = c(1998),
        areacell_id = c('45G01', '45G01', '45G01'),
        temperature = c(1,2,4))), "month"), "Notice month column is missing")

    # Import works
    mfdb_import_temperature(mdb, data.frame(
        year = rep(c(1998, 1999), each = 12),
        month = c(1:12, 1:12),
        areacell = c(rep('45G01', times = 24)),
        temperature = c(1:12, 25:36)))
    area_group <- mfdb_group(divA = c("divA"))
    ok(cmp(mfdb_temperature(mdb, list(year = c(1998, 1999, 2000), timestep = step_quarters, area = area_group)),
        list("0.0" = structure(
            data.frame(
                year = rep(c(1998, 1999), each = 4),
                step = rep(c("q1", "q2", "q3", "q4"), times = 2),
                area = rep("divA", times = 8),
                temperature = c(
                    mean(1:3), mean(4:6), mean(7:9), mean(10:12),
                    mean(25:27), mean(28:30), mean(31:33), mean(34:36)),
                stringsAsFactors = FALSE),
            timestep = step_quarters,
            area = area_group,
            generator = "mfdb_temperature"))),
        "Can collate temperatures by quarter")

    mfdb_import_temperature(mdb, data.frame(
        year = c(1998),
        month = c(4,5,6),
        areacell = c('45G01', '45G01', '45G01'),
        temperature = c(1,2,4)))
})

section("Temperature", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(5)))
    mfdb_import_division(mdb, list(divB = c('45G01', '45G02'), divC = c('45G01')))

    # Notice missing columns
    ok(cmp_error(mfdb_import_temperature(mdb, data.frame(
        year = c(1998),
        areacell_id = c('45G01', '45G01', '45G01'),
        temperature = c(1,2,4))), "month"), "Notice month column is missing")

    # Import works
    mfdb_import_temperature(mdb, data.frame(
        year = rep(c(1998, 1999), each = 12),
        month = c(1:12, 1:12),
        areacell = c(rep('45G01', times = 24)),
        temperature = c(1:12, 25:36)))
    area_group <- mfdb_group(divA = c("divA"))
    ok(cmp(mfdb_temperature(mdb, list(year = c(1998, 1999, 2000), timestep = step_quarters, area = area_group)),
        list("0.0" = structure(
            data.frame(
                year = rep(c(1998, 1999), each = 4),
                step = rep(c("q1", "q2", "q3", "q4"), times = 2),
                area = rep("divA", times = 8),
                temperature = c(
                    mean(1:3), mean(4:6), mean(7:9), mean(10:12),
                    mean(25:27), mean(28:30), mean(31:33), mean(34:36)),
                stringsAsFactors = FALSE),
            timestep = step_quarters,
            area = area_group,
            generator = "mfdb_temperature"))),
        "Can collate temperatures by quarter")

    # Another import replaces previous data
    mfdb_import_temperature(mdb, data.frame(
        year = rep(c(1998, 1999), each = 12),
        month = c(1:12, 1:12),
        areacell = c(rep('45G01', times = 24)),
        temperature = c(12:23, 20:31)))
    ok(cmp(mfdb_temperature(mdb, list(year = c(1998, 1999, 2000), timestep = step_quarters, area = area_group)),
        list("0.0" = structure(
            data.frame(
                year = rep(c(1998, 1999), each = 4),
                step = rep(c("q1", "q2", "q3", "q4"), times = 2),
                area = rep("divA", times = 8),
                temperature = c(
                    mean(12:14), mean(15:17), mean(18:20), mean(21:23),
                    mean(20:22), mean(23:25), mean(26:28), mean(29:31)),
                stringsAsFactors = FALSE),
            timestep = step_quarters,
            area = area_group,
            generator = "mfdb_temperature"))),
        "Second import cleared previous data")
})

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

# Disconnect
mfdb_disconnect(mdb)
mfdb_disconnect(mdb2)
