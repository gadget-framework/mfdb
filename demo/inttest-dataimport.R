# This script demonstrates details of the mfdb_import_* functions, what can
# go wrong as well as how to use it.
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

# Empty database & connect
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('inttest-dataimport', db_params = db_params, destroy_schema = TRUE)
mfdb('inttest-dataimport-Baltic', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('inttest-dataimport', db_params = db_params, save_temp_tables = FALSE)
mdb2 <- mfdb('inttest-dataimport-Baltic', db_params = db_params, save_temp_tables = FALSE)

ok_group("Areacell/divisions", {
    # Can't populate divisions yet, no areacells defined
    ok(cmp_error(mfdb_import_division(mdb, list(divA = c('45G01', '45G02', '45G03'))), 'areacell vocabulary'), "Areacell not populated yet")

    # So define some, so it should work
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(5.1, 5.2, 5.3)))
    mfdb_import_division(mdb, list(divA = c('45G01', '45G02', '45G03')))
    ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM division")[1,1], 3), "Inserted 3 rows into division")

    # Can't populate with invalid areacells, divC was rolled back so still 3 rows
    ok(cmp_error(mfdb_import_division(mdb, list(divB = c('45G01', '45G02', '45G08'), divC = c('45G01'))), 'areacell vocabulary.*45G08'), "Invalid areacell values")
    ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM division")[1,1], 3), "Inserted 3 rows into division")

    # Worked this time
    mfdb_import_division(mdb, list(divB = c('45G01', '45G02'), divC = c('45G01')))
    ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM division")[1,1], 6), "Inserted 6 rows into division")

    # Do similar to mdb2, to show case study data is isolated
    mfdb_import_area(mdb2, data.frame(id = c(1,2,3), name = c('45G03', '45G04', '45G05'), size = c(10, 11, 12)))
    ok(cmp_error(mfdb_import_division(mdb2, list(divB = c('45G01', '45G02', '45G03'), divC = c('45G01'))), 'areacell vocabulary'), "areacell values not for this case-study")
    mfdb_import_division(mdb2, list(divA = c('45G03', '45G04'), divD = c('45G04', '45G05')))

    # Finally, we can make a report out of this
    area_group <- mfdb_group(divA = c("divA"), divB = c("divB"), divAB = c("divA", "divB"))
    ok(cmp(mfdb_area_size(mdb, list(area = area_group)),
        list("0" = structure(
            data.frame(area = c("divA", "divAB", "divB"), size = c(15.6, 25.9, 10.3), stringsAsFactors = FALSE),
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

    # Can change the area size and update the report
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(10)))
    area_group <- mfdb_group(divA = c("divA"), divB = c("divB"), divAB = c("divA", "divB"))
    ok(cmp(mfdb_area_size(mdb, list(area = area_group)),
        list("0" = structure(
            data.frame(area = c("divA", "divAB", "divB"), size = c(30, 50, 20), stringsAsFactors = FALSE),
            area = area_group,
            generator = "mfdb_area_size"))),
        "Get new combined size after updating area sizes")

    # Can import areas and divisions at the same time
    mfdb_import_area(mdb, table_string("
id	name	size	division
10	a10	1001	div10
11	a11	1003	div10
12	a12	1005	div10
13	a20	2002	div20
14	a21	2004	div20
15	a22	2008	div20
    "))
    ok(cmp(mfdb_area_size(mdb, list(area = mfdb_group(g1 = c('div10'), g2 = c('div20'), g3 = c('div10', 'div20'))))[[1]][,c('area', 'size')],
        table_string("
area	size
g1	3009
g2	6014
g3	9023
        ")), "Inserted divisions at the same time as areas")
})

ok_group("Temperature", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(5)))
    mfdb_import_division(mdb, list(divB = c('45G01', '45G02'), divC = c('45G01')))

    # Notice missing columns
    ok(cmp_error(mfdb_import_temperature(mdb, data.frame(
        year = c(1998),
        areacell = c('45G01', '45G01', '45G01'),
        temperature = c(1,2,4))), "month"), "Notice month column is missing")

    # Import works
    mfdb_import_temperature(mdb, data.frame(
        year = rep(c(1998, 1999), each = 12),
        month = c(1:12, 1:12),
        areacell = c(rep('45G01', times = 24)),
        temperature = c(1:12, 25:36)))
    area_group <- mfdb_group(divA = c("divA"))
    ok(cmp(mfdb_temperature(mdb, list(year = c(1998, 1999, 2000), timestep = mfdb_timestep_quarterly, area = area_group)),
        list("0.0.0" = structure(
            data.frame(
                year = rep(c(1998L, 1999L), each = 4),
                step = rep(c("1", "2", "3", "4"), times = 2),
                area = rep("divA", times = 8),
                mean = c(
                    mean(1:3), mean(4:6), mean(7:9), mean(10:12),
                    mean(25:27), mean(28:30), mean(31:33), mean(34:36)),
                stringsAsFactors = FALSE),
            year = as.list(structure(as.numeric(1998:2000), names = 1998:2000)),
            step = mfdb_timestep_quarterly,
            area = area_group,
            generator = "mfdb_survey_index_mean"))),
        "Can collate temperatures by quarter")

    mfdb_import_temperature(mdb, data.frame(
        year = c(1998),
        month = c(4,5,6),
        areacell = c('45G01', '45G01', '45G01'),
        temperature = c(1,2,4)))

    # Another import replaces previous data
    mfdb_import_temperature(mdb, data.frame(
        year = rep(c(1998, 1999), each = 12),
        month = c(1:12, 1:12),
        areacell = c(rep('45G01', times = 24)),
        temperature = c(12:23, 20:31)))
    ok(cmp(mfdb_temperature(mdb, list(year = c(1998, 1999, 2000), timestep = mfdb_timestep_quarterly, area = area_group)),
        list("0.0.0" = structure(
            data.frame(
                year = rep(c(1998L, 1999L), each = 4),
                step = rep(c("1", "2", "3", "4"), times = 2),
                area = rep("divA", times = 8),
                mean = c(
                    mean(12:14), mean(15:17), mean(18:20), mean(21:23),
                    mean(20:22), mean(23:25), mean(26:28), mean(29:31)),
                stringsAsFactors = FALSE),
            year = as.list(structure(as.numeric(1998:2000), names = 1998:2000)),
            step = mfdb_timestep_quarterly,
            area = area_group,
            generator = "mfdb_survey_index_mean"))),
        "Second import cleared previous data")
})

ok_group("Species", {
    mfdb_disconnect(mdb)
    mfdb('inttest-dataimport', db_params = db_params, destroy_schema = TRUE)
    mdb <- mfdb('inttest-dataimport', db_params = db_params, save_temp_tables = FALSE)
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(5.1, 5.2, 5.3)))

    # Can't empty taxonomy if we already have data
    mfdb_import_survey(mdb,
        data_source = 'survey1',
        data.frame(
            Year = c('1998'),  # NB: Not case-sensitive
            mOnth = c(1:12),
            areacell = c('45G01'),
            species = c('COD'),
            age =    c(  1),
            length = c( 10),
            weight = c(100)))
    ok(cmp_error(mfdb_empty_taxonomy(mdb, 'species'), 'delete on table "species"'), "Can't empty taxonony when it's used")

    # Can after emptying
    mfdb_import_survey(mdb,
        data_source = 'survey1',
        data.frame())
    mfdb_empty_taxonomy(mdb, 'species')

    # Now can't import data
    ok(cmp_error(mfdb_import_survey(mdb,
        data_source = 'survey1',
        data.frame(
            Year = c('1998'),  # NB: Not case-sensitive
            mOnth = c(1:12),
            areacell = c('45G01'),
            species = c('COD'),
            age =    c(  1),
            length = c( 10),
            weight = c(100))
    ), "species"), "Can't insert with an empty species taxonomy")
})

# Disconnect
mfdb_disconnect(mdb)
mfdb_disconnect(mdb2)
