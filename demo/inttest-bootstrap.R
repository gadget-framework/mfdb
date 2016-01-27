# This script demonstrates bootstrap sampling an area
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
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = FALSE)

ok_group("Bootstrap samples of area sizes and lengths", {
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
            length = c(110,150,130,110,135,146, 165,162,136,135,134,122)))
    mfdb_import_survey(mdb,
        data_source = 'cell2',
        data.frame(
            year = c('1998'),
            month = c(1:12),
            areacell = c('45G02'),
            species = c('COD'),
            length = c(210,250,230,210,235,246, 265,262,236,235,234,222)))
    mfdb_import_survey(mdb,
        data_source = 'cell3',
        data.frame(
            year = c('1998'),
            month = c(1:12),
            areacell = c('45G03'),
            species = c('COD'),
            length = c(310,350,330,310,335,346, 365,362,336,335,334,322)))

    # We want 1 area, that's a combination of A and B
    area_group <- mfdb_group(all = c("divA", "divB"))
    # We want to take 5 samples "all" with replacement
    # We fix the seed to 35, so we get the same samples every time this script is run
    area_bootstrap_group <- mfdb_bootstrap_group(5, area_group, seed = 35)
    agg <- mfdb_area_size(mdb, params = list(area = area_bootstrap_group))

    # Instead of the normal 1, we get back 5 data frames
    ok(cmp(length(agg), 5), "Got 5 distinct groupings")

    # In the first case, all = divB & divA
    ok(cmp(agg[[1]], structure(
        data.frame(
            area = 'all',
            size = 400 + (10 + 200),  # divB + divA
            stringsAsFactors = FALSE
        ),
        area = list(all = c("divB", "divA")),
        generator = "mfdb_area_size"
    )), "divB, divA")

    # In the second, all = divB & divB
    ok(cmp(agg[[2]], structure(
        data.frame(
            area = 'all',
            size = 400 + 400,  # divB + divB
            stringsAsFactors = FALSE
        ),
        area = list(all = c("divB", "divB")),
        generator = "mfdb_area_size"
    )), "divB, divB")

    ok(cmp(agg[[3]], structure(
        data.frame(
            area = 'all',
            size = (10 + 200) + (10 + 200),
            stringsAsFactors = FALSE
        ),
        area = list(all = c("divA", "divA")),
        generator = "mfdb_area_size"
    )), "divA, divA")

    ok(cmp(agg[[4]], structure(
        data.frame(
            area = 'all',
            size = (10 + 200) + 400,
            stringsAsFactors = FALSE
        ),
        area = list(all = c("divA", "divB")),
        generator = "mfdb_area_size"
    )), "divA, divB")

    ok(cmp(agg[[5]], structure(
        data.frame(
            area = 'all',
            size = 400 + 400,
            stringsAsFactors = FALSE
        ),
        area = list(all = c("divB", "divB")),
        generator = "mfdb_area_size"
    )), "divB, divB")

    # Can do the same with counts
    length_group <- mfdb_step_interval('len', 50)
    agg <- mfdb_sample_count(mdb, c('length'), params = list(
        area = area_bootstrap_group,
        length = length_group,
        null = NULL))
    ok(cmp(length(agg), 5), "Got 5 distinct groupings")
    ok(cmp(agg[["0.0.1.0"]], structure(
        data.frame(
            year = 'all',
            step = 'all',
            area = 'all',
            length = c('len100', 'len150', 'len200', 'len250', 'len300', 'len350'),
            number = c(9, 3, 9, 3, 9, 3),
            stringsAsFactors = FALSE),
        year = list(all = as.integer(c(1998, 1998))),
        step = list(all = as.integer(c(1, 12))),
        area = list(all = c("divB", "divA")),
        length = list(
            len0   = structure(call("seq",   0,  49), min =   0, max =  50),
            len50  = structure(call("seq",  50,  99), min =  50, max = 100),
            len100 = structure(call("seq", 100, 149), min = 100, max = 150),
            len150 = structure(call("seq", 150, 199), min = 150, max = 200),
            len200 = structure(call("seq", 200, 249), min = 200, max = 250),
            len250 = structure(call("seq", 250, 299), min = 250, max = 300),
            len300 = structure(call("seq", 300, 349), min = 300, max = 350),
            len350 = structure(call("seq", 350, 399), min = 350, max = 400)),
        generator = "mfdb_sample_count")), "divB, divA")
    ok(cmp(agg[["0.0.2.0"]], structure(
        data.frame(
            year = 'all',
            step = 'all',
            area = 'all',
            length = c('len300', 'len350'),
            number = c(18, 6),
            stringsAsFactors = FALSE),
        year = list(all = as.integer(c(1998, 1998))),
        step = list(all = as.integer(c(1, 12))),
        area = list(all = c("divB", "divB")),
        length = list(
            len0   = structure(call("seq",   0,  49), min =   0, max =  50),
            len50  = structure(call("seq",  50,  99), min =  50, max = 100),
            len100 = structure(call("seq", 100, 149), min = 100, max = 150),
            len150 = structure(call("seq", 150, 199), min = 150, max = 200),
            len200 = structure(call("seq", 200, 249), min = 200, max = 250),
            len250 = structure(call("seq", 250, 299), min = 250, max = 300),
            len300 = structure(call("seq", 300, 349), min = 300, max = 350),
            len350 = structure(call("seq", 350, 399), min = 350, max = 400)),
        generator = "mfdb_sample_count")), "divB, divB")
    ok(cmp(agg[["0.0.3.0"]], structure(
        data.frame(
            year = 'all',
            step = 'all',
            area = 'all',
            length = c('len100', 'len150', 'len200', 'len250'),
            number = c(18, 6, 18, 6),
            stringsAsFactors = FALSE),
        year = list(all = as.integer(c(1998, 1998))),
        step = list(all = as.integer(c(1, 12))),
        area = list(all = c("divA", "divA")),
        length = list(
            len0   = structure(call("seq",   0,  49), min =   0, max =  50),
            len50  = structure(call("seq",  50,  99), min =  50, max = 100),
            len100 = structure(call("seq", 100, 149), min = 100, max = 150),
            len150 = structure(call("seq", 150, 199), min = 150, max = 200),
            len200 = structure(call("seq", 200, 249), min = 200, max = 250),
            len250 = structure(call("seq", 250, 299), min = 250, max = 300)),
        generator = "mfdb_sample_count")), "divA, divA")
    ok(cmp(agg[["0.0.4.0"]], structure(
        data.frame(
            year = 'all',
            step = 'all',
            area = 'all',
            length = c('len100', 'len150', 'len200', 'len250', 'len300', 'len350'),
            number = c(9, 3, 9, 3, 9, 3),
            stringsAsFactors = FALSE),
        year = list(all = as.integer(c(1998, 1998))),
        step = list(all = as.integer(c(1, 12))),
        area = list(all = c("divA", "divB")),
        length = list(
            len0   = structure(call("seq",   0,  49), min =   0, max =  50),
            len50  = structure(call("seq",  50,  99), min =  50, max = 100),
            len100 = structure(call("seq", 100, 149), min = 100, max = 150),
            len150 = structure(call("seq", 150, 199), min = 150, max = 200),
            len200 = structure(call("seq", 200, 249), min = 200, max = 250),
            len250 = structure(call("seq", 250, 299), min = 250, max = 300),
            len300 = structure(call("seq", 300, 349), min = 300, max = 350),
            len350 = structure(call("seq", 350, 399), min = 350, max = 400)),
        generator = "mfdb_sample_count")), "divA, divB")
    ok(cmp(agg[["0.0.5.0"]], structure(
        data.frame(
            year = 'all',
            step = 'all',
            area = 'all',
            length = c('len300', 'len350'),
            number = c(18, 6),
            stringsAsFactors = FALSE),
        year = list(all = as.integer(c(1998, 1998))),
        step = list(all = as.integer(c(1, 12))),
        area = list(all = c("divB", "divB")),
        length = list(
            len0   = structure(call("seq",   0,  49), min =   0, max =  50),
            len50  = structure(call("seq",  50,  99), min =  50, max = 100),
            len100 = structure(call("seq", 100, 149), min = 100, max = 150),
            len150 = structure(call("seq", 150, 199), min = 150, max = 200),
            len200 = structure(call("seq", 200, 249), min = 200, max = 250),
            len250 = structure(call("seq", 250, 299), min = 250, max = 300),
            len300 = structure(call("seq", 300, 349), min = 300, max = 350),
            len350 = structure(call("seq", 350, 399), min = 350, max = 400)),
        generator = "mfdb_sample_count")), "divB, divB")
})

ok_group("Bootstrapped empty results", {
    # Take 5 samples of an empty region
    area_group <- mfdb_group(all = c("aardvark"))
    area_bootstrap_group <- mfdb_bootstrap_group(5, area_group, seed = 35)
    agg <- mfdb_area_size(mdb, params = list(area = area_bootstrap_group))
    ok(cmp(names(agg), c(
        "0", "1", "2", "3", "4",
        NULL)), "Still got 5 data.frames")

    area_group <- mfdb_group(all = c("aardvark"))
    area_bootstrap_group <- mfdb_bootstrap_group(5, area_group, seed = 35)
    agg <- mfdb_sample_meanlength(mdb, c(), params = list(area = area_bootstrap_group))
    ok(cmp(names(agg), c(
        "0.0.0", "0.0.1", "0.0.2", "0.0.3", "0.0.4",
        NULL)), "Still got 5 data.frames")
})
