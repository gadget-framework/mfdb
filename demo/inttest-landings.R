# This script demonstrates importing and querying landings
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

# Empty database & rebuild
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb(gsub("inttest", "inttest-landings", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb(gsub("inttest", "inttest-landings", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, save_temp_tables = FALSE)

# Set up some areas / divisions
mfdb_import_area(mdb, data.frame(
    name = c('45G01', '45G02', '45G03'),
    division = c('divA', 'divA', 'divB'),
    size = c(10,200,400)))

# We can either have "research" or "landings"
mfdb_import_sampling_type(mdb, data.frame(
    name = c("RES", "LND"),
    description = c("Research", "Landings"),
    stringsAsFactors = FALSE))

# Set up the vessels we use in this example
mfdb_import_vessel_taxonomy(mdb, data.frame(
    name = c('1.COM', '2.COM'),
    vessel_type = c('1.COM', '2.COM'),
    stringsAsFactors = FALSE
))

# Import some landings data
mfdb_import_survey(mdb,
    data_source = 'landings1',
    data.frame(
        year = c(2000),
        month = c(1:12),
        areacell = c('45G01'),
        species = c('COD'),
        sampling_type = 'LND',
        vessel = '1.COM',
        weight_total = c(1110,1510,1310,1110,1310,1410, 1610,1610,1310,1310,1310,1210),
        stringsAsFactors = TRUE))
mfdb_import_survey(mdb,
    data_source = 'landings2',
    data.frame(
        year = c(2000),
        month = c(1:12),
        areacell = c('45G01'),
        species = c('COD'),
        sampling_type = 'LND',
        vessel = '2.COM',
        weight_total = c(1120,1520,1320,1120,1320,1420, 1620,1620,1320,1320,1320,1220),
        stringsAsFactors = TRUE))
area_group <- mfdb_group('divA' = c('divA'))
vessel_group <- c('1.COM', '2.COM')

# Landings don't have a count
agg_data <- mfdb_sample_count(mdb, c("vessel"), params = list(
    year = 2000,
    step = mfdb_timestep_biannually,
    area = area_group,
    vessel = vessel_group))
agg_data[[1]]$number <- as.numeric(agg_data[[1]]$number)  # NB: SQLite returns logical, postgres numeric
ok(ut_cmp_equal(agg_data[[1]], structure(
    data.frame(
        year = as.integer(2000),
        step = c("1", "1", "2", "2"),
        area = c('divA'),
        vessel = c('1.COM', '2.COM', '1.COM', '2.COM'),
        number = as.numeric(c(NA, NA, NA, NA)),
        stringsAsFactors = FALSE),
    year = list("2000" = 2000),
    step = mfdb_timestep_biannually,
    area = area_group,
    vessel = list('1.COM' = '1.COM', '2.COM' = '2.COM'),
    generator = "mfdb_sample_count")), "No count for these records")

# Query database to combine these
agg_data <- mfdb_sample_totalweight(mdb, c("vessel"), params = list(
    year = 2000,
    step = mfdb_timestep_biannually,
    area = area_group,
    vessel = vessel_group))
ok(cmp(agg_data[[1]], structure(
    data.frame(
        year = as.integer(2000),
        step = c("1", "1", "2", "2"),
        area = c('divA'),
        vessel = c('1.COM', '2.COM', '1.COM', '2.COM'),
        total_weight = c(
            1110 + 1510 + 1310 + 1110 + 1310 + 1410,
            1120 + 1520 + 1320 + 1120 + 1320 + 1420,
            1610 + 1610 + 1310 + 1310 + 1310 + 1210,
            1620 + 1620 + 1320 + 1320 + 1320 + 1220,
            NULL),
        stringsAsFactors = FALSE),
    year = list("2000" = 2000),
    step = mfdb_timestep_biannually,
    area = area_group,
    vessel = list('1.COM' = '1.COM', '2.COM' = '2.COM'),
    generator = "mfdb_sample_totalweight")), "No count for these records")

# Import this into a catchinkilos component
gd <- gadget_directory(tempfile())
gadget_dir_write(gd, gadget_likelihood_component('catchinkilos',
    data = agg_data[[1]]))
ok(cmp_file(gd, "likelihood",
    ver_string,
    "; ",
    "[component]",
    "name\tcatchinkilos",
    "weight\t0",
    "type\tcatchinkilos",
    "datafile\tData/catchinkilos.catchinkilos.sumofsquares",
    "function\tsumofsquares",
    "aggregationlevel\t0",
    "epsilon\t10",
    "areaaggfile\tAggfiles/catchinkilos.catchinkilos.area.agg",
    "fleetnames\t",
    "stocknames\t",
    NULL), "Wrote likelihood file")
ok(cmp_file(gd, "Data/catchinkilos.catchinkilos.sumofsquares",
    ver_string,
    "; -- data --",
    "; year\tstep\tarea\tvessel\ttotal_weight",
    "2000\t1\tdivA\t1.COM\t7760",
    "2000\t1\tdivA\t2.COM\t7820",
    "2000\t2\tdivA\t1.COM\t8360",
    "2000\t2\tdivA\t2.COM\t8420",
    NULL), "Wrote component data file")

# Can also group by year, in which case aggregationlevel == 1
agg_data <- mfdb_sample_totalweight(mdb, c("vessel"), params = list(
    year = 2000,
    step = mfdb_timestep_yearly,
    area = area_group,
    vessel = vessel_group))
gadget_dir_write(gd, gadget_likelihood_component('catchinkilos',
    data = agg_data[[1]]))
ok(cmp_file(gd, "likelihood",
    ver_string,
    "; ",
    "[component]",
    "name\tcatchinkilos",
    "weight\t0",
    "type\tcatchinkilos",
    "datafile\tData/catchinkilos.catchinkilos.sumofsquares",
    "function\tsumofsquares",
    "aggregationlevel\t1",
    "epsilon\t10",
    "areaaggfile\tAggfiles/catchinkilos.catchinkilos.area.agg",
    "fleetnames\t",
    "stocknames\t",
    NULL), "Wrote likelihood file")
ok(cmp_file(gd, "Data/catchinkilos.catchinkilos.sumofsquares",
    ver_string,
    "; -- data --",
    "; year\tarea\tvessel\ttotal_weight",
    "2000\tdivA\t1.COM\t16120",
    "2000\tdivA\t2.COM\t16240",
    NULL), "Wrote yearly data, step was dropped")

ok_group("Can import counts too, but doesn't affect result", {
    mfdb_import_survey(mdb,
        data_source = 'landings1',
        data.frame(
            year = c(2000),
            month = c(1:12),
            areacell = c('45G01'),
            species = c('COD'),
            sampling_type = 'LND',
            vessel = '1.COM',
            weight_total = c(1110,1510,1310,1110,1310,1410, 1610,1610,1310,1310,1310,1210),
            count = c(1,2,3,1,2,3, 4,5,6,4,5,6),
            stringsAsFactors = TRUE))
    agg_data <- mfdb_sample_totalweight(mdb, c("vessel"), params = list(
        year = 2000,
        step = mfdb_timestep_biannually,
        area = area_group,
        vessel = vessel_group))
    ok(cmp(agg_data[[1]], structure(
        data.frame(
            year = as.integer(2000),
            step = c("1", "1", "2", "2"),
            area = c('divA'),
            vessel = c('1.COM', '2.COM', '1.COM', '2.COM'),
            total_weight = c(
                1110 + 1510 + 1310 + 1110 + 1310 + 1410,
                1120 + 1520 + 1320 + 1120 + 1320 + 1420,
                1610 + 1610 + 1310 + 1310 + 1310 + 1210,
                1620 + 1620 + 1320 + 1320 + 1320 + 1220,
                NULL),
            stringsAsFactors = FALSE),
        year = list("2000" = 2000),
        step = mfdb_timestep_biannually,
        area = area_group,
        vessel = list('1.COM' = '1.COM', '2.COM' = '2.COM'),
        generator = "mfdb_sample_totalweight")), "No count for these records")
})

ok_group("weight and weight_total isn't allowed", {
    ok(cmp_error(mfdb_import_survey(mdb,
        data_source = 'landings1',
        data.frame(
            year = c(2000),
            month = c(1:12),
            areacell = c('45G01'),
            species = c('COD'),
            sampling_type = 'LND',
            vessel = '1.COM',
            weight = c(111,151,131,111,131,141, 161,161,131,131,131,121),
            weight_total = c(1110,1510,1310,1110,1310,1410, 1610,1610,1310,1310,1310,1210),
            stringsAsFactors = TRUE)), "weight.*weight_total"), "Can't specify both weight and weight_total")
})

ok_group("Preserve weight precision", {
    data_in <- data.frame(year = 2010, month = 1, areacell = '45G01', weight_total = rnorm(1e5, 100, 10))
    mfdb_import_survey(mdb, data_in)
    out_mfdb <- mfdb_sample_totalweight(mdb = mdb, cols = NULL, params = list(year = 2010))
    ok(ut_cmp_equal(
        out_mfdb[[1]]$total_weight,
        sum(data_in$weight_total),
        tolerance = 1e-7), "Total weight through MFDB matches R")
})

mfdb_disconnect(mdb)
