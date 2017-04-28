# Excerise various aggregate functions
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

# Empty database & rebuild
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('inttest-aggregate', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('inttest-aggregate', db_params = db_params, save_temp_tables = TRUE) # TODO:

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(
    name = c('45G01', '45G02', '45G03'),
    division = c('divA', 'divA', 'divB'),
    size = c(10,200,400)))

ok_group("mfdb_unaggregated", {
    # Set up some dummy tows/vessels
    mfdb_import_tow_taxonomy(mdb, data.frame(
        name = c('C1', 'C2', 'R1', 'R2'),
        latitude = c(64.10000, 63.40000, 64.90000, 66.20000),
        longitude = c(-23.15000, -20.18000, -13.55000, -18.55000),
        depth = c(98.82, 44.90, 140.91, 122.61),
        length = c(10, 10, 20, 20),
        stringsAsFactors = FALSE
    ))
    mfdb_import_vessel_taxonomy(mdb, data.frame(
        name = c('A', 'B', 'C', 'D'),
        full_name = c('Alfred', 'Bertie', 'Claire', 'Daisy'),
        vessel_type = c('1.RSH', '1.COM', '1.COM', '1.FRZ'),
        length = c(15, 18, 20, 24),
        power = c(50, 100, 150, 900),
        tonnage = c(900, 800, 700, 600),
        stringsAsFactors = FALSE
    ))

    # Import a survey for the data we are interested in
    mfdb_import_survey(mdb, data_source = "cod2000",
        table_string("
year    month   areacell        species tow  vessel length  age     weight
1998    1       45G01           COD     C1   A      21      2       210
1999    1       45G02           COD     C1   B      34      3       220
2000    1       45G03           COD     C1   C      34      3       230
1998    1       45G01           COD     C2   A      62      1       320
1999    1       45G02           COD     C2   B      53      1       330
2000    1       45G03           COD     C2   C      54      2       430
1998    1       45G01           COD     R1   A      28      2       210
1999    1       45G02           COD     R1   B      34      3       220
2000    1       45G03           COD     R1   C      24      3       230
1998    1       45G01           COD     R2   A      12      1       320
1999    1       45G02           COD     R2   B      44      1       330
2000    1       45G03           COD     R2   C      14      2       430
        "))

    # Use like to filter tow
    agg_data <- mfdb_sample_totalweight(mdb, c(), list(
        step = mfdb_timestep_yearly,
        tow = mfdb_unaggregated(like = c("%2", "C1")),
        null = NULL))
    ok(cmp(unattr(agg_data[[1]]), data.frame(
        year = c("all"),
        step = c('1'),
        area = c('all'),
        total_weight = c(
            sum(320, 330, 430, 320, 330, 430, 210, 220, 230),
        NULL),
        stringsAsFactors = FALSE)), "Selected %2,C1 entries")

    # If we add tow column, results aren't aggregated
    agg_data <- mfdb_sample_totalweight(mdb, c("tow"), list(
        step = mfdb_timestep_yearly,
        tow = mfdb_unaggregated(like = c("%2", "C1")),
        null = NULL))
    ok(cmp(unattr(agg_data[[1]]), data.frame(
        year = c("all"),
        step = c('1'),
        area = c('all'),
        tow = c("C1", "C2", "R2"),
        total_weight = c(
            sum(210, 220, 230),
            sum(320, 330, 430),
            sum(320, 330, 430)
        ),
        stringsAsFactors = FALSE)), "Selected and grouped by %2,C1 entries")

    # not_like generates equivalent results
    agg_data <- mfdb_sample_totalweight(mdb, c("tow"), list(
        step = mfdb_timestep_yearly,
        tow = mfdb_unaggregated(not_like = c("R1")),
        null = NULL))
    ok(cmp(unattr(agg_data[[1]]), data.frame(
        year = c("all"),
        step = c('1'),
        area = c('all'),
        tow = c("C1", "C2", "R2"),
        total_weight = c(
            sum(210, 220, 230),
            sum(320, 330, 430),
            sum(320, 330, 430)
        ),
        stringsAsFactors = FALSE)), "Selected !R1 entries")

    # Can also use like on a non-taxonomy column
    agg_data <- mfdb_sample_totalweight(mdb, c('tow'), list(
        step = mfdb_timestep_yearly,
        tow = mfdb_unaggregated(),
        vessel_full_name = mfdb_unaggregated(like = c("%e")),
        null = NULL))
    ok(cmp(unattr(agg_data[[1]]), data.frame(
        year = c("all"),
        step = c('1'),
        area = c('all'),
        tow = c("C1", "C2", "R1", "R2"),
        total_weight = c(220 + 230, 330 + 430, 220 + 230, 330 + 430),
        stringsAsFactors = FALSE)), "Selected all vessels ending with e")
})

ok_group("mfdb_na_group", {
    # Import a survey for the data we are interested in
    mfdb_import_survey(mdb, data_source = "cod2000",
        table_string("
year    month   areacell        species length  age     weight
1998    1       45G01           COD     21      2       210
1999    1       45G02           COD     34      3       NA
2000    1       45G03           COD     NA      3       230
1998    1       45G01           COD     62      1       320
1999    1       45G02           COD     NA      1       330
2000    1       45G03           COD     54      2       430
1998    1       45G01           HAD     28      2       NA
1999    1       45G02           HAD     NA      3       220
2000    1       45G03           HAD     NA      3       NA
1998    1       45G01           HAD     12      1       NA
1999    1       45G02           HAD     NA      1       330
2000    1       45G03           HAD     14      2       430
        "))

    # Group by length without na_group, NA's shouldn't be visible
    agg_data <- mfdb_sample_count(mdb, c('species', 'length'), list(
        species = mfdb_unaggregated(),
        length = mfdb_step_interval("len", 10),
        null = NULL))
    ok(cmp(unattr(agg_data[[1]]), data.frame(
        year = c("all"),
        step = c('all'),
        area = c('all'),
        species = c('COD', 'COD', 'COD', 'COD', 'HAD', 'HAD'),
        length = c('len20', 'len30', 'len50', 'len60', 'len10', 'len20'),
        number = c(1,1,1,1,2,1),
        stringsAsFactors = FALSE)), "NAs aren't visible by default")

    # Add na_group for others
    agg_data <- mfdb_sample_count(mdb, c('species', 'length'), list(
        species = mfdb_unaggregated(),
        length = mfdb_na_group(mfdb_step_interval("len", 10), 'len_unknown'),
        null = NULL))
    ok(cmp(unattr(agg_data[[1]]), data.frame(
        year = c("all"),
        step = c('all'),
        area = c('all'),
        species = c('COD', 'COD', 'COD', 'COD', 'COD', 'HAD', 'HAD', 'HAD'),
        length = c('len20', 'len30', 'len50', 'len60', 'len_unknown', 'len10', 'len20', 'len_unknown'),
        number = c(1,1,1,1,2,2,1,3),
        stringsAsFactors = FALSE)), "NAs aren't visible by default")
})
