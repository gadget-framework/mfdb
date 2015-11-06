# Excerise storing / grouping by tow metadata
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
mfdb('', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = TRUE) # TODO:

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(10,200,400)))
mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G03')))

ok_group("Tow metadata example", {
    # Create a temporary gadget directory
    gd <- gadget_directory(tempfile())

    # Set up the tows we use in this example
    mfdb_import_tow_taxonomy(mdb, data.frame(
        name = c('A', 'B', 'C', 'D'),
        latitude = c(64.10000, 63.40000, 64.90000, 66.20000),
        longitude = c(-23.15000, -20.18000, -13.55000, -18.55000),
        depth = c(98.82, 44.90, 140.91, 122.61),
        length = c(10, 10, 20, 20),
        stringsAsFactors = FALSE
    ))

    # Import a survey for the data we are interested in
    mfdb_import_survey(mdb, data_source = "cod2000",
        table_string("
year    month   areacell        species tow  length  age     weight
2000    1       45G01           COD     A    21      2       210
2000    1       45G02           COD     A    34      3       220
2000    1       45G03           COD     A    34      3       230
2000    1       45G01           COD     B    62      1       320
2000    1       45G02           COD     B    53      1       330
2000    1       45G03           COD     B    54      2       430
2000    1       45G01           COD     C    28      2       210
2000    1       45G02           COD     C    34      3       220
2000    1       45G03           COD     C    24      3       230
2000    1       45G01           COD     D    12      1       320
2000    1       45G02           COD     D    44      1       330
2000    1       45G03           COD     D    14      2       430
        "))

    # Group the data by tow depth
    agg_data <- mfdb_sample_meanlength(mdb, c('tow_depth'), list(
        step = mfdb_timestep_yearly,
        tow_depth = mfdb_interval('depth', c(0, 50, 100, 150)),
        null = NULL))
    ok(cmp(unattr(agg_data[[1]]), data.frame(
        year = c('all'),
        step = c('1'),
        area = c('all'),
        tow_depth = c('depth0', 'depth100', 'depth50'),
        number = c(3, 6, 3),
        mean = c(
            mean(c(62, 53, 54)),
            mean(c(28, 34, 24, 12, 44, 14)),
            mean(c(21, 34, 34)),
        NULL),
        stringsAsFactors = FALSE)), "Grouped by tow_depth")

    # Show all tow data unaggregated
    agg_data <- mfdb_sample_meanlength(mdb, c('tow', 'tow_latitude', 'tow_longitude', 'tow_depth', 'tow_length'), list(
        step = mfdb_timestep_yearly,
        area = mfdb_group(divB = 'divB'),
        tow = mfdb_unaggregated(),
        tow_latitude = mfdb_unaggregated(),
        tow_longitude = mfdb_unaggregated(),
        tow_depth = mfdb_unaggregated(),
        tow_length = mfdb_unaggregated(),
        null = NULL))
    ok(cmp(unattr(agg_data[[1]]), data.frame(
        year = c('all'),
        step = c('1'),
        area = c('divB'),
        tow = c('A', 'B', 'C', 'D'),
        tow_latitude = c(64.10000, 63.40000, 64.90000, 66.20000),
        tow_longitude = c(-23.15000, -20.18000, -13.55000, -18.55000),
        tow_depth = c(98.82, 44.90, 140.91, 122.61),
        tow_length = c(10, 10, 20, 20),
        number = c(1, 1, 1, 1),
        mean = c(34, 54, 24, 14),
        stringsAsFactors = FALSE)), "Shows all detail of tows")

    # Tow length can also be used to scale samples
    agg_data <- mfdb_sample_meanlength(mdb, c('tow'), list(
        step = mfdb_timestep_yearly,
        tow = mfdb_unaggregated(),
        null = NULL), scale_index = 'tow_length')
    ok(cmp(unattr(agg_data[[1]]), data.frame(
        year = c('all'),
        step = c('1'),
        area = c('all'),
        tow = c('A', 'B', 'C', 'D'),
        number = c(3 / 10, 3 / 10, 3 / 20, 3 / 20),
        mean = c(
            (21 * (1/10) + 34 * (1/10) + 34 * (1/10)) / (3/10),
            (62 * (1/10) + 53 * (1/10) + 54 * (1/10)) / (3/10),
            (28 * (1/20) + 34 * (1/20) + 24 * (1/20)) / (3/20),
            (12 * (1/20) + 44 * (1/20) + 14 * (1/20)) / (3/20),
        NULL),
        stringsAsFactors = FALSE)), "Grouped by tow_depth")
})
