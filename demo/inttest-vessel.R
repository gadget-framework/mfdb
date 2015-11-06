# Excerise storing / grouping by vessel information
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
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = FALSE)

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(10,200,400)))
mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G03')))

ok_group("Vessel metadata example", {
    # Create a temporary gadget directory
    gd <- gadget_directory(tempfile())

    # Set up the vessels we use in this example
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
year    month   areacell        species vessel  length  age     weight
2000    1       45G01           COD     A           21      2       210
2000    1       45G02           COD     A           34      3       220
2000    1       45G03           COD     A           34      3       230
2000    1       45G01           COD     B           62      1       320
2000    1       45G02           COD     B           53      1       330
2000    1       45G03           COD     B           54      2       430
2000    1       45G01           COD     C           28      2       210
2000    1       45G02           COD     C           34      3       220
2000    1       45G03           COD     C           24      3       230
2000    1       45G01           COD     D           12      1       320
2000    1       45G02           COD     D           44      1       330
2000    1       45G03           COD     D           14      2       430
        "))

    # Group the data by vessel length
    agg_data <- mfdb_sample_meanlength(mdb, c('vessel_length'), list(
        step = mfdb_timestep_yearly,
        vessel_length = mfdb_interval('veslen', c(0, 10, 20, 50)),
        null = NULL))
    ok(cmp(unattr(agg_data[[1]]), data.frame(
        year = c('all'),
        step = c('1'),
        area = c('all'),
        vessel_length = c('veslen10', 'veslen20'),
        number = c(6, 6),
        mean = c(
            mean(c(21, 34, 34, 62, 53, 54)),
            mean(c(28, 34, 24, 12, 44, 14)),
        NULL),
        stringsAsFactors = FALSE)), "Grouped by vessel_length")

    # Group the data by vessel type and area
    agg_data <- mfdb_sample_meanlength(mdb, c('vessel_type'), list(
        step = mfdb_timestep_yearly,
        area = mfdb_group(x = 'divA', y = 'divB'),
        vessel_type = mfdb_group(RSH = '1.RSH', COM = '1.COM'),
        null = NULL))
    ok(cmp(unattr(agg_data[[1]]), data.frame(
        year = c('all'),
        step = c('1'),
        area = c('x', 'x', 'y', 'y'),
        vessel_type = c('COM', 'RSH', 'COM', 'RSH'),
        number = c(4, 2, 2, 1),
        mean = c(
            mean(c(62, 53, 28, 34)),
            mean(c(21, 34)),
            mean(c(54, 24)),
            mean(c(34)),
        NULL),
        stringsAsFactors = FALSE)), "Grouped by vessel_name, area")

    # Show each vessel separately, with full name, power, tonnage
    agg_data <- mfdb_sample_meanlength(mdb, c('vessel', 'vessel_full_name', 'vessel_power', 'vessel_tonnage'), list(
        step = mfdb_timestep_yearly,
        vessel = mfdb_unaggregated(),
        vessel_full_name = mfdb_unaggregated(),
        vessel_power = mfdb_unaggregated(),
        vessel_tonnage = mfdb_unaggregated(),
        null = NULL))
    ok(cmp(unattr(agg_data[[1]]), data.frame(
        year = c('all'),
        step = c('1'),
        area = c('all'),
        vessel = c('A', 'B', 'C', 'D'),
        vessel_full_name = c('Alfred', 'Bertie', 'Claire', 'Daisy'),
        vessel_power = c(50, 100, 150, 900),
        vessel_tonnage = c(900, 800, 700, 600),
        number = c(3, 3, 3, 3),
        mean = c(
            mean(c(21, 34, 34)),
            mean(c(62, 53, 54)),
            mean(c(28, 34, 24)),
            mean(c(12, 44, 14)),
        NULL),
        stringsAsFactors = FALSE)), "Grouped by vessel_length")
})
