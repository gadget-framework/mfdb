# This script demonstrates creating fleet files
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

ok_group("Fleetfile example", {
    # Create a temporary gadget directory
    gd <- gadget_directory(tempfile())

    # Set up the vessels we use in this example
    mfdb_import_vessel_taxonomy(mdb, data.frame(
        name = c('1.RSH', '2.COM'),
        vessel_type = c('1.RSH', '2.COM'),
        stringsAsFactors = FALSE
    ))

    # Import a survey for the data we are interested in
    mfdb_import_survey(mdb, data_source = "cod2000",
        table_string("
year    month   areacell        species vessel  length  age     weight
2000    1       45G01           COD     1.RSH           21      2       210
2000    1       45G02           COD     2.COM           34      3       220
2000    1       45G03           COD     2.COM           34      3       230
2000    1       45G01           COD     2.COM           62      1       320
2000    1       45G02           COD     2.COM           53      1       330
2000    1       45G03           COD     2.COM           54      2       430
2000    1       45G01           COD     1.RSH           28      2       210
2000    1       45G02           COD     2.COM           34      3       220
2000    1       45G03           COD     1.RSH           24      3       230
2000    1       45G01           COD     1.RSH           12      1       320
2000    1       45G02           COD     2.COM           44      1       330
2000    1       45G03           COD     1.RSH           14      2       430
        "))

    # Write out a total fleet component to our gadget directory
    gadget_dir_write(gd, gadget_fleet_component(
        'totalfleet',
        name = 'igfs',
        data = mfdb_sample_count(mdb, c(), list(
            year = 2000,
            step = mfdb_timestep_yearly,
            area = mfdb_group(x = 'divA', y = 'divB'),
            null = NULL))[[1]]))
    ok(cmp_file(gd, file.path('Modelfiles', 'fleet.fleet'),
        ver_string,
        "; ",
        "[fleetcomponent]",
        "totalfleet\tigfs",
        "livesonareas\t1\t2",
        "multiplicative\t1",
        "suitability\t",
        "amount\tData/fleet.igfs.data",
        NULL), "Livesonareas derived from data")
    ok(cmp_file(gd, file.path('Data', 'fleet.igfs.data'),
        ver_string,
        "; -- data --",
        "; year\tstep\tarea\tfleetname\tnumber",
        "2000\t1\t1\tigfs\t8",
        "2000\t1\t2\tigfs\t4",
        NULL), "Areas in data have become numeric")
})

ok_group("Deprecated usages---don't do this", {
    # Create a temporary gadget directory
    gd <- gadget_directory(tempfile())

    # Can override fleetname if required, but no way of
    # creating corresponding multiple components
    gadget_dir_write(gd, gadget_fleet_component(
        'totalfleet',
        name = 'igfs',
        data = mfdb_sample_count(mdb, c('vessel'), list(
            year = 2000,
            step = mfdb_timestep_yearly,
            area = mfdb_group(x = 'divA', y = 'divB'),
            vessel = mfdb_unaggregated(),
            null = NULL))[[1]]))
    ok(cmp_file(gd, file.path('Data', 'fleet.igfs.data'),
        ver_string,
        "; -- data --",
        "; year\tstep\tarea\tvessel\tnumber",
        "2000\t1\t1\t1.RSH\t3",
        "2000\t1\t1\t2.COM\t5",
        "2000\t1\t2\t1.RSH\t2",
        "2000\t1\t2\t2.COM\t2",
        NULL), "Areas in data have become numeric")
})
