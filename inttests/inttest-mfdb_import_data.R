library(unittest)
library(mfdb)
source('tests/utils/inttest-helpers.R')

# Convert a string into a data.frame
table_string <- function (text, ...) read.table(
    text = text,
    blank.lines.skip = TRUE,
    header = TRUE,
    stringsAsFactors = FALSE,
    ...)


# Empty database & rebuild
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb(gsub("inttest", "inttest-mfdb_import_data", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb(gsub("inttest", "inttest-mfdb_import_data", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, save_temp_tables = FALSE)

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(
    name = c('45G01', '45G02', '45G03'),
    division = c('divA', 'divA', 'divB'),
    size = c(10,200,400)))

# Set up the vessels we use in this example
mfdb_import_vessel_taxonomy(mdb, table_string('
name    full_name               vessel_type     length  power   tonnage
   A    "Arni Fridriksson"      1.RSH           70      50      900
   B    "Bjarni Saemundsson"    1.RSH           56      100     800
   C    "Cefas Endeavour"       1.RSH           65.50   150     700
   D    "Dallaporta"            1.RSH           24      900     600
   S    "Commercial vessel 1a"  2.COM           10      50      900
   T    "Commercial vessel 1b"  2.COM           30      50      900
   U    "Commercial vessel 2a"  2.COM           50      50      900
   V    "Commercial vessel 2b"  2.COM           70      50      900
'))


ok_group('mfdb_import_survey:invalid_taxonomy_data', {
    ok(ut_cmp_error(mfdb_import_survey(mdb, data_source = "x", table_string('
        year    month   areacell        species length  age     weight
        2000    1       45Z01           COD         21      2       210
    ')), "mfdb_import_area()"), "All areacells invalid: told to use mfdb_import_x()")

    ok(ut_cmp_error(mfdb_import_survey(mdb, data_source = "x", table_string('
        year    month   areacell        species vessel  length  age     weight
        2000    1       45G01           COD     ZZ          21      2       210
    ')), "mfdb_import_vessel_taxonomy()"), "All vessels invalid: told to use mfdb_import_x()")

    ok(ut_cmp_error(mfdb_import_survey(mdb, data_source = "x", table_string('
        year    month   areacell        species vessel  length  age     weight
        2000    1       45G01           COD     A          21      2       210
        2000    1       45G01           COD     ZZ          21      2       210
    ')), "mfdb_import_vessel_taxonomy()"), "Some vessels invalid: told to use mfdb_import_x()")

    ok(ut_cmp_error(mfdb_import_survey(mdb, data_source = "x", table_string('
        year    month   areacell        species sex
        2000    1       45G01           COD      ZZ
    ')), "mfdb::sex"), "All sex invalid: Told to check built-in taxonomy")

})
