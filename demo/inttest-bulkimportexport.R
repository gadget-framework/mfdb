# This script demonstrates using the bulk import/export functions
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

# Set up a bunch of tables
mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(10,200,400)))
mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G03')))
mfdb_import_cs_taxonomy(mdb, 'index_type', data.frame(name = c(
    "acoustic",
    "guesswork",
    NULL)))
mfdb_import_survey_index(mdb, data_source = 'acoustic_index1', data.frame(
    index_type = 'acoustic',
    year = '1998',
    month = 1:12,
    areacell = '45G01',
    #              -----Q1----- -----Q2----- -----Q3----- -----Q4-----
    value =      c(12, 11, 10,  9, 8, 7,     6, 5, 4,     3, 2, 1     )))
mfdb_import_survey(mdb,
    data_source = 'cell3',
    data.frame(
        year = rep(1900:1999, each = 12),  # NB: Buckets of data so we exercise batching
        month = rep(c(1:12), 100),
        areacell = c('45G03'),
        species = c('COD'),
        #              -----Q1----- -----Q2----- -----Q3----- -----Q4-----
        length = rep(c(213,253,333, 313,133,343, 163,363,233, 333,133,323 ), 100),
        count =  rep(c(  2,  1,  2,   1,  2,  1,   1,  2,  1,   2,  1,  2 ), 100)))
mfdb_import_stomach(mdb,
    data_source = "cod2000",
    predator_data = table_string("
stomach_name    year    month   areacell        species length  weight
A               2000    1       45G01           COD     21      210
B               2000    1       45G01           COD     34      220
C               2000    1       45G01           COD     34      230

D               2000    1       45G01           COD     62      320
E               2000    1       45G01           COD     33      330

G               2000    1       45G01           COD     34      430
    "),
    prey_data = table_string("
stomach_name    species digestion_stage length  weight  count
A               CAP             1                       1       10      5
A               CAP             1                       4       40      1
B               CAP             1                       1       10      5
B               CAP             4                       1       10      5
B               CAP             5                       1       10      8
B               CAP             5                       1       10      5
C               CLL             2                       3.5     9.5     3

D               CAP             1                       1.4     10      1
D               CLL             5                       4       40      1
E               CAP             1                       1.4     10      1
    "))

ok(cmp(
    mfdb_sample_count(mdb, c(), params = list())[[1]][,'number'],
    sum( 2,  1,  2,   1,  2,  1,   1,  2,  1,   2,  1,  2 ) * 100), "Sample rows all intact")

ok(cmp(
    mfdb_stomach_presenceratio(mdb, c(), params = list(prey_species = 'CAP'))[[1]][,'ratio'],
    4 / 6), "Stomach rows all intact")
ok(cmp(
    mfdb_stomach_presenceratio(mdb, c(), params = list(prey_species = 'CLL'))[[1]][,'ratio'],
    2 / 6), "Stomach rows all intact")

# Export, destroy, reimport
dump_dir <- tempfile()
mfdb_cs_dump(mdb, dump_dir)
mfdb_disconnect(mdb)
mfdb('', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = FALSE)
mfdb_cs_restore(mdb, dump_dir)
ok(cmp(
    mfdb_sample_count(mdb, c(), params = list())[[1]][,'number'],
    sum( 2,  1,  2,   1,  2,  1,   1,  2,  1,   2,  1,  2 ) * 100), "Sample rows all intact (after first import)")
ok(cmp(
    mfdb_stomach_presenceratio(mdb, c(), params = list(prey_species = 'CAP'))[[1]][,'ratio'],
    4 / 6), "Stomach rows all intact (after first import)")
ok(cmp(
    mfdb_stomach_presenceratio(mdb, c(), params = list(prey_species = 'CLL'))[[1]][,'ratio'],
    2 / 6), "Stomach rows all intact (after first import)")

# Import second time should remove existing data
mfdb_cs_restore(mdb, dump_dir)
ok(cmp(
    mfdb_sample_count(mdb, c(), params = list())[[1]][,'number'],
    sum( 2,  1,  2,   1,  2,  1,   1,  2,  1,   2,  1,  2 ) * 100), "Sample rows all intact (after second import)")
ok(cmp(
    mfdb_stomach_presenceratio(mdb, c(), params = list(prey_species = 'CAP'))[[1]][,'ratio'],
    4 / 6), "Stomach rows all intact (after second import)")
ok(cmp(
    mfdb_stomach_presenceratio(mdb, c(), params = list(prey_species = 'CLL'))[[1]][,'ratio'],
    2 / 6), "Stomach rows all intact (after second import)")

# Can still import data (i.e. sequences aren't borked)
mfdb_import_survey(mdb,
    data_source = 'cell4',
    data.frame(
        year = c('1998'),
        month = c(1:12),
        areacell = c('45G03'),
        species = c('COD'),
        #          -----Q1----- -----Q2----- -----Q3----- -----Q4-----
        length = c(213,253,333, 313,133,343, 163,363,233, 333,133,323 ),
        count =  c(  2,  1,  2,   1,  2,  1,   1,  2,  1,   2,  1,  2 )))
mfdb_import_survey(mdb,
    data_source = 'cell5',
    data.frame(
        year = c('1998'),
        month = c(1:12),
        areacell = c('45G03'),
        species = c('COD'),
        #          -----Q1----- -----Q2----- -----Q3----- -----Q4-----
        length = c(213,253,333, 313,133,343, 163,363,233, 333,133,323 ),
        count =  c(  2,  1,  2,   1,  2,  1,   1,  2,  1,   2,  1,  2 )))

# Test we can output a tarball
dump_tar <- paste0(tempfile(), ".tar.gz")
mfdb_cs_dump(mdb, dump_tar)

# Restore it
mfdb_cs_restore(mdb, dump_tar)
ok(cmp(
    mfdb_sample_count(mdb, c(), params = list())[[1]][,'number'],
    sum( 2,  1,  2,   1,  2,  1,   1,  2,  1,   2,  1,  2 ) * 102), "Sample rows all intact (after tar import)")
ok(cmp(
    mfdb_stomach_presenceratio(mdb, c(), params = list(prey_species = 'CAP'))[[1]][,'ratio'],
    4 / 6), "Stomach rows all intact (after tar import)")
ok(cmp(
    mfdb_stomach_presenceratio(mdb, c(), params = list(prey_species = 'CLL'))[[1]][,'ratio'],
    2 / 6), "Stomach rows all intact (after tar import)")
