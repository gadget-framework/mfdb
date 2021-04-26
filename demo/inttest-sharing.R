# This script demonstrates sharing schemas between users
#
# ok and ok_group are there so we can run this code and verify it is correct,
# and not needed every-day use.
#
# NB: if you want to run this, you must be using mfdb-workspace, also be warned
# any data stored in your database will be destroyed.
library(DBI)
library(unittest)
library(mfdb)
source('tests/utils/helpers.R')
source('tests/utils/inttest-helpers.R')

# Recreate schema
schema_name <- 'inttest_sharing'
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb(schema_name, db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb(schema_name, db_params = db_params)

# Doesn't make sense without postgresql
if (mdb$db_args$dbname == ':memory:') quit()

mfdb_import_area(mdb, data.frame(
    name = c('45G01', '45G02', '45G03'),
    division = c('divA', 'divA', 'divB'),
    size = c(10,200,400)))
mfdb_import_survey(mdb, data_source = "cod2000", table_string("
year    month   areacell        species length  age     weight
2000    1       45G01           COD     21      2       210
2000    1       45G02           COD     34      3       220
2000    1       45G03           COD     34      3       230
2000    1       45G01           COD     62      1       320
2000    1       45G02           COD     53      1       330
2000    1       45G03           COD     54      2       430
2000    1       45G01           COD     28      2       210
2000    1       45G02           COD     34      3       220
2000    1       45G03           COD     24      3       230
2000    1       45G01           COD     12      1       320
2000    1       45G02           COD     44      1       330
2000    1       45G03           COD     14      2       430
    "))
ok(cmp(mfdb_sample_count(mdb, c(), list())[[1]]$number, 12), "There is data")
more_data <- table_string("
year    month   areacell        species length  age     weight
2001    1       45G01           COD     21      2       210
")

# The user we will use for the sharing test
tryCatch(mfdb:::mfdb_send(mdb, "DROP OWNED BY gelda"), error = function (e) NULL)
gelda_params <- db_params
gelda_params$user <- 'gelda'
gelda_params$password <- 'adleg'

# By default, no access
ok(
    cmp_error(mfdb(schema_name, db_params = gelda_params), 'denied'),
    "Gelda can't connect to the main schema")

# Read-only share
tryCatch({
    mfdb_share_with(mdb, 'gelda')
}, error = function (e) {
    stop("Couldn't share with gelda, maybe they don't exist? Run \"CREATE USER gelda WITH PASSWORD 'adleg'\"")
})
gelda_mdb <- mfdb(schema_name, db_params = gelda_params)
ok(
    cmp(mfdb_sample_count(gelda_mdb, c(), list())[[1]]$number, 12),
    "Now we've granted permissions, they can")
ok(
    cmp_error(mfdb_import_survey(gelda_mdb, data_source = "gelda", more_data), "denied"),
    "But importing isn't allowed")

# Read/write share
mfdb_share_with(mdb, 'gelda', import = TRUE)
gelda_mdb <- mfdb(schema_name, db_params = gelda_params)
ok(
    cmp(mfdb_sample_count(gelda_mdb, c(), list())[[1]]$number, 12),
    "Can fetch data")
ok({
    mfdb_import_survey(gelda_mdb, data_source = "gelda", more_data)
    TRUE
}, "Can add data")
ok(
    cmp(mfdb_sample_count(gelda_mdb, c(), list())[[1]]$number, 13),
    "Data appears in subsequent queries")
ok(
    cmp_error(mfdb:::mfdb_send(gelda_mdb, "DROP TABLE inttest_sharing.sample"), "must be owner"),
    "Not allowed to do DDL changes")

# Revoke permissions
mfdb_share_with(mdb, 'gelda', import = FALSE)
gelda_mdb <- mfdb(schema_name, db_params = gelda_params)
ok(
    cmp_error(mfdb_import_survey(gelda_mdb, data_source = "gelda", more_data), "denied"),
    "Importing no longer allowed")

mfdb_share_with(mdb, 'gelda', query = FALSE, import = FALSE)
ok(
    cmp_error(mfdb(schema_name, db_params = gelda_params), 'denied'),
    "Gelda can no longer connect")

# Use public
mfdb_share_with(mdb, 'public')
ok(
    cmp(mfdb_sample_count(gelda_mdb, c(), list())[[1]]$number, 13),
    "Now we've granted permissions, they can")
ok(
    cmp_error(mfdb_import_survey(gelda_mdb, data_source = "gelda", more_data), "denied"),
    "But importing isn't allowed")
mfdb_share_with(mdb, 'public', query = FALSE, import = FALSE)
