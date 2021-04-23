# This script demonstrates upgrading a database
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

# Empty database
if (exists("mdb")) mfdb_disconnect(mdb)

# Create a connection to gather DB args
mdb <- mfdb('inttest-dbupgrade', db_params = db_params)
db_args <- mdb$db_args
mfdb_disconnect(mdb)

if (db_args$dbname != ':memory:') {
    # Rebuild the version 2 schema
    mdb <- mfdb('Test', db_params = db_params, destroy_schema = TRUE)
    system(paste(
        'echo "DROP SCHEMA public CASCADE" | psql',
        ' --host=', db_args$host,
        ' --dbname=', db_args$dbname,
        sep="", collapse=""))
    system(paste(
        'echo "DROP SCHEMA test CASCADE" | psql',
        ' --host=', db_args$host,
        ' --dbname=', db_args$dbname,
        sep="", collapse=""))
    system(paste(
        'psql',
        ' --host=', db_args$host,
        ' --dbname=', db_args$dbname,
        ' --file=tests/utils/schema_2.x.sql',
        sep="", collapse=""))

    # Create a proper MFDB object, should upgrade to latest version
    mdb <- mfdb('Test', db_params = db_params, save_temp_tables = FALSE)
    ok(cmp(
        mfdb:::mfdb_fetch(mdb, "SELECT MAX(version) FROM mfdb_schema")[1,1],
        as.integer(gsub("\\..*", "", packageVersion("mfdb")))), "Database now at latest release")

    # Can add more data after upgrading
    mfdb_import_survey(mdb, data_source = "had_post_upgrade", table_string("
    year    month   areacell        species length  age     weight
    2000    1       45G01           HAD     21      2       210
        "))

    # Check schema name edge cases
    ok(cmp_error(
        mfdb('public', db_params = db_params, save_temp_tables = FALSE),
        "public"), "Can't connect to the public schema")
    ok(cmp_error(
        mfdb('', db_params = db_params, save_temp_tables = FALSE, destroy_schema = TRUE),
        "schema_name[^X]*\\* test"), "Not allowed to give an empty schema name when destroying, given list of schemas")
    ok(cmp_error(
        mfdb('', db_params = db_params, save_temp_tables = FALSE),
        "schema_name[^X]*\\* test"), "Not allowed to give an empty schema name, test schema still there")

    # Drop public schema so we don't import this data later
    mdb <- mfdb('public', db_params = db_params, destroy_schema = TRUE)
}

if (db_args$dbname != ':memory:') {
    # Rebuild a version 5 schema and upgrade it
    system(paste(
        'echo "DROP SCHEMA test_5x CASCADE" | psql',
        ' --host=', db_args$host,
        ' --dbname=', db_args$dbname,
        sep="", collapse=""))
    system(paste(
        'pg_restore',
        ' --host=', db_args$host,
        ' --dbname=', db_args$dbname,
        ' tests/utils/schema_5.x.dump',
        sep="", collapse=""))
    mdb <- mfdb('test_5x', db_params = db_params)
    ok(cmp(
        mfdb:::mfdb_fetch(mdb, "SELECT MAX(version) FROM mfdb_schema")[1,1],
        as.integer(gsub("\\..*", "", packageVersion("mfdb")))), "Database now at latest release")
}
