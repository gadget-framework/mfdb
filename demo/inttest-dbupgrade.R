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
source('mfdb/tests/utils/helpers.R')
source('mfdb/tests/utils/inttest-helpers.R')

# Empty database
if (exists("mdb")) mfdb_disconnect(mdb)

# Rebuild the version 2 schema
mdb <- mfdb('Test', db_params = db_params, destroy_schema = TRUE)
system(paste(
    'echo "DROP SCHEMA public CASCADE" | psql',
    ' --host=', db_params$host,
    ' --dbname=', db_params$dbname,
    sep="", collapse=""))
system(paste(
    'echo "DROP SCHEMA test CASCADE" | psql',
    ' --host=', db_params$host,
    ' --dbname=', db_params$dbname,
    sep="", collapse=""))
system(paste(
    'psql',
    ' --host=', db_params$host,
    ' --dbname=', db_params$dbname,
    ' --file=mfdb/tests/utils/schema_2.x.sql',
    sep="", collapse=""))

# Create a proper MFDB object, should upgrade to latest version
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = FALSE)
ok(cmp(
    mfdb:::mfdb_fetch(mdb, "SELECT MAX(version) FROM mfdb_schema")[1,1],
    as.integer(gsub("\\..*", "", packageVersion("mfdb")))), "Database now at latest release")

# Check schema name edge cases
ok(cmp_error(
    mfdb('public', db_params = db_params, save_temp_tables = FALSE),
    "public"), "Can't connect to the public schema")
ok(cmp_error(
    mfdb('', db_params = db_params, save_temp_tables = FALSE, destroy_schema = TRUE),
    "case_study_name[^X]*\\* test"), "Not allowed to give an empty case_study name when destroying, given list of schemas")
ok(cmp_error(
    mfdb('', db_params = db_params, save_temp_tables = FALSE),
    "case_study_name[^X]*\\* test"), "Not allowed to give an empty case_study name, test schema still there")
