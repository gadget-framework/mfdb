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
system(paste(
    'pg_restore',
    ' --clean',
    ' --host=', db_params$host,
    ' --dbname=', db_params$dbname,
    ' mfdb/tests/utils/schema_2.x.sql',
    sep="", collapse=""))

# Create a proper MFDB object, should upgrade to latest version
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = FALSE)
ok(cmp(
    mfdb:::mfdb_fetch(mdb, "SELECT MAX(version) FROM mfdb_schema")[1,1],
    as.integer(gsub("\\..*", "", packageVersion("mfdb")))), "Database now at latest release")
