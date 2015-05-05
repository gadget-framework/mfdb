# This script demonstrates that taxonomies get rebuilt on database connect
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

# Rebuild database, taxonomy got populated
if (exists("mdb")) mfdb_disconnect(mdb)
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = FALSE)
ok(all(mfdb:::mfdb_fetch(mdb, "SELECT name, description FROM species WHERE species_id = 9999999999")[1,] == 
  mfdb::species[mfdb::species$name == 'TBX', c('name', 'description')]), "Entry for 9999999999 matches package")
ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM species")[1,1], nrow(mfdb::species)), "Species has right number of entries")

# Fiddle about with entry
mfdb:::mfdb_import_taxonomy(mdb, 'species', data.frame(id = c(1), name = c('TBX'), description = c('Wormy Worms')))
ok(all(mfdb:::mfdb_fetch(mdb, "SELECT species_id, description FROM species WHERE name = 'TBX'")[1,] == 
  c(9999999999, 'Wormy Worms')), "Entry for 9999999999 was updated")

# Connect as a different case study. shouldn't recreate tables, but should have fixed taxonomy
mdb2 <- mfdb('Baltic', db_params = db_params, save_temp_tables = FALSE)
ok(all(mfdb:::mfdb_fetch(mdb, "SELECT species_id, description FROM species WHERE name = 'TBX'")[1,] == 
  mfdb::species[mfdb::species$name == 'TBX', c('id', 'description')]), "Entry for 9999999999 matches package")

# Can't connect to an unknown case study
ok(cmp_error(mfdb('Camel', db_params = db_params), "Unknown"))
