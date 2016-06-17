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
mfdb('Test', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = FALSE)

ok(all(mfdb:::mfdb_fetch(mdb, "SELECT name, description FROM species WHERE species_id = 9999999999")[1,] == 
  mfdb::species[mfdb::species$name == 'TBX', c('name', 'description')]), "Entry for 9999999999 matches package")
ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM species")[1,1], nrow(mfdb::species)), "Species has right number of entries")

# Fiddle about with entry
mfdb:::mfdb_import_taxonomy(mdb, 'species', data.frame(id = c(1), name = c('TBX'), description = c('Wormy Worms')))
ok(all(mfdb:::mfdb_fetch(mdb, "SELECT species_id, description FROM species WHERE name = 'TBX'")[1,] == 
  c(9999999999, 'Wormy Worms')), "Entry for 9999999999 was updated")

# Connect as a different case study. should have it's own tables, not affected by the above
mdb2 <- mfdb('Test Baltic', db_params = db_params, save_temp_tables = FALSE)
ok(cmp(
    mfdb:::mfdb_fetch(mdb2, "SELECT species_id, description FROM species WHERE name = 'TBX'")[1,],
    data.frame(
        species_id = 9999999999,
        description = as.character(mfdb::species[mfdb::species$name == 'TBX', 'description']),
        stringsAsFactors = FALSE)), "Test Baltic's schema is distinct")

# Check we can create sampling types
mfdb_import_sampling_type(mdb, data.frame(
    name = c("S", "L"),
    description = c("Survey", "Landings"),
    stringsAsFactors = FALSE))
ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT * FROM sampling_type"), data.frame(
    sampling_type_id = as.integer(1:2),
    name = c("S", "L"),
    t_group = as.character(NA),
    description = c("Survey", "Landings"),
    stringsAsFactors = FALSE)), "Inserted sampling types")

# Now add 2, modify 1...
mfdb_import_sampling_type(mdb, data.frame(
    name = c("S1", "S2", "L"),
    description = c("Survey 1", "Survey 2", "Commercial Landings"),
    stringsAsFactors = FALSE))
ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT * FROM sampling_type"), data.frame(
    sampling_type_id = as.integer(c(1,3,4,2)),
    name = c("S", "S1", "S2", "L"),
    t_group = as.character(NA),
    description = c("Survey", "Survey 1", "Survey 2", "Commercial Landings"),
    stringsAsFactors = FALSE)), "Updated sampling types")
