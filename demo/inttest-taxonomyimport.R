# This script demonstrates that taxonomies get rebuilt on database connect
#
# ok and ok_group are there so we can run this code and verify it is correct,
# and not needed every-day use.
#
# NB: if you want to run this, you must be using mfdb-workspace, also be warned
# any data stored in your database will be destroyed.
library(unittest)
library(mfdb)
source('tests/utils/helpers.R')
source('tests/utils/inttest-helpers.R')

# Rebuild database, taxonomy got populated
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb(gsub("inttest", "inttest-taxonomyimport", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb(gsub("inttest", "inttest-taxonomyimport", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, save_temp_tables = FALSE)

ok(ut_cmp_identical(
    mfdb:::mfdb_fetch(mdb, "SELECT name, description FROM species WHERE name = 'TBX'"),
    data.frame(
        name = 'TBX',
        description = as.character(mfdb::species[mfdb::species$name == 'TBX', 'description']),
        stringsAsFactors = FALSE)), "Entry for 9999999999 matches package")
ok(cmp(as.integer(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM species")[1,1]), nrow(mfdb::species)), "Species has right number of entries")

# Fiddle about with entry
mfdb:::mfdb_import_taxonomy(mdb, 'species', data.frame(id = c(1), name = c('TBX'), description = c('Wormy Worms')))
ok(ut_cmp_equal(  # NB: Not identical since 9999999999 is too big to be integer
    mfdb:::mfdb_fetch(mdb, "SELECT species_id, description FROM species WHERE name = 'TBX'"),
    data.frame(
        species_id = 9999999999,
        description = 'Wormy Worms',
        stringsAsFactors = FALSE)), "Entry for 9999999999 was updated")

# Connect as a different case study. should have it's own tables, not affected by the above
mfdb(gsub("inttest", "inttest-taxonomyimport Baltic", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, destroy_schema = TRUE)
mdb2 <- mfdb(gsub("inttest", "inttest-taxonomyimport Baltic", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, save_temp_tables = FALSE)
ok(cmp(
    mfdb:::mfdb_fetch(mdb2, "SELECT CAST(species_id AS TEXT) AS species_id, description FROM species WHERE name = 'TBX'")[1,],
    data.frame(
        species_id = '9999999999',
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
ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT * FROM sampling_type ORDER BY name"), data.frame(
    sampling_type_id = as.integer(c(2,1,3,4)),
    name = c("L", "S", "S1", "S2"),
    t_group = as.character(NA),
    description = c("Commercial Landings", "Survey", "Survey 1", "Survey 2"),
    stringsAsFactors = FALSE)), "Updated sampling types")


# Create some tows, then add a larger set. Make sure the regenerated IDs don't clash
mfdb_import_tow_taxonomy(mdb, data.frame(
    name = paste0("ta", 1:4),
    description = paste0("Tow A", 1:4),
    stringsAsFactors = TRUE))
mfdb_import_tow_taxonomy(mdb, data.frame(
    name = sprintf("tb%02d", 1:24),
    description = sprintf("Tow B %02d", 1:24),
    stringsAsFactors = TRUE))
ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT tow_id, name FROM tow ORDER BY name"), data.frame(
    tow_id = as.integer(c(1:4, 1:24 + 4)),  # NB: All IDs incremented
    name = c(paste0("ta", 1:4), sprintf("tb%02d", 1:24)),
    stringsAsFactors = FALSE)), "Imported 2 tow taxonomies")

mfdb_disconnect(mdb)
