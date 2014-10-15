# Script to exercise mfdb
# NB: To use this you *must* be using mfdb-workspace and not mind loosing data
library(unittest)
library(mfdb)
source('mfdb/tests/utils/helpers.R')
source('mfdb/tests/utils/inttest-helpers.R')

# Empty database
conn <- db_connection()
remove_mfdb_tables(conn)

# Rebuild database, taxonomy got populated
mdb <- mfdb('Iceland', db_params = db_params, save_temp_tables = TRUE)
ok(all(mfdb:::mfdb_fetch(mdb, "SELECT name, description FROM species WHERE species_id = 9999999999")[1,] == 
  mfdb::species[mfdb::species$name == 'TBX', c('name', 'description')]), "Entry for 9999999999 matches package")
ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM species")[1,1], nrow(mfdb::species)), "Species has right number of entries")

# Fiddle about with entry
mfdb:::mfdb_import_taxonomy(mdb, 'species', data.frame(id = c('9999999999'), name = c('XXX'), description = c('Wormy Worms')))
ok(all(mfdb:::mfdb_fetch(mdb, "SELECT name, description FROM species WHERE species_id = 9999999999")[1,] == 
  c('XXX', 'Wormy Worms')), "Entry for 9999999999 was updated")

# Try again, shouldn't recreate tables, but should have fixed taxonomy
mfdb_disconnect(mdb)
mdb <- mfdb('Iceland', db_params = db_params, save_temp_tables = TRUE)
ok(all(mfdb:::mfdb_fetch(mdb, "SELECT name, description FROM species WHERE species_id = 9999999999")[1,] == 
  mfdb::species[mfdb::species$name == 'TBX', c('name', 'description')]), "Entry for 9999999999 matches package")

section("Areacell/division import", function() {
    # Can't populate divisions yet, no areacells defined
    ok(cmp_error(mfdb_import_division(mdb, list(divA = c('45G01', '45G02', '45G03'))), 'areacell vocabulary'), "Areacell not populated yet")

    # So define some, so it should work
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(5)))
    mfdb_import_division(mdb, list(divA = c('45G01', '45G02', '45G03')))
    ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM division")[1,1], 3), "Inserted 3 rows into division")

    # Can't populate with invalid areacells, divC was rolled back so still 3 rows
    ok(cmp_error(mfdb_import_division(mdb, list(divB = c('45G01', '45G02', '45G08'), divC = c('45G01'))), 'areacell vocabulary'), "Invalid areacell values")
    ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM division")[1,1], 3), "Inserted 3 rows into division")

    # Worked this time
    mfdb_import_division(mdb, list(divB = c('45G01', '45G02'), divC = c('45G01')))
    ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM division")[1,1], 6), "Inserted 6 rows into division")
})

section("Temperature import", function() {
    # Notice missing columns
    ok(cmp_error(mfdb_import_temperature(mdb, data.frame(
        year = c(1998),
        areacell_id = c('45G01', '45G01', '45G01'),
        temperature = c(1,2,4))), "month"), "Notice month column is missing")

    # Import works, and removes old data
    mfdb_import_temperature(mdb, data.frame(
        year = c(1998),
        month = c(1,2,3),
        areacell = c('45G01', '45G01', '45G01'),
        temperature = c(1,2,4)))
    mfdb_import_temperature(mdb, data.frame(
        year = c(1998),
        month = c(4,5,6),
        areacell = c('45G01', '45G01', '45G01'),
        temperature = c(1,2,4)))
})
