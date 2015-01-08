# This script demonstrates bootstrap sampling an area
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

# Empty database
mfdb('', destroy_schema = TRUE)

# Rebuild database, taxonomy got populated
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = TRUE)
ok(all(mfdb:::mfdb_fetch(mdb, "SELECT name, description FROM species WHERE species_id = 9999999999")[1,] == 
  mfdb::species[mfdb::species$name == 'TBX', c('name', 'description')]), "Entry for 9999999999 matches package")
ok(cmp(mfdb:::mfdb_fetch(mdb, "SELECT count(*) FROM species")[1,1], nrow(mfdb::species)), "Species has right number of entries")

ok_group("Unaggregated length / weight / age samples", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(10,200,400)))
    mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G03')))

    # Import data for each areacell
    mfdb_import_survey(mdb,
        data_source = 'cell1',
        data.frame(
            year = c('1998'),
            month = c(1:12),
            areacell = c('45G01'),
            species = c('COD'),
            length = c(110,150,130,110,135,146, 165,162,136,135,134,122)))
    mfdb_import_survey(mdb,
        data_source = 'cell2',
        data.frame(
            year = c('1998'),
            month = c(1:12),
            areacell = c('45G02'),
            species = c('COD'),
            length = c(210,250,230,210,235,246, 265,262,236,235,234,222)))
    mfdb_import_survey(mdb,
        data_source = 'cell3',
        data.frame(
            year = c('1998'),
            month = c(1:12),
            areacell = c('45G03'),
            species = c('COD'),
            length = c(310,350,330,310,335,346, 365,362,336,335,334,322)))

    # Bootstrap sample area sizes
    set.seed(35)
    area_group <- mfdb_group(all = c("divA", "divB"))
    area_bootstrap_group <- mfdb_bootstrap_group(5, area_group)
    agg <- mfdb_area_size(mdb, params = list(area = area_bootstrap_group))
    ok(cmp(length(agg), 5), "Got 5 distinct groupings")
    ok(cmp(attr(agg[[1]], "area"), list(all = c("divB", "divA"))), "divB, divA")
    ok(cmp(agg[[1]]$size, 400 + 200 + 10), "divB + divA")
    ok(cmp(attr(agg[[2]], "area"), list(all = c("divB", "divB"))), "divB, divB")
    ok(cmp(agg[[2]]$size, 400 + 400), "divB + divB")
    ok(cmp(attr(agg[[3]], "area"), list(all = c("divA", "divA"))), "divA, divA")
    ok(cmp(agg[[3]]$size, 200 + 10 + 200 + 10), "divA + divA")
    ok(cmp(attr(agg[[4]], "area"), list(all = c("divA", "divB"))), "divA, divB")
    ok(cmp(agg[[4]]$size, 400 + 200 + 10), "divA + divB")
    ok(cmp(attr(agg[[5]], "area"), list(all = c("divB", "divB"))), "divB, divB")
    ok(cmp(agg[[5]]$size, 400 + 400), "divB + divB")
})
