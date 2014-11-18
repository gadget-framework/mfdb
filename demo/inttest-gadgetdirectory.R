# Script to demonstrate building gadget directories from a DB
# NB: To use this you *must* be using mfdb-workspace and not mind loosing data
library(unittest)
library(mfdb)
source('mfdb/tests/utils/helpers.R')
source('mfdb/tests/utils/inttest-helpers.R')

# Empty database & rebuild
conn <- db_connection()
remove_mfdb_tables(conn)
mdb <- mfdb('Iceland', db_params = db_params, save_temp_tables = TRUE, create_schema = TRUE)
gd <- gadget_directory(tempfile())

ok_group("Area File", {
    # Import data
    mfdb_import_area(mdb, data.frame(
        id = c(1,2,3),
        name = c('45G01', '45G02', '45G03'),
        size = c(5)))
    mfdb_import_division(mdb, list(
        divA = c('45G01'),
        divB = c('45G02', '45G03')))
    mfdb_import_temperature(mdb, data.frame(
        year = rep(c(1998, 1999), each = 12),
        month = c(1:12, 1:12),
        areacell = c(rep('45G01', times = 24)),
        temperature = c(1:12, 25:36)))

    # Create an areafile
    area_group <- mfdb_group(a = c("divA"), b = c("divB"))
    gadget_dir_write(gd, gadget_areafile(
        mfdb_area_size(mdb, list(area = area_group))[[1]],
        mfdb_temperature(mdb, list(year = c(1998, 1999), timestep = step_quarters, area = area_group))[[1]]
        ))

    # Looks like this
    ok(cmp_file(gd, "area",
        ver_string,
        "areas\ta\tb",
        "size\t5\t10",
        "temperature\t",
        "; -- data --",
        "; year\tstep\tarea\ttemperature",
        "1998\tq1\ta\t2",
        "1998\tq2\ta\t5",
        "1998\tq3\ta\t8",
        "1998\tq4\ta\t11",
        "1999\tq1\ta\t26",
        "1999\tq2\ta\t29",
        "1999\tq3\ta\t32",
        "1999\tq4\ta\t35"
        ), "Areafile on disk matches")
})
