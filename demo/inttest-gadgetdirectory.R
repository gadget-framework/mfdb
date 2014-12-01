# This script demonstrates the full process from import to writing to a GADGET
# directory.
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

# Empty database, so we start from scratch
conn <- db_connection()
remove_mfdb_tables(conn)

# Open a connection to the DB for the Iceland case study, and open an output
# directory to save files in
mdb <- mfdb('Iceland', db_params = db_params, save_temp_tables = TRUE)
gd <- gadget_directory(tempfile())

ok_group("Area File", {
    # 3 area cells 45G01--3, each of which are 5km^2
    mfdb_import_area(mdb, data.frame(
        id = c(1,2,3),
        name = c('45G01', '45G02', '45G03'),
        size = c(5)))

    # Divide area up into 2 divisions, divA and divB
    mfdb_import_division(mdb, list(
        divA = c('45G01'),
        divB = c('45G02', '45G03')))

    # Import temperature data for these areas
    mfdb_import_temperature(mdb, data.frame(
        year = rep(c(1998, 1999), each = 12),
        month = c(1:12, 1:12),
        areacell = c(rep('45G01', times = 24)),
        temperature = c(1:12, 25:36)))

    # Define a grouping for area, a for divA and b for divB
    area_group <- mfdb_group(a = c("divA"), b = c("divB"))

    # Group area and temperature data into a & b, group temperature data into
    # quarterly periods, write the lot out into a gadget areafile
    gadget_dir_write(gd, gadget_areafile(
        mfdb_area_size(mdb, list(area = area_group))[[1]],
        mfdb_temperature(mdb, list(year = c(1998, 1999), timestep = mfdb_timestep_quarterly, area = area_group))[[1]]
        ))

    # Resulting areafile contains the following data
    ok(cmp_file(gd, "area",
        ver_string,
        "areas\ta\tb",
        "size\t5\t10",
        "temperature\t",
        "; -- data --",
        "; year\tstep\tarea\ttemperature",
        "1998\t1\ta\t2",
        "1998\t2\ta\t5",
        "1998\t3\ta\t8",
        "1998\t4\ta\t11",
        "1999\t1\ta\t26",
        "1999\t2\ta\t29",
        "1999\t3\ta\t32",
        "1999\t4\ta\t35"
        ), "Areafile on disk matches")
})
