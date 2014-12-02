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
mfdb('', destroy_schema = TRUE)

# Open a connection to the DB for the Iceland case study, and open an output
# directory to save files in
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = TRUE)

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

    # Initalise a gadget directory to output into
    gd <- gadget_directory(tempfile())

    # Define a grouping for area, a for divA and b for divB
    area_group <- mfdb_group(a = c("divA"), b = c("divB"))

    # Group area and temperature data into a & b, group temperature data into
    # quarterly periods, write the lot out into a gadget areafile
    gadget_dir_write(gd, gadget_areafile(
        mfdb_area_size(mdb, list(area = area_group))[[1]],
        mfdb_temperature(mdb, list(year = c(1998, 1999), timestep = mfdb_timestep_quarterly, area = area_group))[[1]]
        ))

    # Resulting areafile contains the following data
    ok(cmp_file(gd, "Modelfiles/area",
        ver_string,
        "; a\tb",
        "areas\t1\t2",
        "size\t5\t10",
        "temperature\t",
        "; -- data --",
        "; year\tstep\tarea\ttemperature",
        "1998\t1\t1\t2",
        "1998\t2\t1\t5",
        "1998\t3\t1\t8",
        "1998\t4\t1\t11",
        "1999\t1\t1\t26",
        "1999\t2\t1\t29",
        "1999\t3\t1\t32",
        "1999\t4\t1\t35"
        ), "Areafile on disk matches")
})

ok_group("Length / weight / age samples", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(5)))
    mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G01')))

    # Import a survey
    mfdb_import_survey(mdb,
        data_source = 'survey1',
        data.frame(
            year = c('1998'),
            month = c(1:12),
            areacell = c('45G01'),
            species = c('COD'),
            age =    c(  1,  2,  1,  2,  1,  2,   1,  2,  1,  2,  1,  2),
            length = c( 10, 50, 30, 10, 35, 46,  65, 62, 36, 35, 34, 22),
            weight = c(100,500,300,100,350,460, 650,320,360,350,340,220)))

    # Initalise a gadget directory to output into
    gd <- gadget_directory(tempfile())

    # Get the mean length data from the database
    agg_data <- mfdb_meanlength(mdb, list(
            year = 1998:2000,
            area = mfdb_group(divA = c("divA")),
            timestep = mfdb_timestep_biannually,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 50, by = 5))))

    # Write it into a likelihood component
    # NB: We use agg_data[[1]] since mfdb_meanlength() could have returned
    # multiple bootstrap samples, in this case we didn't use bootstrapping, so
    # only got one.
    gadget_dir_write(gd, gadget_likelihood_component('catchstatistics', data = agg_data[[1]]))

    # Likelihood file has a component
    ok(cmp_file(gd, "likelihood",
        ver_string,
        "; ",
        "[component]",
        "name\tcatchstatistics",
        "weight\t0",
        "type\tcatchstatistics",
        "datafile\tData/catchstatistics.catchstatistics.lengthnostddev",
        "function\tlengthnostddev",
        "areaaggfile\tAggfiles/catchstatistics.catchstatistics.area.agg",
        "ageaggfile\tAggfiles/catchstatistics.catchstatistics.age.agg",
        "fleetnames\t",
        "stocknames\t"
        ), "Likelihood file updated")

    # Data files written
    ok(cmp_file(gd, "Data/catchstatistics.catchstatistics.lengthnostddev",
        ver_string,
        "; -- data --",
        "; year\tstep\tarea\tage\tnumber\tmean",
        "1998\t1\tdivA\tall\t5\t26.2",
        "1998\t2\tdivA\tall\t4\t31.75"
        ), "datafile updated")
    ok(cmp_file(gd, "Aggfiles/catchstatistics.catchstatistics.area.agg",
        ver_string,
        "divA\t1"
        ), "areafile updated")
    ok(cmp_file(gd, "Aggfiles/catchstatistics.catchstatistics.age.agg",
        ver_string,
        paste0("all\t", paste(1:1000, collapse = "\t"))
        ), "age updated")
})
