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
    agg_data <- mfdb_sample_meanlength(mdb, c('age'), list(
            year = 1998:2000,
            area = mfdb_group(divA = c("divA")),
            timestep = mfdb_timestep_biannually,
            age = mfdb_group(all = 1:1000),
            length = mfdb_interval("len", seq(0, 50, by = 5))))

    # Write it into a likelihood component
    # NB: We use agg_data[[1]] since mfdb_sample_meanlength() could have returned
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

ok_group("Maturity stage samples", {
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
            maturity_stage = c(  1,  2,  3,  1,  2,  3,   1,  2,  3,  1,  2,  3),
            age =            c(  1,  2,  1,  2,  1,  2,   1,  2,  1,  2,  1,  2),
            length =         c( 10, 50, 30, 10, 35, 46,  65, 62, 36, 35, 34, 22),
            count =          c(100,500,300,100,350,460, 650,320,360,350,340,220)))

    # Initalise a gadget directory to output into
    gd <- gadget_directory(tempfile())

    # Treat the maturity stage as a stock, divide up into mature and immature
    agg_data <- mfdb_sample_count(mdb, c('maturity_stage', 'age', 'length'), list(
            year = 1998:2000,
            area = mfdb_group(divA = c("divA")),
            length = mfdb_step_interval('len', by = 10, to = 100),
            timestep = mfdb_timestep_biannually,
            maturity_stage = mfdb_group(imm = 1, mat = 2:5)))

    # Write it into a likelihood component
    gadget_dir_write(gd, gadget_likelihood_component(
        'stockdistribution', data = agg_data[[1]]))

    # Likelihood file has a component
    ok(cmp_file(gd, "likelihood",
        ver_string,
        "; ",
        "[component]",
        "name\tstockdistribution",
        "weight\t0",
        "type\tstockdistribution",
        "datafile\tData/stockdistribution.stockdistribution.sumofsquares",
        "function\tsumofsquares",
        "overconsumption\t0",
        "epsilon\t10",
        "areaaggfile\tAggfiles/stockdistribution.stockdistribution.area.agg",
        "ageaggfile\tAggfiles/stockdistribution.stockdistribution.age.agg",
        "lenaggfile\tAggfiles/stockdistribution.stockdistribution.len.agg",
        "fleetnames\t",
        "stocknames\t"
        ), "Likelihood file updated")

    # Data files written
    ok(cmp_file(gd, "Data/stockdistribution.stockdistribution.sumofsquares",
        ver_string,
        "; -- data --",
        "; year\tstep\tarea\tstock\tage\tlength\tnumber",
        "1998\t1\tdivA\timm\tall\tlen10\t200",
        "1998\t1\tdivA\tmat\tall\tlen30\t650",
        "1998\t1\tdivA\tmat\tall\tlen40\t460",
        "1998\t1\tdivA\tmat\tall\tlen50\t500",
        "1998\t2\tdivA\timm\tall\tlen30\t350",
        "1998\t2\tdivA\timm\tall\tlen60\t650",
        "1998\t2\tdivA\tmat\tall\tlen20\t220",
        "1998\t2\tdivA\tmat\tall\tlen30\t700",
        "1998\t2\tdivA\tmat\tall\tlen60\t320",
        NULL), "datafile updated")
    ok(cmp_file(gd, "Aggfiles/stockdistribution.stockdistribution.area.agg",
        ver_string,
        "divA\t1",
        NULL), "area aggregation file")
    ok(cmp_file(gd, "Aggfiles/stockdistribution.stockdistribution.age.agg",
        ver_string,
        "all\tX",
        NULL), "age aggregtaion file")
    ok(cmp_file(gd, "Aggfiles/stockdistribution.stockdistribution.len.agg",
        ver_string,
        "len0\t0\t10",
        "len10\t10\t20",
        "len20\t20\t30",
        "len30\t30\t40",
        "len40\t40\t50",
        "len50\t50\t60",
        "len60\t60\t70",
        "len70\t70\t80",
        "len80\t80\t90",
        "len90\t90\t100",
        NULL), "len aggregation file")
})
