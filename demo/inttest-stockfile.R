# This script demonstrates creating stock files
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

# Empty database & rebuild
mfdb('', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = TRUE)

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(10,200,400)))
mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G03')))

ok_group("Stockfile example", {
    # Create a temporary gadget directory
    gd <- gadget_directory(tempfile())

    # Import a survey for the data we are interested in
    mfdb_import_survey(mdb, data_source = "cod2000",
        table_string("
year	month	areacell	species	maturity_stage	length	age	weight
2000	1	45G01		COD	1		21	2	210
2000	1	45G01		COD	5		34	3	220
2000	1	45G01		COD	5		34	3	230
2000	1	45G01		COD	5		62	1	320
2000	1	45G01		COD	5		53	1	330
2000	1	45G01		COD	5		54	2	430
2000	1	45G01		COD	1		28	2	210
2000	1	45G01		COD	5		34	3	220
2000	1	45G01		COD	1		24	3	230
2000	1	45G01		COD	1		12	1	320
2000	1	45G01		COD	5		44	1	330
2000	1	45G01		COD	1		14	2	430
        "))

    # Query min/max age, and average weight for our length groups (Immature cod)
    imm_data <- mfdb_sample_meanweight(mdb, c('age', 'length'), list(
        age = NULL, # This means the age column will have "all", but aggfile will have min & max
        length = mfdb_step_interval('', 10, to = 100),
        species = 'COD',
        maturity_stage = '1',
        null = NULL))

    # Write both min/max and refweighfile into our gadget directory
    gadget_dir_write(gd, gadget_stockfile_extremes('cod.imm', imm_data[[1]]))
    gadget_dir_write(gd, gadget_stockfile_refweight('cod.imm', imm_data[[1]]))

    # Do the same for mature cod
    mat_data <- mfdb_sample_meanweight(mdb, c('age', 'length'), list(
        age = NULL, # This means the age column will have "all", but aggfile will have min & max
        length = mfdb_step_interval('len', 10, to = 100),
        species = 'COD',
        maturity_stage = '5',
        null = NULL))
    gadget_dir_write(gd, gadget_stockfile_extremes('cod.mat', mat_data[[1]]))
    gadget_dir_write(gd, gadget_stockfile_refweight('cod.mat', mat_data[[1]]))

    # Stockfiles mentioned in the mainfile
    ok(cmp_file(gd, "main",
        ver_string,
        "timefile\t",
        "areafile\t",
        "printfiles\t; Required comment",
        "[stock]",
        "stockfiles\tModelfiles/cod.imm\tModelfiles/cod.mat",
        "[tagging]",
        "[otherfood]",
        "[fleet]",
        "[likelihood]",
        NULL), "Mainfile mentions both new stockfiles")

    ok(cmp_file(gd, "Modelfiles/cod.imm",
        ver_string,
        "refweightfile\tModelfiles/cod.imm.refwgt",
        "stockname\tcod.imm",
        "minage\t1",
        "maxage\t2",
        "minlength\t10",
        "maxlength\t20",
        NULL), "Immature cod stockfile")
    ok(cmp_file(gd, "Modelfiles/cod.imm.refwgt",
        ver_string,
        "; -- data --",
        "; length\tmean",
        "10\t375",
        "20\t216.666666666667",
        NULL), "Immature cod stock refweightfile")

    ok(cmp_file(gd, "Modelfiles/cod.mat",
       ver_string,
        "refweightfile\tModelfiles/cod.mat.refwgt",  #TODO: Can we have these on the end?
        "stockname\tcod.mat",
        "minage\t3",
        "maxage\t3",
        "minlength\tlen30",
        "maxlength\tlen60",
        NULL), "Mature cod stockfile")
    ok(cmp_file(gd, "Modelfiles/cod.mat.refwgt",
        ver_string,
        "; -- data --",
        "; length\tmean",
        "len30\t223.333333333333",  #TODO: Rounding?
        "len40\t330",
        "len50\t380",
        "len60\t320",
        NULL), "Mature cod stock refweightfile")
})
