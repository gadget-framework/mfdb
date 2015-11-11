# This script demonstrates storing and retrieving stomach data
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

cmp_table <- function(tbls, expected) {
    ok(cmp(length(tbls), 1), "result only returned one data.frame")
    cmp(tbls[[1]][names(tbls[[1]])], expected)
}

# Empty database & rebuild
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = FALSE)

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(10,200,400)))
mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G03')))

ok_group("Stomach data", {
    # Import a stomach survey
    mfdb_import_stomach(mdb,
        data_source = "cod2000",
        predator_data = shuffle_df(table_string("
stomach_name	year	month	areacell	species	length	weight
A		2000	1	45G01		COD	21	210
B		2000	1	45G01		COD	34	220
C		2000	1	45G01		COD	34	230

D		2000	1	45G01		COD	62	320
E		2000	1	45G01		COD	33	330

G		2000	1	45G01		COD	34	430
        ")),
        prey_data = shuffle_df(table_string("
stomach_name	species	digestion_stage	length	weight	count
A		CAP		1	1	10	5
A		CAP		1	4	40	1
B		CAP		1	1	10	5
B		CAP		4	1	10	5
B		CAP		5	1	10	8
B		CAP		5	1	10	5
C		CLL		2	3.5	9.5	3

D		CAP		1	1.4	10	1
D		CLL		5	4	40	1
E		CAP		1	1.4	10	1
        ")))

     # Find out the ratio of capelin in stomachs
     ok(cmp_table(
         mfdb_stomach_presenceratio(mdb, c("predator_weight"), list(
             predator_weight = mfdb_interval("w", c(200,300,400,500)),
             prey_species = 'CAP')),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             predator_weight = c('w200', 'w300'),
             ratio = c(
                 # CAP in A, B out of A, B, C
                 2 / 3,
                 # CAP in D, E out of D, E
                 2 / 2,
                 NULL),
             stringsAsFactors = FALSE)), "Aggregated & got ratio")

     # Find out the ratio of capelin, grouped by digestion stage
     ok(cmp_table(
         mfdb_stomach_presenceratio(mdb, c("predator_weight", "digestion_stage"), list(
             predator_weight = mfdb_interval("w", c(200,300,400,500)),
             digestion_stage = mfdb_group(undigested = 1, digested = 2:5),
             prey_species = 'CAP')),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             predator_weight = c('w200', 'w200', 'w300'),
             digestion_stage = c('digested', 'undigested', 'undigested'),
             ratio = c(
                 # digested CAP in B (stages 4 & 5) out of A, B, C
                 1 / 3,
                 # undigested CAP in A, B out of A, B, C
                 2 / 3,
                 # undigested CAP in D, E out of D, E
                 2 / 2,
                 NULL),
             stringsAsFactors = FALSE)), "Aggregated & got ratio")

    # Find out the length distribution of all prey
    ok(cmp_table(
         mfdb_stomach_preycount(mdb, c("digestion_stage", "prey_length"), list(
             digestion_stage = mfdb_group(undigested = 1, digested = 2:5),
             prey_length = mfdb_step_interval("pl", to = 20, by = 1))),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             digestion_stage = c('digested', 'digested', 'digested', 'undigested', 'undigested'),
             prey_length = c('pl1', 'pl3', 'pl4', 'pl1', 'pl4'),
             number = c(
                 5 + 8 + 5,
                 3,
                 1,
                 5 + 5 + 1 + 1,
                 1,
             NULL),
             stringsAsFactors = FALSE)), "Length distribution of all prey")

    # Find out the mean length of all prey
    ok(cmp_table(
         mfdb_stomach_preymeanlength(mdb, c("digestion_stage"), list(
             digestion_stage = mfdb_group(undigested = 1, digested = 2:5))),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             digestion_stage = c('digested', 'undigested'),
             number = c(sum(5, 8, 5, 3, 1), sum(5, 1, 5, 1, 1)),
             mean_length = c(
                 weighted.mean(c(1, 1, 1, 3.5, 4),
                               c(5, 8, 5, 3,   1)),
                 weighted.mean(c(1, 4, 1, 1.4, 1.4),
                               c(5, 1, 5, 1,   1)),
             NULL),
             stringsAsFactors = FALSE)), "Mean length of all prey")

    # Find out the mean weight of all prey
    ok(cmp_table(
         mfdb_stomach_preymeanweight(mdb, c("digestion_stage"), list(
             digestion_stage = mfdb_group(undigested = 1, digested = 2:5))),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             digestion_stage = c('digested', 'undigested'),
             number = c(sum(5, 8, 5, 3, 1), sum(5, 1, 5, 1, 1)),
             mean_weight = c(
                 weighted.mean(c(10, 10, 10, 9.5, 40),
                               c( 5,  8,  5,   3,  1)),
                 weighted.mean(c(10, 40, 10, 10, 10),
                               c( 5,  1,  5,  1,  1)),
             NULL),
             stringsAsFactors = FALSE)), "Mean weight of all prey")
})

ok_group("Stomach content likelihood compoment", {
     gd <- gadget_directory(tempfile())
     # Find out the ratio of capelin in stomachs
     res <- mfdb_stomach_presenceratio(mdb, c("predator_length", "prey_length"), list(
             predator_length = mfdb_interval("cod", c(20,30,40,50)),
             prey_length = mfdb_interval("cap", c(1,1.3,3,5)),
             prey_species = 'CAP'))

     gadget_dir_write(gd, gadget_likelihood_component(
         "stomachcontent",
         name = "cod-stomachs",
         prey_labels = c("codimm", "codmat", "codother"),
         prey_digestion_coefficients = 3:1,
         predator_names = c("cuthbert", "dibble"),
         data = res[[1]]))

     ok(cmp_file(gd, "likelihood",
         ver_string,
         "; ",
         "[component]",
         "name\tcod-stomachs",
         "weight\t0",
         "type\tstomachcontent",
         "function\tscsimple",
         "datafile\tData/stomachcontent.cod-stomachs.scsimple",
         "epsilon\t10",
         "areaaggfile\tAggfiles/stomachcontent.cod-stomachs.area.agg",
         "predatornames\tcuthbert\tdibble",
         "predatorlengths\t",
         "lenaggfile\tAggfiles/stomachcontent.cod-stomachs.len.agg",
         "preyaggfile\tAggfiles/stomachcontent.cod-stomachs.prey.agg",
         NULL), "likelihood file contains stomachcontent component")
    ok(cmp_file(gd, "Aggfiles/stomachcontent.cod-stomachs.prey.agg",
        ver_string,
        "; ",
        "cap1\t",
        "codimm\tcodmat\tcodother",
        "lengths\t1\t1.3",
        "digestioncoefficients\t3\t2\t1",
        "; ",
        "cap1.3\t",
        "codimm\tcodmat\tcodother",
        "lengths\t1.3\t3",
        "digestioncoefficients\t3\t2\t1",
        "; ",
        "cap3\t",
        "codimm\tcodmat\tcodother",
        "lengths\t3\t5",
        "digestioncoefficients\t3\t2\t1",
        NULL), "prey aggregation file")

     # Still works whe there's only 1 prey label
     gadget_dir_write(gd, gadget_likelihood_component(
         "stomachcontent",
         name = "cod-stomachs",
         prey_labels = c("cod"),
         prey_digestion_coefficients = 3:1,
         predator_names = c("cuthbert", "dibble"),
         data = res[[1]]))
    ok(cmp_file(gd, "Aggfiles/stomachcontent.cod-stomachs.prey.agg",
        ver_string,
        "; ",
        "cap1\t",
        "cod\t",
        "lengths\t1\t1.3",
        "digestioncoefficients\t3\t2\t1",
        "; ",
        "cap1.3\t",
        "cod\t",
        "lengths\t1.3\t3",
        "digestioncoefficients\t3\t2\t1",
        "; ",
        "cap3\t",
        "cod\t",
        "lengths\t3\t5",
        "digestioncoefficients\t3\t2\t1",
        NULL), "prey aggregation file (only one label)")
})

ok_group("Predator/Prey mismatch", {
    ok(cmp_error(mfdb_import_stomach(mdb,
        data_source = "cod2000",
        predator_data = shuffle_df(table_string("
stomach_name	year	month	areacell	species	length	weight
AA		2000	1	45G02		COD	21	210
BB		2000	1	45G02		COD	34	220
CC		2000	1	45G02		COD	34	230
        ")),
        prey_data = shuffle_df(table_string("
stomach_name	species	digestion_stage	length	weight	count
AA		CAP		1			1	10	5
AA		CAP		1			4	40	1
BB		CAP		1			1	10	5
XX		CLL		2			3.5	9.5	3
BB		CAP		4			1	10	5
YY		CLL		2			3.5	9.5	3
        "))), "stomachs.*XX.*YY"), "Complained that XX and YY are unknown stomachs")

     ok(cmp_table(
         mfdb_stomach_presenceratio(mdb, c("predator_weight"), list(
             predator_weight = mfdb_interval("w", c(200,300,400,500)),
             prey_species = 'CAP')),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             predator_weight = c('w200', 'w300'),
             ratio = c(
                 # CAP in A, B out of A, B, C
                 2 / 3,
                 # CAP in D, E out of D, E
                 2 / 2,
                 NULL),
             stringsAsFactors = FALSE)), "Old data is still available in table")

    # Replace data, should modify the output of queries
    mfdb_import_stomach(mdb,
        data_source = "cod2000",
        predator_data = shuffle_df(table_string("
stomach_name	year	month	areacell	species	length	weight
AA		2000	1	45G02		COD	21	210
BB		2000	1	45G02		COD	34	220
CC		2000	1	45G02		COD	34	230
        ")),
        prey_data = shuffle_df(table_string("
stomach_name	species	digestion_stage	length	weight	count
AA		CAP		1			1	10	5
        ")))
     ok(cmp_table(
         mfdb_stomach_presenceratio(mdb, c("predator_weight"), list(
             predator_weight = mfdb_interval("w", c(200,300,400,500)),
             prey_species = 'CAP')),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             predator_weight = c('w200'),
             ratio = c(
                 1 / 3,
                 NULL),
             stringsAsFactors = FALSE)), "Old data was replaced")

    # Remove data, shouldn't be able to find it again
    mfdb_import_stomach(mdb,
        data_source = "cod2000",
        predator_data = data.frame(),
        prey_data = data.frame())
     ok(cmp(
         unattr(mfdb_stomach_presenceratio(mdb, c("predator_weight"), list(
             predator_weight = mfdb_interval("w", c(200,300,400,500)),
             prey_species = 'CAP'))[['0.0.0.0']]),
         data.frame()), "cod2000 data removed")
})
