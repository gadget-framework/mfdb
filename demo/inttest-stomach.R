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

table_string <- function (str) {
    read.table(
        textConnection(str),
        blank.lines.skip = TRUE,
        header = TRUE,
        stringsAsFactors = FALSE)
}

cmp_table <- function(tbls, expected) {
    ok(cmp(length(tbls), 1), "result only returned one data.frame")
    cmp(tbls[[1]][names(tbls[[1]])], expected)
}

shuffle_df <- function(df) df[sample(nrow(df)),]

# Empty database & rebuild
mfdb('', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = TRUE)

ok_group("Stomach data", {
    # Set-up areas/divisions
    mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(10,200,400)))
    mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G03')))

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
stomach_name	species_id	digestion_stage_id	length	weight	count
A		CAP		1			1	10	5
A		CAP		1			4	40	1
B		CAP		1			1	10	5
B		CAP		4			1	10	5
B		CAP		5			1	10	8
B		CAP		5			1	10	5
C		CLL		2			3.5	9.5	3

D		CAP		1			1.4	10	1
D		CLL		5			4	40	1
E		CAP		1			1.4	10	1
        ")))

     # Find out the ratio of capelin in stomachs
     ok(cmp_table(
         mfdb_stomach_ratio(mdb, c("predator_weight"), list(
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
         mfdb_stomach_ratio(mdb, c("predator_weight", "digestion_stage"), list(
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
                 # undigested CAP in A out of A, B, C
                 2 / 3,
                 # undigested CAP in D, E out of D, E
                 2 / 2,
                 NULL),
             stringsAsFactors = FALSE)), "Aggregated & got ratio")
})
