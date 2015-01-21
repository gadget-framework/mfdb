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
B		2000	1	45G01		COD	62	420
C		2000	1	45G01		COD	33	430
D		2000	1	45G01		COD	34	240
        ")),
        prey_data = shuffle_df(table_string("
stomach_name	species_id	digestion_stage_id	length	weight	count
A		CAP		1			1	10	5
A		CLL		5			4	40	1
B		CAP		4			1	10	5
B		CAP		5			1	10	8
C		CLL		2			3.5	9.5	3
D		CAP		1			1.4	10	1
D		CLL		5			4	40	1

        ")))

     # Find out the ratio of capelin in each stomach
     ok(cmp_table(
         mfdb_stomach_ratio(mdb, c("predator_weight", "digestion_stage"), list(
             predator_weight = mfdb_interval("w", c(200,300,400,500)),
             digestion_stage = mfdb_unaggregated(),
             prey_species = 'CAP')),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             predator_weight = c('w200', 'w400', 'w400'),
             digestion_stage = c(1,4,5),
             ratio = c(
                 # stage-1 CAP in A + D / Everything in A + D
                 (5 + 1) / (sum(5, 1) + sum(1, 1)),
                 # stage-4 CAP in B / Everything in B + C
                 (5) / (sum(5, 8)),
                 # stage-5 CAP in B / Everything in B + C
                 (8) / (sum(5, 8)),
                 NULL),
             stringsAsFactors = FALSE)), "Aggregated & got ratio")
})

