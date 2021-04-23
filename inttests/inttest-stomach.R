library(unittest)
library(mfdb)
source('tests/utils/inttest-helpers.R')

# Convert a string into a data.frame
table_string <- function (text, ...) read.table(
    text = text,
    blank.lines.skip = TRUE,
    header = TRUE,
    stringsAsFactors = FALSE,
    ...)


# Empty database & rebuild
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('inttest-stomach', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('inttest-stomach', db_params = db_params, save_temp_tables = FALSE)

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(
    name = c('45G01', '45G02', '45G03'),
    division = c('divA', 'divA', 'divB'),
    size = c(10,200,400)))

# NB: SQLite won't fail, columns are dynamically typed
if (!mfdb:::mfdb_is_sqlite(mdb)) ok(ut_cmp_error(mfdb_import_stomach(
    mdb,
    predator_data = table_string("
stomach_name	year	month	areacell	species	length	weight
A		2000	1	45G01		COD	21	210
B		2000	1	45G01		COD	34	220
C		2000	1	45G01		COD	34	230
    "),
    prey_data = table_string("
stomach_name	species	digestion_stage	length	weight	count
A		CAP		1	1	10	parp
C		CLL		2	3.5	9.5	4
    ")), "parp"), "Invalid data reported (wasn't lost in transactions)")
