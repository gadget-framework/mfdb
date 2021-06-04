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
mfdb(gsub("inttest", "inttest-stomach", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb(gsub("inttest", "inttest-stomach", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, save_temp_tables = FALSE)

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(
    name = c('45G01', '45G02', '45G03'),
    division = c('divA', 'divA', 'divB'),
    size = c(10,200,400)))

# NB: SQLite won't fail, columns are dynamically typed
if (!mfdb:::mfdb_is_duckdb(mdb) && !mfdb:::mfdb_is_sqlite(mdb)) ok(ut_cmp_error(mfdb_import_stomach(
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

predator_data <- expand.grid(
    year = 2000:3000,
    month = 1:12,
    areacell = '45G01',
    species = 'COD',
    length = c(10, 20, 30),
    stringsAsFactors = TRUE)
predator_data$stomach_name <- paste0('s-', seq_len(nrow(predator_data)))
predator_data$weight <- runif(nrow(predator_data), 100, 300)
prey_data <- expand.grid(
    stomach_name = predator_data$stomach_name,
    species = c('CAP', 'CLL'),
    digestion_stage = c(1,2),
    length = c(1,3,5),
    stringsAsFactors = TRUE)
prey_data$weight <- runif(nrow(prey_data), 1, 20)
prey_data$count <- runif(nrow(prey_data), 1, 20)
mfdb_import_stomach(mdb, predator_data, prey_data)

agg_data <- mfdb_stomach_preymeanweight(mdb, c('predator_species'), list())
ok(ut_cmp_equal(
    agg_data[[1]]$predator_count,
    nrow(predator_data)), "Total aggregate matches predator_data")
ok(ut_cmp_equal(
    agg_data[[1]]$mean_weight,
    sum(prey_data$weight * prey_data$count) / nrow(predator_data),
    tolerance = 1e-3), "Mean weight matches predator_data/prey_data")

mfdb_disconnect(mdb)
