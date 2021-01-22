# Excerise storing / grouping by population
#
# ok and ok_group are there so we can run this code and verify it is correct,
# and not needed every-day use.
library(unittest)
library(mfdb)

table_string <- function (x) read.table(
    text = x,
    blank.lines.skip = TRUE,
    header = TRUE,
    stringsAsFactors = FALSE)
unattr <- function (obj) {
    attributes(obj) <- attributes(obj)[c('names', 'row.names', 'class')]
    obj
}

# Empty database & rebuild
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('inttest-tripdata', db_params = list(dbname = "mf_inttest"), destroy_schema = TRUE)
mdb <- mfdb('inttest-tripdata', db_params = list(dbname = "mf_inttest"))

# logging::setLevel('FINEST')

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(
    name = c('45G01', '45G02', '45G03'),
    division = c('divA', 'divA', 'divB'),
    size = c(10,200,400)))

# Populations are arbitrary groupings for stocks that can't be derived from other data
# NB: They can inherit from one another using t_group
mfdb_import_population_taxonomy(mdb, table_string('
name 	description 				t_group
ns	"Northern Shrimp"			ns
ns_s	"Northern Shrimp in Skjalfandi"		ns
ns_a	"Northern Shrimp in Arnarfjordur"	ns
ns_i	"Northern Shrimp in Isafjardardjup"	ns
'))

# Import survey data relating to populations
mfdb_import_survey(mdb, data_source = "x",
table_string("
year    month   areacell   species population length  count
2019    1       45G01      PRA     ns_s    	10      285
2019    1       45G01      PRA     ns_s    	20      273

2019    1       45G01      PRA     ns_a    	10      299
2019    1       45G01      PRA     ns_a    	20      252

2019    1       45G01      PRA     ns_i    	10      193
2019    1       45G01      PRA     ns_i    	20      322
"))

ok(ut_cmp_equal(
    unattr(mfdb_sample_count(mdb, c('population', 'length'), list(
        population = mfdb_group(ns_s = 'ns_s', ns = 'ns'),
        length = mfdb_unaggregated()))[[1]]),
    table_string('
year step area population length number
all  all  all         ns     10    777
all  all  all         ns     20    847
all  all  all       ns_s     10    285
all  all  all       ns_s     20    273
    ')), "Can get aggregate ns group as well as ns_s")
