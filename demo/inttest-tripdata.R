# Excerise storing / grouping by tow metadata
#
# ok and ok_group are there so we can run this code and verify it is correct,
# and not needed every-day use.
#
# NB: if you want to run this, you must be using mfdb-workspace, also be warned
# any data stored in your database will be destroyed.
library(unittest)
library(mfdb)
source('tests/utils/helpers.R')
source('tests/utils/inttest-helpers.R')
# logging::setLevel('FINEST')

# Empty database & rebuild
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('inttest-tripdata', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('inttest-tripdata', db_params = db_params)

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(
    name = c('45G01', '45G02', '45G03'),
    division = c('divA', 'divA', 'divB'),
    size = c(10,200,400)))

# Set up some ports
# TODO: Pre-populate? Would probably need a BIGINT for everything tho'
mfdb_import_port_taxonomy(mdb, table_string('
name 	description 	institute 	latitude longitude
ISHFF 	Hafnarfjordur 	ISL 		64.04	21.57
NOHVJ 	Hvalfjordur 	ISL 		64.21	21.45
ISKEF 	Keflavik 	ISL 		64.00	22.33
ISREK 	Reykjavik 	ISL 		64.09	21.55
ISSEJ 	Seydisfjordur 	ISL 		65.15	13.55
ISKJF 	Skerjafjordur 	ISL 		64.07	21.54
'))

# Set up some trips
mfdb_import_trip_taxonomy(mdb, table_string('
name 	start_date	end_date	crew	oil_consumption	start_port	end_port
T1	2019-01-21	2019-02-11	4	3000		ISKEF		ISREK
T2	2019-01-24	2019-02-14	5	9000		ISKEF		ISSEJ
T3	2019-04-21	2019-05-11	4	3000		ISREK		ISKEF
T4	2019-04-24	2019-05-14	5	9000		ISSEJ		ISKEF
'))

# Set up the tows we use in this example
mfdb_import_tow_taxonomy(mdb, table_string('
name latitude longitude  depth length
 T1a     64.1    -23.15  98.82     10
 T1b     64.1    -23.15  98.82     10
 T2a     64.1    -23.15  98.82     10
 T2b     64.1    -23.15  98.82     10
 T3a     64.1    -23.15  98.82     10
 T3b     64.1    -23.15  98.82     10
 T4a     64.1    -23.15  98.82     10
 T4b     64.1    -23.15  98.82     10
'))

mfdb_import_vessel_taxonomy(mdb, table_string('
name full_name
V1	"Vessel 1"
V2	"Vessel 2"
'))

# Import survey data relating to trips
mfdb_import_survey(mdb, data_source = "cod2000",
table_string("
year    month   areacell   species vessel trip tow  length  count
2019    1       45G01      COD     V1     T1   T1a    10      358
2019    1       45G01      COD     V1     T1   T1a    20      320
2019    1       45G01      COD     V1     T1   T1a    30      162
2019    1       45G02      COD     V1     T1   T1b    10      240
2019    1       45G02      COD     V1     T1   T1b    20      278
2019    1       45G02      COD     V1     T1   T1b    30      122

2019    2       45G01      COD     V2     T2   T2a    10      255
2019    2       45G01      COD     V2     T2   T2a    20      138
2019    2       45G01      COD     V2     T2   T2a    30      168
2019    2       45G02      COD     V2     T2   T2b    10      349
2019    2       45G02      COD     V2     T2   T2b    20      106
2019    2       45G02      COD     V2     T2   T2b    30      262

2019    4       45G01      COD     V1     T3   T3a    10      395
2019    4       45G01      COD     V1     T3   T3a    20      214
2019    4       45G01      COD     V1     T3   T3a    30      195
2019    4       45G02      COD     V1     T3   T3b    10      396
2019    4       45G02      COD     V1     T3   T3b    20      325
2019    4       45G02      COD     V1     T3   T3b    30      269

2019    5       45G01      COD     V2     T4   T4a    10      239
2019    5       45G01      COD     V2     T4   T4a    20      309
2019    5       45G01      COD     V2     T4   T4a    30      162
2019    5       45G02      COD     V2     T4   T4b    10      342
2019    5       45G02      COD     V2     T4   T4b    20      175
2019    5       45G02      COD     V2     T4   T4b    30      299
"))

ok(ut_cmp_equal(
    unattr(mfdb_sample_count(mdb, c('trip_start_date', 'length'), list(
        trip_start_date = '2019-01-21',
        length = mfdb_unaggregated()))[[1]]),
    table_string('
year step area trip_start_date length number
 all  all  all      2019-01-21     10    598
 all  all  all      2019-01-21     20    598
 all  all  all      2019-01-21     30    284
    ')), "Summed T1a and T1b, since they have a matching start date")

ok(ut_cmp_equal(
    unattr(mfdb_sample_count(mdb, c('trip_start_port', 'length'), list(
        trip_start_port = mfdb_group(KEF = 'ISKEF', other = c('ISREK', 'ISSEJ')),
        length = mfdb_unaggregated()))[[1]]),
    table_string('
year step area trip_start_port length number
 all  all  all             KEF     10   1202
 all  all  all             KEF     20    842
 all  all  all             KEF     30    714
 all  all  all           other     10   1372
 all  all  all           other     20   1023
 all  all  all           other     30    925
    ')), "Translated trip_start_port in and out of attributes")

