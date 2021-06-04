# Excerise storing / grouping by vessel information
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

# Empty database & rebuild
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb(gsub("inttest", "inttest-vessel", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb(gsub("inttest", "inttest-vessel", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, save_temp_tables = FALSE)

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(
    name = c('45G01', '45G02', '45G03'),
    division = c('divA', 'divA', 'divB'),
    size = c(10,200,400)))

ok_group("Vessel metadata example", {
    # Create a temporary gadget directory
    gd <- gadget_directory(tempfile())

    mfdb_import_vessel_owner_taxonomy(mdb, table_string('
name	full_name							t_group
ISL	"Iceland"							ISL
GBR	"Great Britain"							GBR
ITA	"Italy"								ITA
MFRI	"Marine and Freshwater Research Institute (Iceland)"		ISL
CEFAS	"Centre for Environment, Fisheries and Aquaculture Science"	GBR
CNR-IAMC	"Consiglio Nazionale delle Ricerche"			ITA
COMM	"Combined commercial fisheries"					COMM
COMM1	"Commercial fishery 1"						COMM
COMM2	"Commercial fishery 2"						COMM
    '))

    # Set up the vessels we use in this example
    mfdb_import_vessel_taxonomy(mdb, table_string('
name 	full_name		vessel_type	vessel_owner	length	power	tonnage
   A    "Arni Fridriksson"	1.RSH		MFRI		70	50	900
   B    "Bjarni Saemundsson"	1.RSH		MFRI		56	100	800
   C    "Cefas Endeavour"	1.RSH		CEFAS		65.50   150	700
   D    "Dallaporta"		1.RSH		CNR-IAMC	24	900	600
   S	"Commercial vessel 1a"	2.COM		COMM1		10	50	900
   T	"Commercial vessel 1b"	2.COM		COMM1		30	50	900
   U	"Commercial vessel 2a"	2.COM		COMM2		50	50	900
   V	"Commercial vessel 2b"	2.COM		COMM2		70	50	900
    '))

    # Import a survey for the data we are interested in
    mfdb_import_survey(mdb, data_source = "x",
        table_string('
year    month   areacell        species vessel  length  age     weight
2000    1       45G01           COD     A           21      2       210
2000    1       45G02           COD     A           34      3       220
2000    1       45G03           COD     A           34      3       230
2000    1       45G01           COD     B           62      1       320
2000    1       45G02           COD     B           53      1       330
2000    1       45G03           COD     B           54      2       430
2000    1       45G01           COD     C           28      2       210
2000    1       45G02           COD     C           34      3       220
2000    1       45G03           COD     C           24      3       230
2000    1       45G01           COD     D           12      1       320
2000    1       45G02           COD     D           44      1       330
2000    1       45G03           COD     D           14      2       430

2000    1       45G01           COD     S           47      2       383
2000    1       45G02           COD     S           68      3       114
2000    1       45G03           COD     S           15      3       101
2000    1       45G01           COD     T           92      1       251
2000    1       45G02           COD     T           85      1       289
2000    1       45G03           COD     T           64      2       322
2000    1       45G01           COD     U           28      2       140
2000    1       45G02           COD     U           87      3       170
2000    1       45G03           COD     U           63      3       197
2000    1       45G01           COD     V           61      1       160
2000    1       45G02           COD     V           45      1       201
2000    1       45G03           COD     V           32      2       313
        '))

    # Group the data by vessel length
    agg_data <- mfdb_sample_meanlength(mdb, c('vessel_type', 'vessel_length'), list(
        step = mfdb_timestep_yearly,
        vessel_type = mfdb_unaggregated(),
        vessel_length = mfdb_interval('veslen', c(0, 10, 20, 50, 100)),
        null = NULL))
    ok(ut_cmp_equal(unattr(agg_data[[1]]), table_string('
year step area vessel_type vessel_length number     mean
 all  "1"  all       1.RSH      veslen20      3 23.33333
 all  "1"  all       1.RSH      veslen50      9 38.22222
 all  "1"  all       2.COM      veslen10      3 43.33333
 all  "1"  all       2.COM      veslen20      3 80.33333
 all  "1"  all       2.COM      veslen50      6 52.66667
        ', colClasses = c(NA, "character", NA, NA, NA, "numeric", NA)), tolerance = 1e-7), "Grouped by vessel_type, vessel_length")

    # Group the data by vessel type and area
    agg_data <- mfdb_sample_meanlength(mdb, c('vessel_type'), list(
        step = mfdb_timestep_yearly,
        area = mfdb_group(x = 'divA', y = 'divB'),
        vessel_type = mfdb_group(RSH = '1.RSH', COM = '1.COM'),
        null = NULL))
    ok(cmp(unattr(agg_data[[1]]), table_string('
year step area vessel_type number mean
 all  "1"    x         RSH      8 36.0
 all  "1"    y         RSH      4 31.5
        ', colClasses = c(NA, "character", NA, NA, "numeric", NA))), "Grouped by vessel_type/area")

    # Show each vessel separately, with full name, power, tonnage
    agg_data <- mfdb_sample_meanlength(mdb, c('vessel', 'vessel_full_name', 'vessel_power', 'vessel_tonnage'), list(
        step = mfdb_timestep_yearly,
        vessel = mfdb_unaggregated(),
        vessel_full_name = mfdb_unaggregated(),
        vessel_power = mfdb_unaggregated(),
        vessel_tonnage = mfdb_unaggregated(),
        null = NULL))
    ok(cmp(unattr(agg_data[[1]]), table_string('
year step area vessel      vessel_full_name vessel_power vessel_tonnage number     mean
 all    1  all      A     "Arni Fridriksson"           50            900      3 29.66667
 all    1  all      B   "Bjarni Saemundsson"          100            800      3 56.33333
 all    1  all      C      "Cefas Endeavour"          150            700      3 28.66667
 all    1  all      D           "Dallaporta"          900            600      3 23.33333
 all    1  all      S "Commercial vessel 1a"           50            900      3 43.33333
 all    1  all      T "Commercial vessel 1b"           50            900      3 80.33333
 all    1  all      U "Commercial vessel 2a"           50            900      3 59.33333
 all    1  all      V "Commercial vessel 2b"           50            900      3 46.00000
        ', colClasses = c(NA, "character", NA, NA, NA, "numeric", "numeric", "numeric", NA))), "Dissaggregated vessels")

    # Can group vessels by owner, which aggregates multiple vessels together
    agg_data <- mfdb_sample_meanlength(mdb, c('vessel_owner'), list(
        step = mfdb_timestep_yearly,
        vessel_owner = mfdb_unaggregated(),
        null = NULL))
    ok(cmp(unattr(agg_data[[1]]), table_string('
year step area vessel_owner number     mean
 all    1  all        CEFAS      3 28.66667
 all    1  all     CNR-IAMC      3 23.33333
 all    1  all        COMM1      6 61.83333
 all    1  all        COMM2      6 52.66667
 all    1  all         MFRI      6 43.00000
        ', colClasses = c(NA, "character", NA, NA, "numeric", NA))), "Vessels grouped by owner")
})

mfdb_disconnect(mdb)
