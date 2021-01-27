# Excerise storing additional measurements in samples
library(unittest)
library(mfdb)
source('tests/utils/helpers.R')
source('tests/utils/inttest-helpers.R')

# Empty database & rebuild
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('inttest-vessel', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('inttest-vessel', db_params = db_params, save_temp_tables = FALSE)

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(
    name = c('45G01', '45G02', '45G03'),
    division = c('divA', 'divA', 'divB'),
    size = c(10,200,400)))

# Import a survey for the data we are interested in
mfdb_import_survey(mdb, data_source = "x",
    table_string('
year    month   areacell        species length  age     weight	liver_weight	gonad_weight	stomach_weight
2000    1       45G01           COD     21      2       210	93		82		72
2000    1       45G02           COD     34      3       220	28		93		99
2000    1       45G03           COD     44      4       230	93		85		72
    '))

# The "measurements" parameter allows you to choose which weights are returned
agg_data <- mfdb_sample_totalweight(mdb, c('age'), list(
    age = mfdb_unaggregated(),
    null = NULL), measurements = c('overall', 'liver', 'gonad', 'stomach'))
ok(ut_cmp_equal(unattr(agg_data[[1]]), table_string('
year step area age total_weight total_liver_weight total_gonad_weight total_stomach_weight
 all  all  all   2          210                 93                 82                   72
 all  all  all   3          220                 28                 93                   99
 all  all  all   4          230                 93                 85                   72
    ')), "Got all additional weights back")

# Can turn off all but one, for example.
agg_data <- mfdb_sample_totalweight(mdb, c('age'), list(
    age = mfdb_unaggregated(),
    null = NULL), measurements = c('stomach'))
ok(ut_cmp_equal(unattr(agg_data[[1]]), table_string('
year step area age total_stomach_weight
 all  all  all   2                   72
 all  all  all   3                   99
 all  all  all   4                   72
    ')), "Can pick just one measurement")

# Extra measurements can be aggregated
agg_data <- mfdb_sample_meanweight(mdb, c('age'), list(
    null = NULL), measurements = c('liver', 'stomach'))
ok(ut_cmp_equal(unattr(agg_data[[1]]), table_string('
year step area age number mean_liver_weight mean_stomach_weight
 all  all  all all      3          71.33333                  81
    '), tolerance = 1e-7), "Aggregated liver, stomach data")

# ...and stddev got
agg_data <- mfdb_sample_meanweight_stddev(mdb, c('age'), list(
    null = NULL), measurements = c('overall', 'gonad'))
ok(ut_cmp_equal(unattr(agg_data[[1]]), table_string('
year step area age number mean mean_gonad_weight stddev stddev_gonad_weight
 all  all  all all      3  220          86.66667     10            5.686241
    '), tolerance = 1e-7), "Aggregated overall, gonad weights")
