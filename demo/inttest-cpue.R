# Examples of generating CPUE
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
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('Test', db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb('Test', db_params = db_params, save_temp_tables = TRUE) # TODO:

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(id = c(1,2,3), name = c('45G01', '45G02', '45G03'), size = c(10,200,400)))
mfdb_import_division(mdb, list(divA = c('45G01', '45G02'), divB = c('45G03')))

# Create vessels we'll use
mfdb_import_vessel_taxonomy(mdb, data.frame(
    name = c('vA', 'vB', 'vC', 'vD'),
    full_name = c('Alfred', 'Bertie', 'Claire', 'Daisy'),
    vessel_type = c('1.RSH', '1.COM', '1.COM', '1.FRZ'),
    length = c(15, 18, 20, 24),
    power = c(50, 100, 150, 900),
    tonnage = c(900, 800, 700, 600),
    stringsAsFactors = FALSE
))

# Tow details
mfdb_import_tow_taxonomy(mdb, table_string("
name latitude longitude  depth length
vA.1     64.1    -23.15  98.82     10
vA.2     63.4    -20.18  44.90     10
vA.3     64.9    -13.55 140.91     20
vA.4     66.2    -18.55 122.61     20
vA.5     64.9    -13.55  44.90     10
vA.6     66.2    -18.55 140.91     20
vB.1     64.1    -23.15  98.82     20
vB.2     63.4    -20.18  44.90     20
vB.3     64.9    -13.55 140.91     30
vB.4     66.2    -18.55 122.61     30
vB.5     63.4    -20.18  44.90     20
vB.6     64.9    -13.55 140.91     30
"))

# Survey
mfdb_import_survey(mdb, data_source = "logbook-vA", table_string("
tow year month areacell species vessel gear weight_total
vA.1 2000   1    45G01     COD     vA    GN      420     
vA.2 2000   6    45G01     COD     vA    GN      128     
vA.3 2001   1    45G01     COD     vA    GN      425     
vA.4 2001   6    45G01     COD     vA    GN      152     
vA.5 2002   1    45G01     COD     vA    GN      320     
vA.6 2002   6    45G01     COD     vA    GN      152     
vA.1 2000   1    45G02     COD     vA    GN      431
vA.2 2000   6    45G02     COD     vA    GN      492
vA.3 2001   1    45G02     COD     vA    GN      177
vA.4 2001   6    45G02     COD     vA    GN      162
vA.5 2002   1    45G02     COD     vA    GN      256
vA.6 2002   6    45G02     COD     vA    GN      366
"))
mfdb_import_survey(mdb, data_source = "logbook-vB", table_string("
tow year month areacell species vessel gear weight_total
vB.1 2000   1    45G01     COD     vB    GN      208     
vB.2 2000   6    45G01     COD     vB    GN      465     
vB.3 2001   1    45G01     COD     vB    GN      215     
vB.4 2001   6    45G01     COD     vB    GIL     458     
vB.5 2002   1    45G01     COD     vB    GIL     343     
vB.6 2002   6    45G01     COD     vB    GIL     448
vB.1 2000   1    45G02     COD     vB    GN      479
vB.2 2000   6    45G02     COD     vB    GN      224
vB.3 2001   1    45G02     COD     vB    GN      480
vB.4 2001   6    45G02     COD     vB    GN      190
vB.5 2002   1    45G02     COD     vB    GN      110
vB.6 2002   6    45G02     COD     vB    GN      152     
"))

# Fetch the entire dataset to generate model from
all_data <- mfdb_sample_rawdata(mdb, c('gear', 'vessel', 'tow_length'), list(
    year = mfdb_unaggregated(),
    step = mfdb_unaggregated(),
    area = mfdb_unaggregated(),
    gear = mfdb_unaggregated(),
    vessel = mfdb_unaggregated(),
    tow_length = mfdb_unaggregated()
))[[1]]

# Generate CPUE
all_data$cpue <- apply(all_data, 1, function (row) {
    total_weight <- all_data$weight
    effort <- all_data$tow_length
    year <- ifelse(all_data$year == row[['year']], 1, 0)
    gear <- ifelse(all_data$gear == row[['gear']], 1, 0)
    vessel <- ifelse(all_data$vessel == row[['vessel']], 1, 0)

    mod <- lm(log(total_weight) ~ log(effort) + year + gear + vessel)
    return(mod$coefficients[['year']])
})
all_data
