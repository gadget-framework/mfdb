# Prototype EwE support, see bottom of file for usage
library(mfdb)
library(Matrix)

# Helper function: Turn a string into a data.frame
string_to_data.frame <- function (str) {
    read.table(
        textConnection(str),
        blank.lines.skip = TRUE,
        header = TRUE,
        stringsAsFactors = FALSE)
}

# Empty database and reconnect
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb('example_ewe', destroy_schema = TRUE)
mdb <- mfdb('example_ewe')

# Set-up areas, and combine them into 1 100km^2 division
all_areas <- c('45G01', '45G02', '45G03', '45G04', '45G05',
               '45G06', '45G07', '45G08', '45G09', '45G10')
mfdb_import_area(mdb, data.frame(name = all_areas, size = 10))
mfdb_import_division(mdb, list(all = all_areas))

# Create some vessels
mfdb_import_vessel_taxonomy(mdb, data.frame(
    name = c('vA', 'vB', 'vC'),
    full_name = c('Alice', 'Bertie', 'Claire'),
    vessel_type = c('1.RSH', '1.COM', '1.COM'),
    length = c(15, 18, 20),
    power = c(50, 100, 150),
    tonnage = c(900, 800, 700),
    stringsAsFactors = FALSE
))

# Vessel A's data for 2000
mfdb_import_survey(mdb, data_source = "2000.vA.COD", string_to_data.frame("
year month areacell species vessel length age weight count
2000   1    45G01     COD     vA     21    5   365     72 
2000   1    45G01     COD     vA     88    7   255     57 
2000   1    45G01     COD     vA     41    3   196     79 
2000   1    45G01     COD     vA     11    2   330     73 
2000   1    45G01     COD     vA     90    3   440     90 
2000   1    45G01     COD     vA     84    3   468     84 
2000   1    45G01     COD     vA     89    7   388     97 
2000   1    45G01     COD     vA     84    2   150     63 
2000   1    45G01     COD     vA     93    4   418     67 
2000   1    45G01     COD     vA     37    7   202     66 
2000   1    45G01     COD     vA     48    7   134     84 
2000   1    45G01     COD     vA     81    1   407     77 
"))
mfdb_import_survey(mdb, data_source = "2000.vA.HAD", string_to_data.frame("
year month areacell species vessel length age weight count
2000   1    45G01     HAD     vA     9     3    95     90 
2000   1    45G01     HAD     vA     8     2    71     86 
2000   1    45G01     HAD     vA     10    3    80     79 
2000   1    45G01     HAD     vA     5     3    94     50 
2000   1    45G01     HAD     vA     8     2    93     66 
2000   1    45G01     HAD     vA     8     3    95     63 
2000   1    45G01     HAD     vA     5     2    92     53 
2000   1    45G01     HAD     vA     6     2    98     63 
2000   1    45G01     HAD     vA     9     3    86     98 
2000   1    45G01     HAD     vA     6     1    81     76 
2000   1    45G01     HAD     vA     8     3    71     67 
2000   1    45G01     HAD     vA     8     1    82     88 
"))

# Vessel B's data for 2000
mfdb_import_survey(mdb, data_source = "2000.vB.COD", string_to_data.frame("
year month areacell species vessel length age weight count
2000   1    45G01     COD     vB     98    6   375     55 
2000   1    45G01     COD     vB     60    5   429     68 
2000   1    45G01     COD     vB     89    7   399     74 
2000   1    45G01     COD     vB     99    1   429     77 
2000   1    45G01     COD     vB     73    6   202     98 
2000   1    45G01     COD     vB     89    2   115     70 
2000   1    45G01     COD     vB     79    2   250     85 
2000   1    45G01     COD     vB     85    5   170     88 
2000   1    45G01     COD     vB     86    4   174     70 
2000   1    45G01     COD     vB     52    1   433     94 
2000   1    45G01     COD     vB     90    1   455     93 
2000   1    45G01     COD     vB     58    3   251     70 
"))
mfdb_import_survey(mdb, data_source = "2000.vB.HAD", string_to_data.frame("
year month areacell species vessel length age weight count
2000   1    45G01     HAD     vB     6     3    88     96 
2000   1    45G01     HAD     vB     6     2    72     91 
2000   1    45G01     HAD     vB     9     2    76     51 
2000   1    45G01     HAD     vB     5     1    74     92 
2000   1    45G01     HAD     vB     5     3    71     80 
2000   1    45G01     HAD     vB     10    1    95     99 
2000   1    45G01     HAD     vB     10    1    84     67 
2000   1    45G01     HAD     vB     5     2    97     90 
2000   1    45G01     HAD     vB     6     3    76     53 
2000   1    45G01     HAD     vB     7     3    80     58 
2000   1    45G01     HAD     vB     10    2    98     59 
2000   1    45G01     HAD     vB     8     1    86     74 
"))

# Vessel vC's data for 2000
mfdb_import_survey(mdb, data_source = "2000.vC.COD", string_to_data.frame("
year month areacell species vessel length age weight count
2000   1    45G01     COD     vC     84    1   342     60 
2000   1    45G01     COD     vC     64    4   137     75 
2000   1    45G01     COD     vC     87    2   225     68 
2000   1    45G01     COD     vC     81    5   193     78 
2000   1    45G01     COD     vC     52    7   254     65 
2000   1    45G01     COD     vC     61    1   248     91 
2000   1    45G01     COD     vC     67    4   175     56 
2000   1    45G01     COD     vC     68    5   467     55 
2000   1    45G01     COD     vC     89    2   195     84 
2000   1    45G01     COD     vC     56    7   123     86 
2000   1    45G01     COD     vC     99    7   266     84 
2000   1    45G01     COD     vC     89    2   253     86 
"))
mfdb_import_survey(mdb, data_source = "2000.C.HAD", string_to_data.frame("
year month areacell species vessel length age weight count
2000   1    45G01     HAD     vC     8     1    88     56 
2000   1    45G01     HAD     vC     6     2    78     68 
2000   1    45G01     HAD     vC     7     3    79     56 
2000   1    45G01     HAD     vC     8     2    70     99 
2000   1    45G01     HAD     vC     10    1    93     55 
2000   1    45G01     HAD     vC     6     3    98     90 
2000   1    45G01     HAD     vC     10    1    91     66 
2000   1    45G01     HAD     vC     7     3    82     70 
2000   1    45G01     HAD     vC     8     1    82     73 
2000   1    45G01     HAD     vC     10    2    73     74 
2000   1    45G01     HAD     vC     6     3    78     81 
2000   1    45G01     HAD     vC     10    3    82     99 
"))

# Stomach data
mfdb_import_stomach(mdb, data_source = "2000.stomach.COD", predator_data = string_to_data.frame("
stomach_name year month areacell species length age weight
     A       2000   1    45G01     COD    100    7   396  
     B       2000   1    45G01     COD     52    2   379  
     C       2000   1    45G01     COD     90    5   314  
     D       2000   6    45G01     COD     78    5   435  
     E       2000   6    45G01     COD     74    4   175  
     F       2000   6    45G01     COD     85    2   174  
"), prey_data = string_to_data.frame("
stomach_name species digestion_stage weight
     A         CAP          4          30  
     A         CLL          5          48  
     B         CAP          5          33  
     B         CLL          2          24  
     C         CAP          3          12  
     C         CLL          4          24  
     D         CAP          1          27  
     D         CLL          2          38  
     E         CAP          2          48  
     E         CLL          3          24  
     F         CAP          4          37  
     F         CLL          1          19  
"))

# Configure functional groups
start_year <- mfdb_group("2000" = 2000)
grouping_area <- list(area = NULL)  # We don't care, give us the whole area
grouping_cod_nogroup <- list(
    species = 'COD',
    age = NULL)
grouping_cod <- list(
    predator_species = 'COD',
    species = 'COD',
    age = mfdb_group(juv=1:4, adult=5:10))
grouping_had <- list(
    predator_species = 'HAD',
    species = 'HAD',
    age = mfdb_group(juv=1:2, adult=3:5))
grouping_vessel = list(
    vessel = mfdb_unaggregated())
grouping_prey = list(
    prey_species = mfdb_unaggregated())

# Query data and group together
survey_data <- mfdb_concatenate_results(
    mfdb_sample_totalweight(mdb, c('species', 'age'), c(grouping_area, grouping_cod_nogroup))[[1]],
    mfdb_sample_totalweight(mdb, c('species', 'age'), c(grouping_area, grouping_cod))[[1]],
    mfdb_sample_totalweight(mdb, c('species', 'age'), c(grouping_area, grouping_had))[[1]])
catch_data <- mfdb_concatenate_results(
    mfdb_sample_totalweight(mdb, c('species', 'age', 'vessel'), c(grouping_area, grouping_cod, grouping_vessel))[[1]],
    mfdb_sample_totalweight(mdb, c('species', 'age', 'vessel'), c(grouping_area, grouping_had, grouping_vessel))[[1]])
consumption_data <- mfdb_concatenate_results(
    mfdb_stomach_preyweightratio(mdb, c('predator_species', 'age', 'prey_species'), c(grouping_area, grouping_cod, grouping_prey))[[1]],
    mfdb_stomach_preyweightratio(mdb, c('predator_species', 'age', 'prey_species'), c(grouping_area, grouping_had, grouping_prey))[[1]])
area_data <- mfdb_area_size(mdb, grouping_area)[[1]]

# Generate CSVs and print out each
model_files <- list(
    stanza_group = ewe_stanza_group(survey_data),
    stanzas = ewe_stanzas(survey_data),
    model = ewe_model(area_data, survey_data, catch_data),
    diet = ewe_diet(consumption_data),
    pedigree = ewe_pedigree(survey_data, catch_data))

for (x in names(model_files)) {
    cat("== ", x, " ==============\n")
    print(model_files[[x]])
}

# Example output from script:-
# ==  stanzas  ==============
#              StGroupNum Stanza GroupNum Group First Last  Z Leading
# all_cod1              1      1        4  cod0     0    1 NA      NA
# all_cod2              1      2        5  cod2     2    3 NA      NA
# all_cod3              1      3        6  cod4     4    5 NA      NA
# all_cod4              1      4        7  cod6     6    7 NA      NA
# all_cod5              1      5        8  cod8     8    9 NA      NA
# all_haddock1          2      1        9 had10    10   19 NA      NA
# all_haddock2          2      2       10 had20    20   29 NA      NA
# all_haddock3          2      3       11 had30    30   39 NA      NA
# ==  stanza_group  ==============
#             StGroupNum Stanza_Group nstanzas VBGF_Ksp  VBGF_d Wmat RecPower
# all_cod              1      all_cod        5       NA 0.66667   NA        1
# all_haddock          2  all_haddock        3       NA 0.66667   NA        1
# ==  pedigree  ==============
#              Group B PB QB Diet
# all_cod1      cod0 1  1  1    1
# all_cod2      cod2 1  1  1    1
# all_cod3      cod4 1  1  1    1
# all_cod4      cod6 1  1  1    1
# all_cod5      cod8 1  1  1    1
# all_haddock1 had10 1  1  1    1
# all_haddock2 had20 1  1  1    1
# all_haddock3 had30 1  1  1    1
# ==  model  ==============
#              Group Type Biomass PB QB EE ProdCons Unassim DetInput Detritus Discards
# all_cod1      cod0    0      NA NA NA NA       NA      NA       NA       NA       NA
# all_cod2      cod2    0      NA NA NA NA       NA      NA       NA       NA       NA
# all_cod3      cod4    0      NA NA NA NA       NA      NA       NA       NA       NA
# all_cod4      cod6    0      NA NA NA NA       NA      NA       NA       NA       NA
# all_cod5      cod8    0      NA NA NA NA       NA      NA       NA       NA       NA
# all_haddock1 had10    0      NA NA NA NA       NA      NA       NA       NA       NA
# all_haddock2 had20    0      NA NA NA NA       NA      NA       NA       NA       NA
# all_haddock3 had30    0      NA NA NA NA       NA      NA       NA       NA       NA
# ==  diet  ==============
# $dimnames
# $dimnames[[1]]
#     all_cod1     all_cod2     all_cod3     all_cod4     all_cod5 all_haddock1 all_haddock2 all_haddock3 
#       "cod0"       "cod2"       "cod4"       "cod6"       "cod8"      "had10"      "had20"      "had30" 
# 
# $dimnames[[2]]
#     all_cod1     all_cod2     all_cod3     all_cod4     all_cod5 all_haddock1 all_haddock2 all_haddock3 
#       "cod0"       "cod2"       "cod4"       "cod6"       "cod8"      "had10"      "had20"      "had30" 
