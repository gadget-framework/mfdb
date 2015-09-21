# To run this script, first install data sources:-
#  Debian / Ubuntu: apt-get install php5-cli php5-curl
# install.packages('DATRAS', repos = 'http://www.rforge.net/', type = 'source')
library(DATRAS)
library(mfdb)
library(plyr)
library(XML)

# Fetch a stomach dataset (or read cached copy, if we already have it)
get_stomach_dataset <- function(outdir = 'example-datras-data', year = '', country = '', predator = '') {
    dir.create('example-datras-data', recursive = TRUE, showWarnings = FALSE)
    out_file <- file.path(outdir, paste(
        'stomach',
        utils::URLencode(as.character(year)),
        utils::URLencode(country),
        utils::URLencode(predator),
        'xml', sep = "."))

    # Fetch file if it's not already there, then read it
    if (!file.exists(out_file)) {
        url <- paste0(
            "http://ecosystemdata.ices.dk/stomachdata/StomachDataWebServices.asmx/getStomachDataset",
            "?year=", utils::URLencode(as.character(year)),
            "&country=", utils::URLencode(country),
            "&predator=", utils::URLencode(predator),
            "")
        utils::download.file(url, out_file)
    }
    cat("Reading", out_file, "\n")
    return (XML::xmlToDataFrame(out_file, homogeneous = TRUE, collectNames = FALSE, colClasses = c(
        File_Name = "character",
        Latitude = "numeric",
        Longitude = "numeric",
        Estimated_Lat_Lon = "character",
        Date_Time = "character",
        Year = "integer",
        Quarter = "integer",
        Month = "integer",
        Day = "integer",
        Time = "character",
        Station_Haul = "character",
        Sample_Number = "integer",
        ICES_StomachID = "character",
        Depth = "numeric",
        Temperature = "numeric",
        Country = "character",
        Ship = "character",
        ICES_Rectangle = "character",
        Sampling_Method = "character",
        Predator = "character",
        Predator_NODC_Code = "character",
        Predator_mean_Lengh = "numeric",
        Predator_mean_Weight = "numeric",
        Predator_mean_Age = "numeric",
        Predator_Lower_Length_Bound = "numeric",
        Predator_Upper_Length_Bound = "numeric",
        CPUE = "numeric",
        Number_Stomachs_With_Food = "numeric",
        Number_Stomachs_Regurgitated = "numeric",
        Number_Stomachs_With_Skeletal_Remains = "numeric",
        Number_Stomachs_Empty = "numeric",
        Number_Stomachs = "numeric",
        Digestion_Stage = "character",
        Prey_Species_Name = "character",
        Prey_Weight = "numeric",
        Prey_Lower_Length_Bound = "numeric",
        Prey_Upper_Length_Bound = "numeric",
        Prey_Number = "numeric",
        ICES_Internal_ID = "numeric")))
}

# Download data if not already available
if (!file.exists('example-datras-data/NS-IBTS_1995.zip')) {
    dir.create('example-datras-data', recursive = TRUE, showWarnings = FALSE)
    oldwd <- getwd()
    setwd('example-datras-data')
    downloadExchange("NS-IBTS",1995:2005)
    setwd(oldwd)
}

# Create connection to MFDB database, as the Baltic case study
mdb <- mfdb('Baltic')

# Populated ICES gridcells we are interested in
squares <- c(
  "31F1", "31F2",
  "32F1", "32F2", "32F3",
  "33F1", "33F2", "33F3", "33F4",
  "34F1", "34F2", "34F3", "34F4",
  "35F0", "35F1", "35F2", "35F3", "35F4", "35F5",
  "36F0", "36F1", "36F2", "36F3", "36F4", "36F5", "36F6", "36F7", "36F8",
  "37E9", "37F0", "37F1", "37F2", "37F3", "37F4", "37F5", "37F6", "37F7", "37F8",
  "38E8", "38E9", "38F0", "38F1", "38F2", "38F3", "38F4", "38F5", "38F6", "38F7", "38F8",
  "39E8", "39E9", "39F0", "39F1", "39F2", "39F3", "39F4", "39F5", "39F6", "39F7", "39F8",
  "40E8", "40E9", "40F0", "40F1", "40F2", "40F3", "40F4", "40F5", "40F6", "40F7", "40G2",
  "41E7", "41E8", "41E9", "41F0", "41F1", "41F2", "41F3", "41F4", "41F5", "41F6", "41F7", "41G0", "41G1", "41G2",
  "42E7", "42E8", "42E9", "42F0", "42F1", "42F2", "42F3", "42F4", "42F5", "42F6", "42F7", "42G0", "42G1", "42G2",
  "43E8", "43E9", "43F0", "43F1", "43F2", "43F3", "43F4", "43F5", "43F6", "43F7", "43F8", "43F9", "43G0", "43G1", "43G2",
  "44E6", "44E7", "44E8", "44E9", "44F0", "44F1", "44F2", "44F3", "44F4", "44F5", "44F6", "44F7", "44F8", "44F9", "44G0", "44G1",
  "45E6", "45E7", "45E8", "45E9", "45F0", "45F1", "45F2", "45F3", "45F4", "45F9", "45G0", "45G1",
  "46E6", "46E7", "46E8", "46E9", "46F0", "46F1", "46F2", "46F3", "46F4", "46G0", "46G1",
  "47E7", "47E8", "47E9", "47F0", "47F1", "47F2", "47F3",
  "48E6", "48E7", "48E8", "48E9", "48F0", "48F1", "48F2", "48F3", "48F4",
  "49E7", "49E8", "49E9", "49F0", "49F1", "49F2", "49F3", "49F4",
  "50E7", "50E8", "50E9", "50F0", "50F1", "50F2", "50F3", "51E8", "51E9", "51F0", "51F1", "51F2", "52E9", "52F0", "52F1",
  NULL)
mfdb_import_area(mdb, data.frame(
    id = 1:length(squares),
    name = squares,
    size = c(5)))  # NB: Assume all squares are 5km^2

# Make a single division for each area
mfdb_import_division(mdb, structure(as.list(squares), names = squares))

# Hard-code temperature data (TODO: Find proper source)
mfdb_import_temperature(mdb, data.frame(
    year = 1996:2005,
    month = 1,
    areacell = squares[1],
    temperature = 7.4))

# Read in data dump, split out cod data
ignore <- capture.output({
    d <- readExchangeDir(path = 'example-datras-data')
})
cod <- subset(d,Species == 'Gadus morhua')

# Retrieve length data
codL <- merge(cod[['HL']],cod[['HH']][c('haul.id','lat','lon')])
codL$areacell <- icesSquare(codL)
codL$Species <- 'COD'

# Retrieve age data
codA <- merge(cod[['CA']],cod[['HH']][c('haul.id','lat','lon')])
codA$areacell <- as.character(codA$AreaCode)
codA$Species <- 'COD'

# Set-up some sampling types
mfdb_import_sampling_type(mdb, data.frame(
    id = 1,
    name = 'RES',
    description = 'Research'))

# Import cod length data as a survey
tmp <- codL[c('Year','Quarter','areacell','Species','Sex','LngtCm','Count')]
names(tmp) <- c('year','month','areacell','species','sex','length','count')
tmp <- subset(tmp,!is.na(length))
tmp$count <- round(tmp$count)
tmp$sampling_type <- 'RES'
mfdb_import_survey(mdb,
                   data_source = 'example-datras-codL',
                   tmp)

# Import cod age data as a separate survey
tmp <- codA[c('Year','Quarter','areacell','Species','Sex','Age','LngtCm')]
names(tmp) <- c('year','month','areacell','species','sex','age','length')
tmp <- subset(tmp,!is.na(age))
tmp$sampling_type <- 'RES'
mfdb_import_survey(mdb,
                   data_source = 'example-datras-codA',
                   tmp)

# Create a gadget directory, define some defaults to use with our queries below
gd <- gadget_directory("example-datras-model")
defaults <- list(
    area = mfdb_group("101" = squares),
    timestep = mfdb_timestep_quarterly, # Group months to create 2 timesteps for each year
    year = 1996:2005)

# Write out areafile and update mainfile with areafile location
gadget_dir_write(gd,gadget_areafile(
    size = mfdb_area_size(mdb, defaults)[[1]],
    temperature = mfdb_temperature(mdb, defaults)[[1]]))

# Write a penalty component to the likelihood file
gadget_dir_write(gd, gadget_likelihood_component("penalty",
                                                 name = "bounds",
                                                 weight = "0.5",
                                                 data = data.frame(
                                                   switch = c("default"),
                                                   power = c(2),
                                                   stringsAsFactors = FALSE)))

# Query some age-length data and write it as a catchdistribution component
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    age = mfdb_group_numbered('age',1,2,3,4,5,6,7,8,9,10,11,12,13),
    sampling_type = 'RES',
    length = mfdb_interval("len", seq(0, 150, by = 2))),
    defaults))
gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "aldist.datras",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = c("datras"),
                                                 stocknames = 'cod'))
rm(aggdata)

# Query some length data and write a separate component (note different name)
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'RES',
    length = mfdb_interval("len", seq(0, 150, by = 2))),
    defaults))
gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.datras",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = c("datras"),
                                                 stocknames = 'cod'))
rm(aggdata)

# Fetch cod stomach data for 1991 and import it
stomachs <- get_stomach_dataset(year = 1991, predator = "Gadus Morhua")
predators <- stomachs[!duplicated(stomachs$ICES_StomachID), ]

# Work out a map from prey species we can uniquely identify to short names, map rest to NA
species_map <- mfdb_find_species(levels(stomachs$Prey_Species_Name))['name',]
species_map <- vapply(species_map, function (names) if(length(names) == 1) names else as.character(NA), "")

# Import this into DB
mfdb_import_stomach(mdb,
    data.frame(
        stomach_name = predators$ICES_StomachID,
        year = predators$Year,
        month = predators$Month,
        areacell = predators$ICES_Rectangle,
        species = 'COD',
        age = predators$Predator_mean_Age,
        # Map stomach state ro whichever of the 4 DATRAS variables is highest
        stomach_state = mfdb::stomach_state[,'name'][apply(predators[, c(
            'Number_Stomachs_Empty',
            'Number_Stomachs_With_Food',
            'Number_Stomachs_Regurgitated',
            'Number_Stomachs_With_Skeletal_Remains')], 1, which.max)],
        length = predators$Predator_mean_Lengh,
        weight = predators$Predator_mean_Weight,
        stringsAsFactors = TRUE),
    data.frame(
        stomach_name = stomachs$ICES_StomachID,
        species = revalue(stomachs$Prey_Species_Name, species_map),
        digestion_stage = as.numeric(stomachs$Digestion_Stage),
        length = stomachs$Prey_Number,  # NB: According to StomachData.pdf, this is Value[Lengh]
        weight = stomachs$Prey_Weight,
        stringsAsFactors = TRUE),
    data_source = paste('datras_stomach', 1991, 'COD', sep = "_"))
rm(stomachs)
rm(predators)

# Find out the ratio of pand bor in stomachs
res <- mfdb_stomach_presenceratio(mdb, c("predator_length", "prey_length"), list(
        predator_length = mfdb_step_interval("cod", 10, to = 200),
        prey_length = mfdb_step_interval("pra", 50, to = 100, open_ended = TRUE),
        prey_species = 'PRA'))

gadget_dir_write(gd, gadget_likelihood_component(
    "stomachcontent",
    prey_labels = c("praimm", "pramat"),
    prey_digestion_coefficients = 3:1,
    predator_names = c("cod"),
    data = res[[1]]))
