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

# Download data and read in available data for cod
if (!file.exists('example-datras-data/NS-IBTS_1995.zip')) {
    dir.create('example-datras-data', recursive = TRUE, showWarnings = FALSE)
    oldwd <- getwd()
    setwd('example-datras-data')
    downloadExchange("NS-IBTS",1995:2005)
    setwd(oldwd)
}
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

# Create connection to MFDB database, as the Baltic case study
mdb <- mfdb('Baltic')

# Assume all squares are 5km^2
squares <- unique(c(codA$areacell,codL$areacell))
mfdb_import_area(mdb, data.frame(
    id = 1:length(squares),
    name = squares,
    size = c(5)))

# Create divisions and import those
sub.split <- llply(squares, function(x) x)                           
names(sub.split) <- squares
mfdb_import_division(mdb, sub.split)

# Hard-code temperature data (TODO: Find proper source)
mfdb_import_temperature(mdb, data.frame(
    year = 1996:2005,
    month = 1,
    areacell = squares[1],
    temperature = 7.4))

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

# Add any missing squares to known areacells
squares <- c(squares, levels(predators$ICES_Rectangle))
squares <- squares[!duplicated(squares)]
mfdb_import_area(mdb, data.frame(
    name = squares,
    size = c(5),
    stringsAsFactors = FALSE))

# Create divisions and import those
sub.split <- llply(squares, function(x) x)
names(sub.split) <- squares
mfdb_import_division(mdb, sub.split)

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
