# To run this script, first install data sources:-
#  Debian / Ubuntu: apt-get install php5-cli php5-curl
# install.packages('DATRAS', repos = 'http://www.rforge.net/', type = 'source')
library(DATRAS)
library(mfdb)
library(plyr)

# Download data and read in available data for cod
if (!file.exists('example-datras-data/NS-IBTS_1995.zip')) {
    dir.create('example-datras-data', recursive = TRUE, showWarnings = FALSE)
    oldwd <- getwd()
    setwd('example-datras-data')
    downloadExchange("NS-IBTS",1995:2005)
    setwd(oldwd)
}
d <- readExchangeDir(path = 'example-datras-data')
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
mfdb_import_survey(mdb,
                   data_source = 'example-datras-codL',
                   sampling_type = 'RES',
                   tmp)

# Import cod age data as a separate survey
tmp <- codA[c('Year','Quarter','areacell','Species','Sex','Age','LngtCm')]
names(tmp) <- c('year','month','areacell','species','sex','age','length')
tmp <- subset(tmp,!is.na(age))
mfdb_import_survey(mdb,
                   data_source = 'example-datras-codA',
                   sampling_type = 'RES',
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
