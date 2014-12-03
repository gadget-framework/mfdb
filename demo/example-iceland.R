# To run this script, first install data sources: fjolst, geo
library(mfdb)
library(fjolst)
library(geo)
library(plyr)
library(data.table)

# Create connection to MFDB database, as the Icelandic case study
mdb <- mfdb('Iceland')

# Import area definitions
reitmapping <- read.table(
    system.file("demo-data", "reitmapping.tsv", package="mfdb"),
    header=TRUE,
    as.is=TRUE)
mfdb_import_area(mdb, data.frame(
    id = 1:nrow(reitmapping),
    name = reitmapping$GRIDCELL,
    size = c(5)))
mfdb_import_division(mdb, dlply(reitmapping, 'SUBDIVISION', function(x) x$GRIDCELL))
mfdb_import_temperature(mdb, data.frame(
    year = 2012,
    month = 1:12,
    areacell = reitmapping$GRIDCELL[1],
    temperature = 3))

# Set-up some sampling types
mfdb_import_sampling_type(mdb, data.frame(
    id = 1:2,
    name = c('SEA', 'IGFS'),
    description = c('Sea sampling', 'Icelandic ground fish survey')))

# Import length distribution from commercial catches
# 1 inspectors, 2 hafro, 8 on-board discard
comm.stations <- subset(stodvar, synaflokkur == 8)
comm.ldist <- subset(all.le, synis.id %in% comm.stations$synis.id & 
                       tegund == 1)

comm.ldist <- merge(comm.ldist, 
                    comm.stations[c('synis.id','ar','man','lat','lon')])
comm.ldist$areacell <- d2sr(comm.ldist$lat,-comm.ldist$lon)
names(comm.ldist) <- c('sample.id','species','length','count','sex',
                      'maturity','year','month','lat','lon','areacell')
comm.ldist$species <- 'COD' 
comm.ldist <- subset(comm.ldist, areacell %in% reitmapping$GRIDCELL) 
comm.ldist$sex <- c('M','F')[comm.ldist$sex]
comm.ldist$age <- 0
mfdb_import_survey(mdb,
                   data_source = 'example-iceland-comm.ldist',
                   sampling_type = 'SEA',
                   comm.ldist)
rm(comm.ldist)

# Import length distribution from autumn survey
stations <- subset(stodvar, synaflokkur == 30) ## autumn survey 35
ldist <- subset(all.le, synis.id %in% stations$synis.id & tegund == 1)
num.dist <- subset(all.nu, synis.id %in% stations$synis.id & tegund == 1)
ldist.aug <- Skala.med.toldum(ldist,num.dist, tegund==1)
ldist.aug <- merge(ldist.aug, stations[c('synis.id','ar','lat','lon')])
ldist.aug$areacell <- d2sr(ldist.aug$lat,-ldist.aug$lon)
names(ldist.aug) <- c('sample.id','species','length','num','sex',
                      'maturity','count','year','lat','lon','areacell')
ldist.aug$species <- 'COD' 
ldist.aug$month  <- 3
ldist.aug$count <- round(ldist.aug$count)

ldist.aug <- subset(ldist.aug,!is.na(areacell))
ldist.aug <- subset(ldist.aug,areacell %in% reitmapping$GRIDCELL) 
ldist.aug$sex <- c('M','F')[ldist.aug$sex]
ldist.aug$age <- 0
mfdb_import_survey(mdb,
                   data_source = 'example-iceland-ldist.aug',
                   sampling_type = 'IGFS', 
                   ldist.aug)
rm(ldist.aug)

# Create a gadget directory, define some defaults to use with our queries below
gd <- gadget_directory("example-iceland-model")
defaults <- list(
    area = mfdb_group("101" = reitmapping$SUBDIVISION),
    timestep = mfdb_timestep_quarterly,
    year = 1984:2012)

# Write out areafile and update mainfile with areafile location
gadget_dir_write(gd, gadget_areafile(
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

# Query length data to create some catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", seq(0, 150, by = 2))),
  defaults))
gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.igfs",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = c("igfs"),
                                                 stocknames = c("codimm", "codmat")))
rm(aggdata)


aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
  sampling_type = 'SEA',
  length = mfdb_interval("len", seq(0, 150, by = 2))),
  defaults))
gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.comm",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = c("comm"),
                                                 stocknames = c("codimm", "codmat")))
rm(aggdata)
