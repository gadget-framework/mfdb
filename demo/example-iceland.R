# To run this script, first install data sources: fjolst, geo
library(mfdb)
library(fjolst)
library(geo)
library(data.table)

# Create connection to MFDB database, as the Icelandic case study
mdb <- mfdb('Iceland')

# Import area definitions
reitmapping <- read.table(
    system.file("demo-data", "reitmapping.tsv", package="mfdb"),
    header=TRUE,
    as.is=TRUE)
predator_mapping <- data.frame(
    GRIDCELL =  c(2842, 2872, 2882, 2892, 3372, 3382, 4392, 5382, 5872, 5882, 6362, 6372, 6842, 6852),
    DIVISION    = c(rep(201, 10), rep(202, 4)),
    SUBDIVISION = c(2010, 2011, 2012, 2013, 2014, 2015, 2015, 2016, 2017, 2018, 2020, 2021, 2022, 2023),
    stringsAsFactors = FALSE)

mfdb_import_area(mdb, data.frame(
    name = c(reitmapping$GRIDCELL, predator_mapping$GRIDCELL),
    size = c(5)))
mfdb_import_division(mdb, c(
    lapply(split(reitmapping, list(reitmapping$SUBDIVISION)), function (l) l[,'GRIDCELL']),
    lapply(split(predator_mapping, list(predator_mapping$SUBDIVISION)), function (l) l[,'GRIDCELL']),
    NULL))
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
comm.ldist$sampling_type <- 'SEA'
mfdb_import_survey(mdb,
                   data_source = 'example-iceland-comm.ldist',
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
ldist.aug$sampling_type <- 'IGFS'
mfdb_import_survey(mdb,
                   data_source = 'example-iceland-ldist.aug',
                   ldist.aug)
rm(ldist.aug)

# Create a gadget directory, define some defaults to use with our queries below
gd <- gadget_directory("example-iceland-model")
defaults <- list(
    area = mfdb_group("101" = unique(c(reitmapping$SUBDIVISION, predator_mapping$GRIDCELL))),
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

proc.time()

# Stomach data

# ### ffiskar - Predator
# synis.id - survey id (Join to get year/month/lat/lon)
# flokk.id - category id
#
# faerslunumer - entry number (not unique to synis.id)
# ranfiskur - predator (TODO: count? Type?)
# lengd - length of predator
# 
# fj.fmaga - Prey in stomach? (0/1)
# fj.omelt - Undigested? (0/1)
# fj.tomra - Empty stomachs? (0/1)
# fj.aelt - Vomited? (0/1)
# kvarnanr - otolith number (an identifier) "biologists can use the otoliths recovered...to determine the type of fish they ate."
# fj.uthverfir - number with stomachs inverted / inside out
# lenfi - (prey) length category
# 
# The other one I use is fhopar:
# ### fhopar - Prey
# flokk.id - category id (TODO: Can we use this as a stomach ID, does it link to the other table)
# faeduhopur - prey group (can be down to the species or as far as even a phylum I think ... these are in latin thanksfully)
# fjoldi - number
# thyngd - weight
# meltingarstig - State of digestion 1:undigested--5:completely digested

# Map some of the prey species
prey_species <- c(
    "ammodytx" = "SAX",
    "amphipod" = "AAZ",
    "pand bor" = "PRA",
    NULL)

# Select predators and prey for autumn survey
stations <- subset(stodvar, synaflokkur == 30) ## autumn survey 35
predators <- subset(ffiskar, synis.id %in% stations$synis.id & ranfiskur == 1)
predators <- merge(predators, stations[c('synis.id','ar','man','lat','lon')])
preys <- subset(fhopar, flokk.id %in% predators$flokk.id)

mfdb_import_stomach(mdb,
    data_source = "stomach-synaflokkur-30",
    predator_data = data.frame(
        stomach_name = predators$flokk.id,
        year = predators$ar,
        month = predators$man,
        areacell = d2sr(predators$lat, -predators$lon),
        species = 'COD',
        length = predators$lengd,
        stringsAsFactors = TRUE),
    prey_data = data.frame(
        stomach_name = preys$flokk.id,
        species = prey_species[preys$faeduhopur],
        digestion_stage = preys$meltingarstig,
        length = preys$thyngd,
        count = preys$fjoldi,
        stringsAsFactors = TRUE))

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

proc.time()
