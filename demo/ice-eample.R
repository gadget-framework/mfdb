.libPaths(c("/home/bthe/Documents/mfdb-workspace/Rpackages",
          "/usr/local/lib/R/site/3.1/x86_64/library",
          "/home/bthe/r/x86_64/library"))
setwd('~/Documents/mfdb-workspace')
library(mfdb)
library(fjolst)
library(geo)
library(plyr)
library(data.table)
reitmapping <-
  read.table('reitmapping.tsv',header=TRUE,as.is=TRUE)

sub.split <- dlply(reitmapping, 'SUBDIVISION', function(x) x$GRIDCELL)
defaults <- list(
  area = mfdb_group(
    "101" = reitmapping$SUBDIVISION),
  timestep = mfdb_timestep_quarterly, # Group months to create 2 timesteps for each year
  year = 1984:2012)


mdb <- mfdb('Iceland')


gd <- gadget_directory("out")
gadget_dir_write(gd,gadget_areafile(
                                 size = mfdb_area_size(mdb, defaults)[[1]],
                                 temperature = mfdb_temperature(mdb, defaults)[[1]]))



## area setup
mfdb_import_area(mdb, data.frame(id = 1:nrow(reitmapping), name = reitmapping$GRIDCELL, size = c(5)))
mfdb_import_division(mdb, sub.split)
mfdb_import_temperature(mdb, data.frame(
  year = 2012,
  month = 1:12,
  areacell = reitmapping$GRIDCELL[1],
  temperature = 3))



## Actual data 
## 1 inspectors, 2 hafro, 8 on-board discard 
stations <- subset(stodvar, synaflokkur == 30) ## autumn survey 35
comm.stations <- subset(stodvar, synaflokkur == 8)
ldist <- subset(all.le, synis.id %in% stations$synis.id & tegund == 1)
comm.ldist <- subset(all.le, synis.id %in% comm.stations$synis.id & 
                       tegund == 1)
adist <- subset(all.kv, synis.id %in% stations$synis.id & tegund == 1 )
num.dist <- subset(all.nu, synis.id %in% stations$synis.id & tegund == 1 )
ldist.aug <- Skala.med.toldum(ldist,num.dist, tegund==1)
adist.aug <- Skala.med.toldum(adist,num.dist, tegund==1)

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
                   data_source = 'comm-test',
                   sampling_type = 'SEA',
                   comm.ldist)


ldist.aug <- merge(ldist.aug, stations[c('synis.id','ar','lat','lon')])
ldist.aug$areacell <- d2sr(ldist.aug$lat,-ldist.aug$lon)
names(ldist.aug) <- c('sample.id','species','length','num','sex',
                      'maturity','count','year','lat','lon','areacell')
ldist.aug$species <- 'COD' 
ldist.aug$month  <- 3

ldist.aug <- subset(ldist.aug,!is.na(areacell))
ldist.aug <- subset(ldist.aug,areacell %in% reitmapping$GRIDCELL) 
ldist.aug$sex <- c('M','F')[ldist.aug$sex]
ldist.aug$age <- 0
  mfdb_import_survey(mdb,
                     data_source = 'igfs-test',
                     sampling_type = 'IGFS', 
                     ldist.aug)

## lets do something useful
comm.query <- 
mfdb_agelength(mdb, c(list(
  age = mfdb_group(all=0),
  sampling_type = 'SEA',
  length = mfdb_interval("len", seq(0, 150, by = 2))),
  defaults))

gadget_dir_write(gd, gadget_likelihood_component("penalty",
                                                 name = "bounds",
                                                 weight = "0.5",
                                                 data = data.frame(
                                                   switch = c("default"),
                                                   power = c(2),
                                                   stringsAsFactors = FALSE)))


gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.igfs",
                                                 weight = 1,
                                                 data = tmp[[1]],
                                                 fleetnames = c("igfs"),
                                                 stocknames = c("codimm", "codmat")))


gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.comm",
                                                 weight = 1,
                                                 data = tmp[[1]],
                                                 fleetnames = c("comm"),
                                                 stocknames = c("codimm", "codmat")))

