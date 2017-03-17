#  install.packages("measurements")
library(openxlsx)
library(measurements)
library(mfdb)

xlsx_files <- c(
    cccmar = "T1.1 review template FINAL REV1_CCMAR_REV01F.xlsx",
    cibm = "BY TAXON 1 GSA9 Partner CIBM.xlsx",
    csic = "T1.1 review template FINAL ICM-CSIC.xlsx",
    ""
)
sanitize_lat_lon <- function (s) {
    s <- gsub("°", " ", s)
    s <- gsub("'", " ", s)
    measurements::conv_unit(s, from = 'deg_min_sec', to = 'dec_deg')
}

sanitize_haul_name <- function (haul) {
    haul <- gsub("\\W", "_", haul)  # MFDB doesn't support spaces in names
    return(haul)
}

sanitize_excel_date <- function (d) {
    # Turn an excel date offset into a POSIXlt object
    as.POSIXlt(d * (60*60*24), origin="1899-12-30", tz="GMT")
}

months <- c(
    Jan = 1, Feb = 2, Mar = 3, Apr = 4, May = 5, Jun = 6,
    Jul = 7, Aug = 8, Sep = 9, Oct = 10, Nov = 11, Dec = 12
)

species <- c(
     ANN = "Diplodus annularis",
     SSB = "Lithognathus mormyrus",
     MUR = "Mullus surmuletus",
     SBA = "Pagellus acarne",
     PAC = "Pagellus erythrinus",
     SNQ = "Scorpaena notata",
     BBS = "Scorpaena porcus",
     HMM = "Trachurus mediterraneus",
     HKE = "Merluccius merluccius",
     WHB = "Micromesistius poutassou",
     GFB = "Phycis blennoides",
     BOY = "Bolinus brandaris",
     BSS = "Dicentrarchus labrax",
     SBR = "Pagellus bogaraveo",
     SOL = "Solea vulgaris",  # NB: ASFIS calls this "Solea solea"
     SBG = "Sparus aurata"
)
gear <- read.table(textConnection(
'name		description		mesh_size_min	mesh_size_max
OTB.DM38	"traditional Catalan trawl"	38	38
OTB.DM40	"traditional Catalan trawl"	40	40
OTB.DM41	"traditional Catalan trawl"	40	40
OTB.DM42	"traditional Catalan trawl"	40	40
OTB.DM43	"traditional Catalan trawl"	40	40
OTB.DM44	"traditional Catalan trawl"	40	40
OTB.DM45	"traditional Catalan trawl"	40	40
OTB.DM46	"traditional Catalan trawl"	40	40
OTB.DM47	"traditional Catalan trawl"	40	40
OTB.DM48	"traditional Catalan trawl"	40	40
OTB.DM49	"traditional Catalan trawl"	40	40
OTB.DM50	"traditional Catalan trawl"	40	40
OTB.DM51	"traditional Catalan trawl"	40	40
OTB.DM52	"traditional Catalan trawl"	40	40
OTB.DM53	"traditional Catalan trawl"	40	40
OTB.DM54	"traditional Catalan trawl"	40	40
OTB.DM55	"traditional Catalan trawl"	40	40
OTB.DM56	"traditional Catalan trawl"	40	40
OTB.DM57	"traditional Catalan trawl"	40	40
OTB.DM58	"traditional Catalan trawl"	40	40
OTB.DM59	"traditional Catalan trawl"	40	40
HOK.5		"Bottom Longline hooksize <5"	5	5
GTS.GNS_50_100	"Gill net ≥50 mm and <100 mm"	50	100
GTS.GTR_40	"Trammel net <40 mm"		0	40
GTS.GTR_40_60	"Trammel net ≥40 and <60"	40	6
'), header = TRUE)
########### Read in CSIC
case_study <- "csic"

# Open mfdb for this case study
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb(paste0('example-minouw-', case_study), destroy_schema = TRUE)
mdb <- mfdb(paste0('example-minouw-', case_study))

# Read size_frq / haul
size_frq <- openxlsx::readWorkbook(xlsx_files[[case_study]], sheet="size frq", colNames = TRUE)
haul <- openxlsx::readWorkbook(xlsx_files[[case_study]], sheet="haul", colNames = TRUE)
haul$date <- sanitize_excel_date(haul$date)

# Populate empty taxon column
rev_species <- structure(names(species), names = species)
size_frq$taxon <- rev_species[size_frq$taxon2]

# Merge together into one denormalised table
size_frq_haul <- merge(
    size_frq,
    haul,
    by = "haul", all.x = TRUE,
)

# Extract tow data from taxon table, import it
# NB: No depth, since it's not useful, add a depth_category column?
tow <- size_frq_haul[!duplicated(size_frq_haul$haul), c("haul"), drop = FALSE]
names(tow) <- c('name')
mfdb_import_tow_taxonomy(mdb, tow)

# Generate 2 sub-tows, commercial and discard
tow$t_group <- tow$name
tow$name <- paste(tow$t_group, 'C', sep = ".")
mfdb_import_tow_taxonomy(mdb, tow)
tow$name <- paste(tow$t_group, 'D', sep = ".")
mfdb_import_tow_taxonomy(mdb, tow)

# Generate all areas mentioned in table
mfdb_import_area(mdb, data.frame(
    name = unique(size_frq_haul$location),
    division = c('all'),
    size = c(1)))

# Create gear taxonomy based on table
gear <- unique(haul[,c('fishing.gear', 'mesh', 'mesh.size', 'other.aspects.of.net.configuration?')])
mfdb_empty_taxonomy(mdb, 'gear')
mfdb_import_gear_taxonomy(mdb, data.frame(
    name = paste(gear[,"fishing.gear"], gear[,"mesh"], sep = "."),
    description = gear[,"other.aspects.of.net.configuration?"],
    mesh_size = gear[,"mesh.size"]))

# Create species taxonomy based on table
mfdb_empty_taxonomy(mdb, 'species')
mfdb_import_species_taxonomy(mdb, data.frame(name = names(species), description = species))

# First import catch
comm_survey <- size_frq_haul[size_frq_haul$fate == 'C', c("haul", "date", "location", "taxon", "size", "N", "fishing.gear", "mesh")]
names(comm_survey) <- c("orig_tow_name", "date", "areacell", "species", "length", "count", "fishing.gear", "mesh")
comm_survey$year <- comm_survey$date$year + 1900
comm_survey$month <- comm_survey$date$mon + 1
comm_survey$tow <- paste(comm_survey$orig_tow_name, 'C', sep = ".")
comm_survey$gear <- paste(comm_survey[,"fishing.gear"], comm_survey[,"mesh"], sep = ".")
mfdb_import_survey(mdb, data_source = "_comm", comm_survey)

# Then discard from this catch
disc_survey <- size_frq_haul[size_frq_haul$fate == 'D', c("haul", "date", "location", "taxon", "size", "N", "fishing.gear", "mesh")]
names(disc_survey) <- c("orig_tow_name", "date", "areacell", "species", "length", "count", "fishing.gear", "mesh")
disc_survey$year <- disc_survey$date$year + 1900
disc_survey$month <- disc_survey$date$mon + 1
disc_survey$tow <- paste(disc_survey$orig_tow_name, 'D', sep = ".")
mfdb_import_survey(mdb, data_source = "_disc", disc_survey)

# Sample queries
mfdb_sample_count(mdb, c('tow'), list(tow = c('EMPAFISH2003-2005')))
