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

sanitize_name <- function (haul) {
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
     SBG = "Sparus aurata",
     KNU = "",
     PNS = "",
     STM = "",
     RMO = "",
     RPZ = "",
     LUZ = "",
     GHD = "",
     EXY = "",
     MON = "",
     ANE = "",
     ANK = "",
     BOG = "",
     DPS = "",
     HOM = "",
     LDB = "",
     MAZ = "",
     MUL = "",
     MUT = "",
     NEP = "",
     OCC = "",
     PIL = "",
     POD = "",
     RPG = "",
     SRG = "",
     TGS = "",
     AWT = "",
     EPT = "",
     BZV = "",
     CTZ = "",
     AJL = "",
     GUU = "",
     GUM = "",
     OAM = "",
     SOS = "",
     PIC = "",
     YFX = ""
)
fleets <- c(
    comm.large = "commercial >150 HP",
    comm.small = "commercial <150 HP",
    otb.large = "OTBlarge",
    otb.small = "OTBsmall",
    bll.small = "Bottom Longline hooksize  <5",
    gil.med = "Gill net ≥50 mm and <100 mm",
    gtr.small = "Trammel net <40 mm",
    gtr.med = "Trammel net ≥40 and <60",
    gtr.large = "Trammel net <40 mm"
)
reverse_lookup <- function (l, col) {
    # Generate a column based on values in (col)
    structure(names(l), names = l)[col]
}
########### Read in CSIC
case_study <- "csic"

# Open mfdb for this case study
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb(paste0('example-minouw-', case_study), destroy_schema = TRUE)
mdb <- mfdb(paste0('example-minouw-', case_study))

# Read by_taxon2
by_taxon2 <- openxlsx::readWorkbook(xlsx_files[[case_study]], startRow = 2, sheet="by_taxon2", colNames = TRUE)
by_taxon2$date <- sanitize_excel_date(by_taxon2$date)
colnames(by_taxon2)[c(9,10,13,14,17,18)] <- paste0("comm.",colnames(by_taxon2)[c(9,10,13,14,17,18)])
colnames(by_taxon2)[c(11,12,15,16,19,20)] <- paste0("disc.",colnames(by_taxon2)[c(11,12,15,16,19,20)])

# Genrate tows from depth definitions
tow <- data.frame(name = sanitize_name(unique(paste(by_taxon2$cruise, by_taxon2$depth, sep = "_"))))
mfdb_import_tow_taxonomy(mdb, tow)

# Generate 2 sub-tows, commercial and discard
tow$t_group <- tow$name
tow$name <- paste(tow$t_group, 'C', sep = ".")
mfdb_import_tow_taxonomy(mdb, tow)
tow$name <- paste(tow$t_group, 'D', sep = ".")
mfdb_import_tow_taxonomy(mdb, tow)

# Generate all areas mentioned in table
mfdb_import_area(mdb, data.frame(
    name = unique(by_taxon2$location),
    division = c('all'),
    size = c(1)))

# Create species taxonomy based on table
mfdb_empty_taxonomy(mdb, 'species')
mfdb_import_species_taxonomy(mdb, data.frame(name = names(species), description = species))

# Create fleet taxonomy
mfdb_import_vessel_taxonomy(mdb, data.frame(
    name = names(fleets),
    full_name = fleets))

# First import catch
comm_survey <- by_taxon2[!is.na(by_taxon2[,"comm.kg"]), c("cruise", "date", "location", "fleet", "depth", "taxon", "comm.kg")]
names(comm_survey) <- c("cruise", "date", "areacell", "vessel", "depth", "species", "weight")
comm_survey$year <- comm_survey$date$year + 1900
comm_survey$month <- comm_survey$date$mon + 1
comm_survey$tow <- paste0(sanitize_name(paste0(comm_survey$cruise, "_", comm_survey$depth)), ".C")
comm_survey$vessel <- reverse_lookup(fleets, comm_survey$vessel)
mfdb_import_survey(mdb, data_source = "comm.kg.total", comm_survey)

print(mfdb_sample_totalweight(mdb, c('tow', 'vessel'), list(
    tow = mfdb_unaggregated(),
    vessel = mfdb_unaggregated(),
    data_source = c("comm.kg.total")
)))
