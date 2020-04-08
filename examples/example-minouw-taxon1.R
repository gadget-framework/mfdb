#  install.packages("measurements")
library(openxlsx)
library(measurements)
library(mfdb)

# xlsx_file <- "T1.1 review template FINAL REV1_CCMAR_REV01F.xlsx"
#    # Swap mis-labelled hauls
#    haul <- gsub("BRD_Spring_", "BRD_Primavera_", haul)
#    haul <- gsub("BRD_Autumn_", "BRD_Outono_", haul)
#    haul <- gsub("BRD_Summer_", "BRD_Verao_", haul)
#    haul <- gsub("BRD_Winter_", "BRD_Inverno_", haul)
#    # Multiple duration formats
#    fraction of day (actual excel format)
#    "05:55" hours/mins
#    "42:32" mins/secs
# cibm problems
# * Empty rows at start of spreadsheet
# * Inconsistent lat/lon formats: 42\u00B029'50 vs 42 26 13
# * 10 46978 --> 10 46 978
# * No locations defined
# * Column names including all data "discarded (D) N"
# * Some rows with weight but 0 count

xlsx_files <- c(
    cccmar = "T1.1 review template FINAL REV1_CCMAR_REV01F.xlsx",
    cibm = "BY TAXON 1 GSA9 Partner CIBM.xlsx",
    ""
)
sanitize_lat_lon <- function (s) {
    s <- gsub("\u00B0", " ", s) # i.e. degree symbol
    s <- gsub("'", " ", s)
    measurements::conv_unit(s, from = 'deg_min_sec', to = 'dec_deg')
}

sanitize_haul_name <- function (haul) {
    haul <- gsub("\\W", "_", haul)  # MFDB doesn't support spaces in names
    return(haul)
}

months <- c(
    Jan = 1, Feb = 2, Mar = 3, Apr = 4, May = 5, Jun = 6,
    Jul = 7, Aug = 8, Sep = 9, Oct = 10, Nov = 11, Dec = 12
)

########### Read in cibm
case_study <- "cibm"

# Open mfdb for this case study
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb(paste0('example-minouw-', case_study), destroy_schema = TRUE)
mdb <- mfdb(paste0('example-minouw-', case_study))

# Read taxon1 data, tidy up
by_taxon1 <- openxlsx::readWorkbook(xlsx_files[[case_study]], sheet="by_taxon1", startRow = 2, colNames = TRUE)
by_taxon1$taxon <- gsub("\\s+", "", by_taxon1$taxon)
by_taxon1$haul_code <- sanitize_haul_name(by_taxon1$haul_code)
by_taxon1$lat <- sanitize_lat_lon(by_taxon1$lat)
by_taxon1$lon <- sanitize_lat_lon(by_taxon1$lon)
by_taxon1$month <- months[match(by_taxon1$month, names(months))]
by_taxon1$location <- "ccmar"

# Recalculate missing NAs
by_taxon1$repl_comm <- ifelse((by_taxon1$X24 > 0 & by_taxon1$X21 > 0 & by_taxon1$X22 == 0), NA, by_taxon1$X22)
by_taxon1$repl_total <- ifelse((by_taxon1$X24 > 0 & by_taxon1$X21 > 0 & by_taxon1$X22 == 0), NA, by_taxon1$X20)

# Create species taxonomy based on table
species <- by_taxon1[!duplicated(by_taxon1$taxon) & !is.na(by_taxon1$taxon), c("taxon", "X16")]
names(species) <- c('name', 'description')
mfdb_empty_taxonomy(mdb, 'species')
mfdb_import_species_taxonomy(mdb, species)

# Extract tow data from taxon table, import it
tow <- by_taxon1[!duplicated(by_taxon1$haul_code), c("haul_code", "lat", "lon", "depth", "duration.(h)")]
names(tow) <- c('name', 'latitude', 'longitude', 'depth', 'duration')
mfdb_import_tow_taxonomy(mdb, tow)

# Generate 2 sub-tows, commercial and discard
tow$t_group <- tow$name
tow$name <- paste(tow$t_group, 'C', sep = ".")
mfdb_import_tow_taxonomy(mdb, tow)
tow$name <- paste(tow$t_group, 'D', sep = ".")
mfdb_import_tow_taxonomy(mdb, tow)

# Generate all areas mentioned in table
mfdb_import_area(mdb, data.frame(
    name = unique(by_taxon1$location),
    division = c('all'),
    size = c(1)))

# First import catch
comm_survey <- by_taxon1[, c("year", "month", "location", "haul_code", "taxon", "X21")]
# comm_survey$species <- unlist(species[match(comm_survey$taxon, names(species))])
names(comm_survey) <- c("year", "month", "areacell", "orig_tow_name", "species", "weight_total")
comm_survey$tow <- paste(comm_survey$orig_tow_name, 'C', sep = ".")
mfdb_import_survey(mdb, data_source = "_comm", comm_survey)

# Then discard from this catch
disc_survey <- by_taxon1[, c("year", "month", "location", "haul_code", "taxon", "X23")]
# disc_survey$species <- unlist(species[match(disc_survey$taxon, names(species))])
names(disc_survey) <- c("year", "month", "areacell", "orig_tow_name", "species", "weight_total")
disc_survey$tow <- paste(disc_survey$orig_tow_name, 'D', sep = ".")
mfdb_import_survey(mdb, data_source = "_disc", disc_survey)
