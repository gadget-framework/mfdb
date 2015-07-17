# Guess at species based on a partial species name
mfdb_find_species <- Vectorize(function(partial_name) {
    matches <- mfdb::species[grepl(
        paste(strsplit(partial_name, "\\s+")[[1]], collapse = ".*\\s+"),
        mfdb::species$description, ignore.case = TRUE),]
    data.frame(
        id = matches$id,
        name = as.character(matches$name),
        description = as.character(matches$description),
        stringsAsFactors = FALSE)
}, USE.NAMES = TRUE)
