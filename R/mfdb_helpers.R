# Combine multiple result tables, preserving grouping
mfdb_combine_results <- function (...) {
    combined <- rbind(...)
    for (column in names(combined)) {
        x <- lapply(list(...), function (df) attr(df, column))  # Extract attribute list for each df
        x <- do.call(c, x)  # Concatenate the list of attribute lists together
        x <- x[!duplicated(names(x))]  # Strip duplicates, set it against the new df
        attr(combined, column) <- x
    }
    return(combined)
}

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
