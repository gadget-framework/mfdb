# Combine multiple result tables, preserving grouping
mfdb_concatenate_results <- function (...) {
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
mfdb_find_species <- Vectorize(function(partial_name, single_matches_only = FALSE) {
    utils::data('species', package = 'mfdb', envir = environment())
    matches <- species[grepl(
        paste(strsplit(partial_name, "\\s+")[[1]], collapse = ".*\\s+"),
        species$description, ignore.case = TRUE),]
    if (single_matches_only && nrow(matches) != 1) {
        return(data.frame(
            id = NA,
            name = NA,
            description = NA,
            stringsAsFactors = FALSE))
    }
    data.frame(
        id = matches$id,
        name = as.character(matches$name),
        description = as.character(matches$description),
        stringsAsFactors = FALSE)
}, USE.NAMES = TRUE)
