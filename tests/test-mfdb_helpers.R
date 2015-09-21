library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

ok_group("mfdb_find_species", {
    # Can fetch multiple matches in one go
    species <- mfdb_find_species(c("gad. mor", "gadus Mor"))
    ok(ncol(species) == 2, "2 results from species")
    ok(cmp(species[,"gad. mor"], list(
        id = 8791030402,
        name = "COD",
        description = "Cod (Gadus Morhua)")), "gad. mor matches only cod")
    ok(cmp(species[,"gadus Mor"], list(
        id = 8791030402,
        name = "COD",
        description = "Cod (Gadus Morhua)")), "gadus Mor matches only cod")

    # Multiple matches come as vectors
    ok(cmp(mfdb_find_species(c("worms"))[,"worms"], list(
        id = c(5001000000, 5001240400, 8300000000, 9999999999),
        name = c("BWX", "NEX", "CTZ", "TBX"),
        description = c("Bristle Worms (Polychaeta)", "Ragworms (Nereis Spp)", "Arrow Worms (Chaetognatha)", "Tube Worms (Tubeworms)"))), "Worms matches 4 species")

    # No matches produces empty vectors
    ok(cmp(mfdb_find_species(c("not_a_species"))[,"not_a_species"], list(
        id = c(0)[c()],
        name = c("")[c()],
        description = c("")[c()])), "No matches produces empty vector")
})
