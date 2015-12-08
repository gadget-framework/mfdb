library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

ok_group("mfdb_concatenate_results", {
    year_group <- mfdb_group("1998" = "1998", "1999" = "1999", "2000" = "2000", "2001" = "2001")
    area_group <- mfdb_group(divA = c("divA"), divB = c("divB"))
    age_group <- mfdb_group(all = 1:1000)
    length_group <- mfdb_interval("len", seq(0, 50, by = 5))

    noattr <- function(obj) {
        # NB: We're removing class, table_name attributes here, since the concatenation removes them anyway.
        # Technically this is a bug in agg_summary, but it will cause problems in a lot of other tests.
        attributes(obj) <- list(names = names(obj))
        return(obj)
    }
    fake_results <- local(function (mdb, d, year, step, area) {
        structure(
            d,
            year = noattr(agg_summary(mdb, year, 'year', 'year', d, 1)),
            step = noattr(agg_summary(mdb, step, 'step', 'step', d, 1)),
            area = noattr(agg_summary(mdb, area, 'area', 'area', d, 1)))
    }, asNamespace('mfdb'))

    # Combine 2 data frames
    combined <- mfdb_concatenate_results(
        fake_results(fake_mdb(),
            data.frame(year = c(1998:1999), step = c("1", "2"), area = c("divA"), number = c(5, 4), stringsAsFactors = FALSE),
            year = year_group,
            step = mfdb_timestep_biannually,
            area = area_group),
        fake_results(fake_mdb(),
            data.frame(year = c(2000:2001), step = c("2", "1"), area = c("divA", "divB"), number = c(8, 9), stringsAsFactors = FALSE),
            year = year_group,
            step = mfdb_timestep_biannually,
            area = area_group))
    ok(cmp(
        combined,
        fake_results(fake_mdb(),
            data.frame(
                year = c(1998:1999, 2000:2001),
                step = c("1", "2", "2", "1"),
                area = c("divA", "divA", "divA", "divB"),
                number = c(5, 4, 8, 9),
                stringsAsFactors = FALSE),
            year = year_group,
            step = mfdb_timestep_biannually,
            area = area_group)), "Combined data sets")
})

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
