library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

mdb <- fake_mdb()
g <- NULL

ok_group("Aggregates with mfdb_unaggregated(omitNA = FALSE)", local({
    g <<- mfdb_unaggregated()

    pre_query(mdb, g, "col")  # Just check nothing happens
    ok(cmp(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), "col AS out"), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), c()), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), c()), "Where clause")

    ok(cmp_error(
        agg_summary(mdb, mfdb_unaggregated(), 'c.claire', 'claire', data.frame(year = c(1998,1998,1999,1999)), 0),
        "claire"), "Can't convert unaggregated without data")

    ok(cmp(
        agg_summary(mdb, mfdb_unaggregated(), 'c.year', 'year', data.frame(year = c(1998,1998,1999,1999)), 0),
        list("1998" = 1998, "1999" = 1999)),
        "Can convert unaggregated using returned data")
}, asNamespace('mfdb')))

ok_group("Aggregates with mfdb_unaggregated(omitNA = TRUE)", local({
    g <<- mfdb_unaggregated(omitNA = TRUE)

    pre_query(mdb, g, "col")  # Just check nothing happens
    ok(cmp(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), "col AS out"), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), c()), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), "col IS NOT NULL"), "Where clause")
}, asNamespace('mfdb')))

ok_group("Aggregates with mfdb_unaggregated() global taxonomies", local({
    g <<- mfdb_unaggregated()

    ok(cmp(
        select_clause(mdb, g, 'tbl.gear_id', 'out'),
        "(SELECT name FROM gear WHERE gear_id = tbl.gear_id) AS out"), "Select clause")
}, asNamespace('mfdb')))

ok_group("Aggregates with mfdb_unaggregated() CS taxonomies", local({
    g <<- mfdb_unaggregated()

    ok(cmp(
        select_clause(mdb, g, 'tbl.sampling_type_id', 'out'),
        "(SELECT name FROM sampling_type WHERE case_study_id = 0 AND sampling_type_id = tbl.sampling_type_id) AS out"), "Select clause")
}, asNamespace('mfdb')))
