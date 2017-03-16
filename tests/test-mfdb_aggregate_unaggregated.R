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

    ok(cmp(
        agg_summary(mdb, mfdb_unaggregated(), 'c.year', 'year', data.frame(), 0),
        list()),
        "Empty data frame aggregates to empty list")

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

ok_group("Aggregates with mfdb_unaggregated() taxonomies", local({
    g <<- mfdb_unaggregated()

    ok(cmp(
        select_clause(mdb, g, 'tbl.gear_id', 'out'),
        "(SELECT name FROM gear WHERE gear_id = tbl.gear_id) AS out"), "Select clause")
}, asNamespace('mfdb')))

ok_group("Aggregates with mfdb_unaggregated(like / not_like)", local({
    g <<- mfdb_unaggregated(like = "fish%")
    ok(cmp(
        where_clause(mdb, g, "col", "out"),
        "(col LIKE 'fish%')"
    ), "Where clause (not-taxonomy)")
    ok(cmp(
        where_clause(mdb, g, "tbl.gear_id", "out"),
        "(tbl.gear_id IN (SELECT gear_id FROM gear WHERE name  LIKE 'fish%'))"
    ), "Where clause (taxonomy)")

    g <<- mfdb_unaggregated(like = c("fish%", "boops%"), not_like = c("crab%"))
    ok(cmp(
        where_clause(mdb, g, "col", "out"),
        c(
            "(col LIKE 'fish%' OR col LIKE 'boops%')",
            "(col NOT LIKE 'crab%')"
        )
    ), "Where clause (not-taxonomy, multiple terms)")
    ok(cmp(
        where_clause(mdb, g, "tbl.gear_id", "out"),
        c(
            "(tbl.gear_id IN (SELECT gear_id FROM gear WHERE name  LIKE 'fish%' OR name  LIKE 'boops%'))",
            "(tbl.gear_id IN (SELECT gear_id FROM gear WHERE name  NOT LIKE 'crab%'))"
        )
    ), "Where clause (taxonomy, multiple terms)")
}, asNamespace('mfdb')))
