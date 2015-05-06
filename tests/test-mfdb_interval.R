library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

g <- NULL
mdb <- fake_mdb()

ok_group("Can generate objects", {
    expect_error(
        mfdb_interval("l", c()),
        "vect must at least be 2 items long")
    expect_error(
        mfdb_interval("l", c(1)),
        "vect must at least be 2 items long")

    expect_equal(
        class(mfdb_interval("l", c(1, 100))),
        c("mfdb_interval", "mfdb_aggregate"))
})

ok_group("Elements are named by prefixes", local({
    expect_equal(
        agg_summary(mdb, mfdb_interval("l", c(10,20,25,30)), 'col', 'out', data.frame(), 0),
        list(l10 = c(10,20), l20 = c(20,25), l25 = c(25,30)))
    expect_equal(
        agg_summary(mdb, mfdb_interval("l", c(5,15,25,30)), 'col', 'out', data.frame(), 0),
        list(l5 = c(5,15), l15 = c(15,25), l25 = c(25,30)))
}, asNamespace('mfdb')))

ok_group("Can generate SQL", local({
    ok(cmp(
            select_clause(mdb, mfdb_interval("l", c(10,20,30)), "col", "outname"),
            "CASE WHEN col >= 30 THEN NULL WHEN col >= 20 THEN 'l20' WHEN col >= 10 THEN 'l10' END AS outname"),
        "Generated select clause")

    ok(cmp(
            where_clause(mdb, mfdb_interval("l", c(10,20,25,30)), "col", "out"),
            c("col >= 10", "col < 30")),
        "Generated where clause")
    ok(cmp(
            where_clause(mdb, mfdb_interval("l", c(25,50)), "col", "out"),
            c("col >= 25", "col < 50")),
        "Generated where clause")
}, asNamespace('mfdb')))

ok_group("Aggregates with open_ended mfdb_interval", local({
    g <<- mfdb_interval('l', c(10, 20, 100), open_ended = TRUE)
    ok(cmp(capture.output(pre_query(NULL, g, "out")), c(
        "NULL")), "Nothing happened pre_query")
    ok(cmp(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), "CASE WHEN col >= 100 THEN 'l100' WHEN col >= 20 THEN 'l20' WHEN col >= 10 THEN 'l10' END AS out"), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), c()), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), "col >= 10"), "Where clause")
}, asNamespace('mfdb')))
