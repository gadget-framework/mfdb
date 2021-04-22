library(mfdb)
library(unittest, quietly = TRUE)
helpers <- c('utils/helpers.R', 'tests/utils/helpers.R') ; source(helpers[file.exists(helpers)])

g <- NULL
mdb <- fake_mdb()

ok_group("Can generate objects", {
    ok(ut_cmp_error(
        mfdb_interval("l", c()),
        "vect must at least be 2 items long"))
    ok(ut_cmp_error(
        mfdb_interval("l", c(1)),
        "vect must at least be 2 items long"))

    ok(ut_cmp_identical(
        class(mfdb_interval("l", c(1, 100))),
        c("mfdb_interval", "mfdb_aggregate")))
})

ok_group("Elements are named by prefixes", local({
    ok(ut_cmp_identical(
        agg_summary(mdb, mfdb_interval("l", c(10,20,25,30)), 'col', 'out', data.frame(), 0),
        list(
            l10 = structure(call("seq", 10, 19), min = 10, max = 20),
            l20 = structure(call("seq", 20, 24), min = 20, max = 25),
            l25 = structure(call("seq", 25, 29), min = 25, max = 30))))
    ok(ut_cmp_identical(
        agg_summary(mdb, mfdb_interval("l", c(5,15,25,30)), 'col', 'out', data.frame(), 0),
        list(
            l5  = structure(call("seq",  5, 14), min =  5, max = 15),
            l15 = structure(call("seq", 15, 24), min = 15, max = 25),
            l25 = structure(call("seq", 25, 29), min = 25, max = 30))))

    ok(ut_cmp_identical(
        agg_summary(mdb, mfdb_interval("l", c(5,15,25,30), open_ended = c('upper')), 'col', 'out', data.frame(), 0),
        list(
            l5  = structure(call("seq",  5, 14), min =  5, max = 15),
            l15 = structure(call("seq", 15, 24), min = 15, max = 25),
            l25 = structure(call("seq", 25, 29), min = 25, max = 30, max_open_ended = TRUE))), "agg_summary: open_ended=upper goes on final group")

    ok(ut_cmp_identical(
        agg_summary(mdb, mfdb_interval("l", c(5,15,25,30), open_ended = c('lower')), 'col', 'out', data.frame(), 0),
        list(
            l5  = structure(call("seq",  5, 14), min =  5, max = 15, min_open_ended = TRUE),
            l15 = structure(call("seq", 15, 24), min = 15, max = 25),
            l25 = structure(call("seq", 25, 29), min = 25, max = 30))), "agg_summary: open_ended=lower goes on first group")

    ok(ut_cmp_identical(
        agg_summary(mdb, mfdb_interval("l", c(5, 15), open_ended = c('lower', 'upper')), 'col', 'out', data.frame(), 0),
        list(
            l5  = structure(
                call("seq",  5, 14),
                min = 5,
                max = 15,
                min_open_ended = TRUE,
                max_open_ended = TRUE))), "agg_summary: Can have both on a single group")


}, asNamespace('mfdb')))

ok_group("Can generate SQL", local({
    ok(ut_cmp_identical(
            select_clause(mdb, mfdb_interval("l", c(10,20,30)), "col", "outname"),
            "CASE WHEN col < CAST(20 AS REAL) THEN 'l10' WHEN col < CAST(30 AS REAL) THEN 'l20' ELSE 'l20' END AS outname"),
        "Generated select clause")

    ok(ut_cmp_identical(
            where_clause(mdb, mfdb_interval("l", c(10,20,25,30)), "col", "out"),
            c("col >= CAST(10 AS REAL)", "col < CAST(30 AS REAL)")),
        "Generated where clause")
    ok(ut_cmp_identical(
            where_clause(mdb, mfdb_interval("l", c(25,50)), "col", "out"),
            c("col >= CAST(25 AS REAL)", "col < CAST(50 AS REAL)")),
        "Generated where clause")
}, asNamespace('mfdb')))

ok_group("Aggregates with open_ended mfdb_interval", local({
    g <<- mfdb_interval('l', c(10, 20, 100), open_ended = TRUE)
    ok(ut_cmp_identical(capture.output(pre_query(NULL, g, "out")), c(
        "NULL")), "Nothing happened pre_query")
    ok(ut_cmp_identical(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(ut_cmp_identical(select_clause(mdb, g, "col", "out"), "CASE WHEN col < CAST(20 AS REAL) THEN 'l10' WHEN col < CAST(100 AS REAL) THEN 'l20' ELSE 'l20' END AS out"), "Select clause")
    ok(ut_cmp_identical(from_clause(mdb, g, "col", "out"), c()), "From clause")
    ok(ut_cmp_identical(where_clause(mdb, g, "col", "out"), "col >= CAST(10 AS REAL)"), "Where clause")

    g <<- mfdb_interval('l', c(10, 20, 100), open_ended = c('upper'))
    ok(ut_cmp_identical(select_clause(mdb, g, "col", "out"), "CASE WHEN col < CAST(20 AS REAL) THEN 'l10' WHEN col < CAST(100 AS REAL) THEN 'l20' ELSE 'l20' END AS out"), "Select clause c('upper')")

    g <<- mfdb_interval('l', c(10, 20, 100), open_ended = c('lower'))
    ok(ut_cmp_identical(select_clause(mdb, g, "col", "out"), "CASE WHEN col < CAST(20 AS REAL) THEN 'l10' WHEN col < CAST(100 AS REAL) THEN 'l20' ELSE 'l20' END AS out"), "Select clause c('lower')")

    g <<- mfdb_interval('l', c(10, 20, 100), open_ended = c('lower', 'upper'))
    ok(ut_cmp_identical(select_clause(mdb, g, "col", "out"), "CASE WHEN col < CAST(20 AS REAL) THEN 'l10' WHEN col < CAST(100 AS REAL) THEN 'l20' ELSE 'l20' END AS out"), "Select clause c('lower', 'upper')")

}, asNamespace('mfdb')))
