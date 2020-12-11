library(mfdb)
library(unittest, quietly = TRUE)
helpers <- c('utils/helpers.R', 'tests/utils/helpers.R') ; source(helpers[file.exists(helpers)])

g <- NULL
mdb <- NULL

ok_group("Can generate objects", {
    expect_equal(
        class(mfdb_step_interval("l", 5)),
        c("mfdb_step_interval", "mfdb_aggregate"))

    ok(cmp_error(
        mfdb_step_interval('l', 0.5),
        "intervals"), "Steps must be integer")
})

ok_group("Can convert mfdb_step_intervals into lists", local({
    g <<- mfdb_step_interval('l', 10)
    ok(ut_cmp_identical(
        agg_summary(mdb, mfdb_step_interval('l', 10, from = 10, to = 50), 'c.len', 'len', data.frame(), 0),
        list(
            l10 = structure(call("seq", 10, 19), min = 10, max = 20),
            l20 = structure(call("seq", 20, 29), min = 20, max = 30),
            l30 = structure(call("seq", 30, 39), min = 30, max = 40),
            l40 = structure(call("seq", 40, 49), min = 40, max = 50))),
        "Can convert hard-coded interval to list")

    ok(cmp_error(
        agg_summary(mdb, mfdb_step_interval('l', 10), 'c.claire', 'claire', data.frame(), 0),
        "claire"), "Can't convert unbounded interval without data")

    ok(ut_cmp_identical(
        agg_summary(mdb, mfdb_step_interval('l', 10), 'c.len', 'len', data.frame(len = c('l0', 'l20', 'l30', 'l20')), 0),
        list(
            l0  = structure(call("seq",  0,  9), min =  0, max = 10),
            l10 = structure(call("seq", 10, 19), min = 10, max = 20),
            l20 = structure(call("seq", 20, 29), min = 20, max = 30),
            l30 = structure(call("seq", 30, 39), min = 30, max = 40))),
        "Can convert unbounded interval using returned data (max: 30)")
    ok(ut_cmp_identical(
        agg_summary(mdb, mfdb_step_interval('l', 10), 'c.len', 'len', data.frame(len = c('l0', 'l20', 'l30', 'l40')), 0),
        list(
            l0  = structure(call("seq",  0,  9), min =  0, max = 10),
            l10 = structure(call("seq", 10, 19), min = 10, max = 20),
            l20 = structure(call("seq", 20, 29), min = 20, max = 30),
            l30 = structure(call("seq", 30, 39), min = 30, max = 40),
            l40 = structure(call("seq", 40, 49), min = 40, max = 50))),
        "Can convert unbounded interval using returned data (max: 40)")

    ok(ut_cmp_identical(
        agg_summary(mdb, mfdb_step_interval('l', 10, from = 10, to = 50, open_ended = c('upper')), 'c.len', 'len', data.frame(), 0),
        list(
            l10 = structure(call("seq", 10, 19), min = 10, max = 20),
            l20 = structure(call("seq", 20, 29), min = 20, max = 30),
            l30 = structure(call("seq", 30, 39), min = 30, max = 40),
            l40 = structure(call("seq", 40, 49), min = 40, max = 50))),
        "Being defined as open_ended=upper makes no difference to the structure of the group")
}, asNamespace('mfdb')))

ok_group("Aggregates with close_ended mfdb_step_interval", local({
    g <<- mfdb_step_interval('l', 10)
    ok(ut_cmp_identical(capture.output(pre_query(NULL, g, "col")), c(
        "NULL")), "Nothing happened pre_query")
    ok(ut_cmp_identical(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(ut_cmp_identical(select_clause(mdb, g, "col", "out"), "'l' || (greatest(floor(col)::integer, 0) / 10) * 10 AS out"), "Select clause")
    ok(ut_cmp_identical(from_clause(mdb, g, "col", "out"), c()), "From clause")
    ok(ut_cmp_identical(where_clause(mdb, g, "col", "out"), "col >= 0"), "Where clause")

    g <<- mfdb_step_interval('l', 5, from = 10, to = 90)
    ok(ut_cmp_identical(capture.output(pre_query(NULL, g, "col")), c(
        "NULL")), "Nothing happened pre_query")
    ok(ut_cmp_identical(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(ut_cmp_identical(select_clause(mdb, g, "col", "out"), "'l' || (least(greatest(floor(col)::integer, 10), 85) / 5) * 5 AS out"), "Select clause")
    ok(ut_cmp_identical(from_clause(mdb, g, "col", "out"), c()), "From clause")
    ok(ut_cmp_identical(where_clause(mdb, g, "col", "out"), c("col >= 10", "col < 90")), "Where clause")
}, asNamespace('mfdb')))

ok_group("Aggregates with open_ended mfdb_step_interval", local({
    g <<- mfdb_step_interval('l', 10, open_ended = TRUE)
    ok(ut_cmp_identical(capture.output(pre_query(NULL, g, "col")), c(
        "NULL")), "Nothing happened pre_query")
    ok(ut_cmp_identical(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(ut_cmp_identical(select_clause(mdb, g, "col", "out"), "'l' || (greatest(floor(col)::integer, 0) / 10) * 10 AS out"), "Select clause")
    ok(ut_cmp_identical(from_clause(mdb, g, "col", "out"), c()), "From clause")
    ok(ut_cmp_identical(where_clause(mdb, g, "col", "out"), "col >= 0"), "Where clause")

    g <<- mfdb_step_interval('l', 5, from = 10, to = 90, open_ended = TRUE)
    ok(ut_cmp_identical(capture.output(pre_query(NULL, g, "col")), c(
        "NULL")), "Nothing happened pre_query")
    ok(ut_cmp_identical(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(ut_cmp_identical(select_clause(mdb, g, "col", "out"), "'l' || (least(greatest(floor(col)::integer, 10), 85) / 5) * 5 AS out"), "Select clause")
    ok(ut_cmp_identical(from_clause(mdb, g, "col", "out"), c()), "From clause")
    ok(ut_cmp_identical(where_clause(mdb, g, "col", "out"), c("col >= 10")), "Where clause")

    g <<- mfdb_step_interval('l', 5, from = 10, to = 90, open_ended = c('upper'))
    ok(ut_cmp_identical(where_clause(mdb, g, "col", "out"), c("col >= 10")), "Where clause c('upper')")

    g <<- mfdb_step_interval('l', 5, from = 10, to = 90, open_ended = c('lower'))
    ok(ut_cmp_identical(where_clause(mdb, g, "col", "out"), c("col < 90")), "Where clause c('lower')")

    g <<- mfdb_step_interval('l', 5, from = 10, to = 90, open_ended = c('upper', 'lower'))
    ok(ut_cmp_identical(where_clause(mdb, g, "col", "out"), c()), "Where clause c('upper', 'lower')")
}, asNamespace('mfdb')))
