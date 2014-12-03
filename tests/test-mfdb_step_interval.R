library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

g <- NULL
mdb <- NULL

ok_group("Can generate objects", {
    expect_equal(
        class(mfdb_step_interval("l", 5)),
        c("mfdb_step_interval", "mfdb_aggregate"))
})

ok_group("Can convert mfdb_step_intervals into lists", {
    g <<- mfdb_step_interval('l', 10)
    ok(cmp(
        as.list(mfdb_step_interval('l', 10, from = 10, to = 50)),
        list(l10 = c(10, 20), l20 = c(20, 30), l30 = c(30, 40), l40 = c(40, 50))),
        "Can convert closed interval to list")

    ok(cmp_error(as.list(mfdb_step_interval('l', 10)), "to"), "Must provide 'to' for now")
})

ok_group("Aggregates with close_ended mfdb_step_interval", local({
    g <<- mfdb_step_interval('l', 10)
    ok(cmp(capture.output(pre_query(NULL, g, "out")), c(
        "NULL")), "Nothing happened pre_query")
    ok(cmp(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), "'l' || (greatest(floor(col)::integer, 0) / 10) * 10 AS out"), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), c()), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), "col >= 0"), "Where clause")

    g <<- mfdb_step_interval('l', 5, from = 10, to = 90)
    ok(cmp(capture.output(pre_query(NULL, g, "out")), c(
        "NULL")), "Nothing happened pre_query")
    ok(cmp(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), "'l' || (least(greatest(floor(col)::integer, 10), 90) / 5) * 5 AS out"), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), c()), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), c("col >= 10", "col < 90")), "Where clause")
}, asNamespace('mfdb')))

ok_group("Aggregates with open_ended mfdb_step_interval", local({
    g <<- mfdb_step_interval('l', 10, open_ended = TRUE)
    ok(cmp(capture.output(pre_query(NULL, g, "out")), c(
        "NULL")), "Nothing happened pre_query")
    ok(cmp(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), "'l' || (greatest(floor(col)::integer, 0) / 10) * 10 AS out"), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), c()), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), "col >= 0"), "Where clause")

    g <<- mfdb_step_interval('l', 5, from = 10, to = 90, open_ended = TRUE)
    ok(cmp(capture.output(pre_query(NULL, g, "out")), c(
        "NULL")), "Nothing happened pre_query")
    ok(cmp(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), "'l' || (least(greatest(floor(col)::integer, 10), 90) / 5) * 5 AS out"), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), c()), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), c("col >= 10")), "Where clause")
}, asNamespace('mfdb')))
