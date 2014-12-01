library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

g <- NULL

ok_group("Aggregates with mfdb_unaggregated(omitNA = FALSE)", local({
    g <<- mfdb_unaggregated()

    pre_query("mdb", g, "out")  # Just check nothing happens
    ok(cmp(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), "col AS out"), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), c()), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), c()), "Where clause")
}, asNamespace('mfdb')))

ok_group("Aggregates with mfdb_unaggregated(omitNA = TRUE)", local({
    g <<- mfdb_unaggregated(omitNA = TRUE)

    pre_query("mdb", g, "out")  # Just check nothing happens
    ok(cmp(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), "col AS out"), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), c()), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), "col IS NOT NULL"), "Where clause")
}, asNamespace('mfdb')))
