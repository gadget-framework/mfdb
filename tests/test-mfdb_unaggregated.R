library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

ok_group("Aggregates with mfdb_unaggregated(omitNA = FALSE)", executeInNamespace('mfdb', {
    g <- mfdb_unaggregated()

    pre_query("mdb", g, "out")  # Just check nothing happens
    ok(cmp(sample_clause(g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(g, "col", "out"), "col AS out"), "Select clause")
    ok(cmp(from_clause(g, "col", "out"), c()), "From clause")
    ok(cmp(where_clause(g, "col", "out"), c()), "Where clause")
}))

ok_group("Aggregates with mfdb_unaggregated(omitNA = TRUE)", executeInNamespace('mfdb', {
    g <- mfdb_unaggregated(omitNA = TRUE)

    pre_query("mdb", g, "out")  # Just check nothing happens
    ok(cmp(sample_clause(g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(g, "col", "out"), "col AS out"), "Select clause")
    ok(cmp(from_clause(g, "col", "out"), c()), "From clause")
    ok(cmp(where_clause(g, "col", "out"), "col IS NOT NULL"), "Where clause")
}))
