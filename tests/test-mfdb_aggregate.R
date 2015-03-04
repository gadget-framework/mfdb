library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

mdb <- list(case_study_id = 5)

ok_group("Aggregates with NULL", local({
    pre_query(mdb, NULL, "col")  # Just check nothing happens
    ok(cmp(sample_clause(mdb, NULL, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, NULL, "col", "out"), "'all' AS out"), "Select clause")
    ok(cmp(from_clause(mdb, NULL, "col", "out"), c()), "From clause")
    ok(cmp(where_clause(mdb, NULL, "col", "out"), c()), "Where clause")
    ok(cmp(agg_summary(mdb, NULL, "col", "out", list(data.frame())), list(all = 'X')), "Agg summary")
}, asNamespace('mfdb')))

ok_group("Aggregates with numeric", local({
    pre_query(mdb, 5, "col")  # Just check nothing happens

    ok(cmp(sample_clause(mdb, 5, "col", "out"), "0"), "Sample clause")

    ok(cmp(select_clause(mdb, 5, 'col', 'out'), "col AS out"))

    ok(cmp(from_clause(mdb, 5, "col", "out"), c()), "From clause")

    ok(cmp(where_clause(mdb, 5, 'col'), "(col IN (5))"))
    ok(cmp(where_clause(mdb, c(1,2,3), 'col'), "(col IN (1,2,3))"))
    ok(cmp(where_clause(mdb, c(1,NA,3), 'cow'), "(cow IN (1,3) OR cow IS NULL)"))

    ok(cmp(agg_summary(mdb, 5, "col", "out", list(data.frame())), list("5" = 5)), "Agg summary (5)")
    ok(cmp(agg_summary(mdb, c(1,2,3), "col", "out", list(data.frame())), list("1" = 1, "2" = 2, "3" = 3)), "Agg summary (1,2,3)")
    ok(cmp(agg_summary(mdb, c(1,NA,3), "col", "out", list(data.frame())), list("1" = 1, "3" = 3)), "Agg summary (1,NA,3)")
}, asNamespace('mfdb')))

ok_group("Aggregates with character", local({
    pre_query(mdb, "c", "col")  # Just check nothing happens

    ok(cmp(sample_clause(mdb, "c", "col", "out"), "0"), "Sample clause")

    ok(cmp(select_clause(mdb, 'a', 'col', 'out'), "col AS out"))

    ok(cmp(from_clause(mdb, "c", "col", "out"), c()), "From clause")

    ok(cmp(where_clause(mdb, c("a", "b"), 'tbl.boar_id'), "(tbl.boar_id IN ('a','b'))"))

    ok(cmp(agg_summary(mdb, c("a", "b"), "col", "out", list(data.frame())), list("a" = "a", "b" = "b")), "Agg summary")
}, asNamespace('mfdb')))

ok_group("Aggregates with global taxonomies", local({
    pre_query(mdb, "c", "col")  # Just check nothing happens

    ok(cmp(sample_clause(mdb, "c", "tbl.gear_id", "out"), "0"), "Sample clause")

    ok(cmp(
        select_clause(mdb, 'a', 'tbl.gear_id', 'out'),
        "(SELECT name FROM gear WHERE gear_id = tbl.gear_id) AS out"))

    ok(cmp(from_clause(mdb, "c", "tbl.gear_id", "out"), c()), "From clause")

    ok(cmp(where_clause(mdb, c("GEA"), 'tbl.gear_id'),
        "(tbl.gear_id IN (SELECT gear_id FROM gear WHERE name IN ('GEA')))"))

    ok(cmp(agg_summary(mdb, c("GEA"), "tbl.gear_id", "gear", list(data.frame())), list("GEA" = "GEA")), "Agg summary")
}, asNamespace('mfdb')))

ok_group("Aggregates with CS-specific taxonomies", local({
    pre_query(mdb, "c", "col")  # Just check nothing happens

    ok(cmp(sample_clause(mdb, "c", "tbl.sampling_type_id", "out"), "0"), "Sample clause")

    ok(cmp(
        select_clause(mdb, 'a', 'tbl.sampling_type_id', 'out'),
        "(SELECT name FROM sampling_type WHERE case_study_id = 5 AND sampling_type_id = tbl.sampling_type_id) AS out"))

    ok(cmp(from_clause(mdb, "c", "tbl.sampling_type_id", "out"), c()), "From clause")

    ok(cmp(where_clause(mdb, c("SEA"), 'tbl.sampling_type_id'),
        "(tbl.sampling_type_id IN (SELECT sampling_type_id FROM sampling_type WHERE case_study_id = 5 AND name IN ('SEA')))"))

    ok(cmp(agg_summary(mdb, c("SEA"), "tbl.sampling_type_id", "sampling_type", list(data.frame())), list("SEA" = "SEA")), "Agg summary")
}, asNamespace('mfdb')))
