library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

mdb <- list(case_study_id = 5)

ok_group("Aggregates with NULL", local({
    pre_query(mdb, NULL, "out")  # Just check nothing happens
    ok(cmp(sample_clause(mdb, NULL, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, NULL, "col", "out"), "'all' AS out"), "Select clause")
    ok(cmp(from_clause(mdb, NULL, "col", "out"), c()), "From clause")
    ok(cmp(where_clause(mdb, NULL, "col", "out"), c()), "Where clause")
}, asNamespace('mfdb')))

ok_group("Aggregates with numeric", local({
    pre_query(mdb, 5, "out")  # Just check nothing happens

    ok(cmp(sample_clause(mdb, 5, "col", "out"), "0"), "Sample clause")

    ok(cmp(select_clause(mdb, 5, 'col', 'out'), "col AS out"))

    ok(cmp(from_clause(mdb, 5, "col", "out"), c()), "From clause")

    ok(cmp(where_clause(mdb, 5, 'col'), "(col IN (5))"))
    ok(cmp(where_clause(mdb, c(1,2,3), 'col'), "(col IN (1,2,3))"))
    ok(cmp(where_clause(mdb, c(1,NA,3), 'cow'), "(cow IN (1,3) OR cow IS NULL)"))
}, asNamespace('mfdb')))

ok_group("Aggregates with character", local({
    pre_query(mdb, "c", "out")  # Just check nothing happens

    ok(cmp(sample_clause(mdb, "c", "col", "out"), "0"), "Sample clause")

    ok(cmp(select_clause(mdb, 'a', 'col', 'out'), "col AS out"))

    ok(cmp(from_clause(mdb, "c", "col", "out"), c()), "From clause")

    ok(cmp(where_clause(mdb, c("a", "b"), 'tbl.boar_id'), "(tbl.boar_id IN ('a','b'))"))
}, asNamespace('mfdb')))

ok_group("Aggregates with lookup tables", local({
    ok(cmp(where_clause(mdb, c("GEA"), 'tbl.gear_id'),
        "(tbl.gear_id IN (SELECT gear_id FROM gear WHERE name IN ('GEA')))"))
    ok(cmp(where_clause(mdb, c("SEA"), 'tbl.sampling_type_id'),
        "(tbl.sampling_type_id IN (SELECT sampling_type_id FROM sampling_type WHERE case_study_id = 5 AND name IN ('SEA')))"))
}, asNamespace('mfdb')))
