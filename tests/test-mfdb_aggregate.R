library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

ok_group("Aggregates with NULL", {
    mfdb:::pre_query("mdb", NULL, "out")  # Just check nothing happens
    ok(cmp(mfdb:::sample_clause(NULL, "col", "out"), "0"), "Sample clause")
    ok(cmp(mfdb:::select_clause(NULL, "col", "out"), "'all' AS out"), "Select clause")
    ok(cmp(mfdb:::from_clause(NULL, "col", "out"), c()), "From clause")
    ok(cmp(mfdb:::where_clause(NULL, "col", "out"), c()), "Where clause")
})

ok_group("Aggregates with numeric", {
    mfdb:::pre_query.numeric("mdb", 5, "out")  # Just check nothing happens

    ok(cmp(mfdb:::sample_clause.numeric(5, "col", "out"), "0"), "Sample clause")

    sc <- mfdb:::select_clause.numeric
    ok(cmp(sc(5, 'col', 'out'), "col AS out"))

    ok(cmp(mfdb:::from_clause.numeric(5, "col", "out"), c()), "From clause")

    wc <- mfdb:::where_clause.numeric
    ok(cmp(wc(5, 'col'), "(col IN (5))"))
    ok(cmp(wc(c(1,2,3), 'col'), "(col IN (1,2,3))"))
    ok(cmp(wc(c(1,NA,3), 'cow'), "(cow IN (1,3) OR cow IS NULL)"))
})

ok_group("Aggregates with character", {
    mfdb:::pre_query.character("mdb", "c", "out")  # Just check nothing happens

    ok(cmp(mfdb:::sample_clause.character("c", "col", "out"), "0"), "Sample clause")

    sc <- mfdb:::select_clause.character
    ok(cmp(sc('a', 'col', 'out'), "col AS out"))

    ok(cmp(mfdb:::from_clause.character("c", "col", "out"), c()), "From clause")

    wc <- mfdb:::where_clause.character
    ok(cmp(wc(c("a", "b"), 'tbl.boar_id'), "(tbl.boar_id IN ('a','b'))"))
    ok(cmp(wc(c("GEA"), 'tbl.gear_id'), "(tbl.gear_id IN (SELECT gear_id FROM gear WHERE name IN ('GEA')))"))
})
