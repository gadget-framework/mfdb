library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

section("sql_quote", {
    sql_quote <- mfdb:::sql_quote
    ok(sql_quote("") == "''")
    ok(sql_quote("3") == "'3'")
    ok(sql_quote(4) == "4")
    ok(sql_quote("Greengrocer's") == "'Greengrocer''s'")

    ok(sql_quote("3", always_bracket = TRUE) == "('3')")
    ok(sql_quote(c(1,2), always_bracket = TRUE) == "(1,2)")
   
    ok(sql_quote(c(1, 2, 3)) == "(1,2,3)")
    ok(sql_quote(c("a", "bee's", "c")) == "('a','bee''s','c')")
    ok(sql_quote(c(1, NA, 3)) == "(1,NULL,3)")
})

section("select_clause", {
    sc <- mfdb:::select_clause.numeric
    ok(cmp(sc(5, 'col', 'out'), "col AS out"))

    sc <- mfdb:::select_clause.character
    ok(cmp(sc('a', 'col', 'out'), "col AS out"))
})

section("where_clause", {
    wc <- mfdb:::where_clause.numeric
    ok(cmp(wc(5, 'col'), "(col IN (5))"))
    ok(cmp(wc(c(1,2,3), 'col'), "(col IN (1,2,3))"))
    ok(cmp(wc(c(1,NA,3), 'cow'), "(cow IN (1,3) OR cow IS NULL)"))

    wc <- mfdb:::where_clause.character
    ok(cmp(wc(c("a", "b"), 'tbl.boar_id'), "(tbl.boar_id IN ('a','b'))"))
    ok(cmp(wc(c("GEA"), 'tbl.gear_id'), "(tbl.gear_id IN (SELECT gear_id FROM gear WHERE name IN ('GEA')))"))
})
