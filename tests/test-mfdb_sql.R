library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

section("sql_quote", function() {
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

section("sql_col_condition", function() {
    ok(mfdb:::sql_col_condition('col', 5) == "(col IN (5))")
    ok(mfdb:::sql_col_condition('col', c(1,2,3)) == "(col IN (1,2,3))")
    ok(mfdb:::sql_col_condition('cow', c(1,NA,3)) == "(cow IN (1,3) OR cow IS NULL)")
    ok(mfdb:::sql_col_condition('tbl.gear_id', c("GEA"), lookup = "gear") == "(tbl.gear_id IN (SELECT gear_id FROM gear WHERE name IN ('GEA')))")
})
