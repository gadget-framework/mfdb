library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

ok_group("sql_quote", {
    sql_quote <- mfdb:::sql_quote
    ok(cmp_error(sql_quote(c()), "empty"), "Empty vector results in error")

    ok(sql_quote("") == "''")
    ok(sql_quote("3") == "'3'")
    ok(sql_quote(4) == "4")
    ok(sql_quote("Greengrocer's") == "'Greengrocer''s'")

    ok(sql_quote("3", always_bracket = TRUE) == "('3')")
    ok(sql_quote(c(1,2), always_bracket = TRUE) == "(1,2)")
    ok(sql_quote(c(1,2), brackets = "[]") == "[1,2]")
   
    ok(sql_quote(c(1, 2, 3)) == "(1,2,3)")
    ok(sql_quote(c("a", "bee's", "c")) == "('a','bee''s','c')")
    ok(sql_quote(c(1, NA, 3)) == "(1,NULL,3)")

    ok(sql_quote(1, always_quote = TRUE) == "'1'")
    ok(sql_quote(1:5, always_quote = TRUE) == "('1','2','3','4','5')")
})

ok_group("sql_create_index", {
    ci <- mfdb:::sql_create_index
    ok(cmp(ci("tbl", "col"), "CREATE INDEX ON tbl (col)"))
    ok(cmp(ci("tbl", c("A", "B")), "CREATE INDEX ON tbl (A,B)"))
})
