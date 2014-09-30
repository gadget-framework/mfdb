library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

section("sql_quote", function() {
    sql_quote <- mfdb:::sql_quote
    ok(sql_quote("") == "''")
    ok(sql_quote("3") == "'3'")
    ok(sql_quote("Greengrocer's") == "'Greengrocer''s'")
   
    ok(sql_quote(c(1, 2, 3)) == "('1','2','3')")
    ok(sql_quote(c("a", "bee's", "c")) == "('a','bee''s','c')")
    ok(sql_quote(c(1, NA, 3)) == "('1',NULL,'3')")
})
