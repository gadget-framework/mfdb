library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

section("Can generate objects", {
    expect_error(
        mfdb_interval("l", c()),
        "vect must at least be 2 items long")
    expect_error(
        mfdb_interval("l", c(1)),
        "vect must at least be 2 items long")

    expect_equal(
        class(mfdb_interval("l", c(1, 100))),
        c("mfdb_interval"))
})

section("Elements are named by prefixes", {
    expect_equal(
        as.list(mfdb_interval("l", c(10,20,25,30))),
        list(l10 = c(10,20), l20 = c(20,25), l25 = c(25,30)))
    expect_equal(
        as.list(mfdb_interval("l", c(5,15,25,30))),
        list(l5 = c(5,15), l15 = c(15,25), l25 = c(25,30)))
})

section("Can generate SQL", {
    ok(cmp(
            mfdb:::select_clause.mfdb_interval(mfdb_interval("l", c(10,20,30)), "col", "outname"),
            "CASE WHEN col >= 30 THEN NULL WHEN col >= 20 THEN 'l20' WHEN col >= 10 THEN 'l10' END AS outname"),
        "Generated select clause")

    ok(cmp(
            mfdb:::where_clause.mfdb_interval(mfdb_interval("l", c(10,20,25,30)), "col"),
            c("col >= 10", "col < 30")),
        "Generated where clause")
    ok(cmp(
            mfdb:::where_clause.mfdb_interval(mfdb_interval("l", c(25,50)), "col"),
            c("col >= 25", "col < 50")),
        "Generated where clause")
})
