library(testthat)

context("Mareframe intervals")

test_that("Can generate objects", {
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

test_that("Elements are named by prefixes", {
    expect_equal(
        as.list(mfdb_interval("l", c(10,20,25,30))),
        list(l10 = c(10,20), l20 = c(20,25), l25 = c(25,30)))
    expect_equal(
        as.list(mfdb_interval("l", c(5,15,25,30))),
        list(l5 = c(5,15), l15 = c(15,25), l25 = c(25,30)))
})
