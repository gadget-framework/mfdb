context("Marefrane groups")

test_that("Can generate groupobjects", {
    expect_equal(
        class(mfdb_group("penalty")),
        c("mfdb_group"))
})

test_that("Can denormalise a group", {
    expect_equal(
        denormalize(
            mfdb_group("", c(1,2,3), c(88))),
        list(c("1","1"), c("1","2"), c("1","3"), c("2","88")))

    expect_equal(
        denormalize(
            mfdb_group("a", c(1,2,3), badger = c(88, 21), c(99))),
        list(c("a1","1"), c("a1","2"), c("a1","3"), c("badger","88"), c("badger","21"), c("a3","99")))
})
