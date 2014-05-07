context("Marefrane groups")

test_that("Can generate groupobjects", {
    expect_equal(
        class(mfdb_group("penalty")),
        c("mfdb_group"))
})

test_that("Can denormalise a group", {
    expect_equal(
        denormalize(
            mfdb_group(c(1,2,3), c(88))),
        c("1=1", "1=2", "1=3", "2=88"))

    expect_equal(
        denormalize(
            mfdb_group(c(1,2,3), c(88)), collapse = " - "),
        c("1 - 1", "1 - 2", "1 - 3", "2 - 88"))

    expect_equal(
        denormalize(
            mfdb_group(c(1,2,3), badger = c(88, 21), c(99)), prefix="a"),
        c("a1=1", "a1=2", "a1=3", "badger=88", "badger=21", "a3=99"))
})
