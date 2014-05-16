context("Marefrane groups")

test_that("Can generate groupobjects", {
    expect_equal(
        class(mfdb_group("penalty")),
        c("mfdb_group"))
})

test_that("Can denormalise a group", {
    expect_equal(
        denormalize(
            mfdb_group("", c(1,"two",3), c(88))),
        list(c("0","1","1"), c("0","1","two"), c("0","1","3"), c("0","2","88")))

    expect_equal(
        denormalize(
            mfdb_group("a", c(1,2,3), badger = c(88, 21), c(99))),
        list(c("0","a1","1"), c("0","a1","2"), c("0","a1","3"), c("0","badger","88"), c("0","badger","21"), c("0","a3","99")))
})

test_that("Can get a bootstrap sample", {
    # Single values just come back the same, but you get 3 "samples"
    expect_equal(
        denormalize(
            mfdb_group("g", c(44), c(88)), bootstrap = 2),
        list(c("1","g1","44"), c("1","g2","88"), c("2","g1","44"), c("2","g2","88")))

    dn <- denormalize(mfdb_group("g", c(44, 55), c(88, 99)), bootstrap = 2)
    for(i in 1:4) {
        expect_equal(dn[[i]][[1]], "1")
        expect_equal(dn[[i+4]][[1]], "2")
    }
    for(i in 1:2) {
        expect_true(dn[[i]][[3]] %in% c("44", "55"))
        expect_true(dn[[i+2]][[3]] %in% c("88", "99"))
        expect_true(dn[[i+4]][[3]] %in% c("44", "55"))
        expect_true(dn[[i+6]][[3]] %in% c("88", "99"))
    }
})
