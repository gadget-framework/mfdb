context("Gadget directories")

test_that("Will create a directory when creating gadget_directories", {
    expect_error(
        gadget_directory("/dont-run-as-root"),
        "/dont-run-as-root")

    dir <- tempfile()
    expect_true(!file.exists(dir))
    gd <- gadget_directory(dir)
    expect_true(file.exists(dir) && file_test("-d", dir))
})
