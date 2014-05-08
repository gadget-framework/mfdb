context("Gadget data files")

test_that("Can generate gadgefile objects", {
    expect_error(
        gadget_file(),
        "filename")

    expect_equal(
        class(gadget_file("wibble")),
        c("gadget_file"))
})

test_that("Can get a string representation", {
    expect_equal(
        strsplit(as.character(gadget_file("wobble", properties = list(
            cabbage = "yes",
            potatoes = "2 please",
            sprouts = 'Like, "Eeeew!"'))), "\n"),
        list(
            "cabbage\tyes",
            "potatoes\t2 please",
            'sprouts\tLike, "Eeeew!"'))
})
