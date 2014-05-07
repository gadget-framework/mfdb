context("Gadget data files")

test_that("Can generate gadgefile objects", {
    expect_error(
        gadgetfile(),
        "filename")

    expect_equal(
        class(gadgetfile("wibble")),
        c("gadgetfile"))
})

test_that("Can get a string representation", {
    expect_equal(
        strsplit(as.character(gadgetfile("wobble", properties = list(
            cabbage = "yes",
            potatoes = "2 please",
            sprouts = 'Like, "Eeeew!"'))), "\n"),
        list(
            "cabbage\tyes",
            "potatoes\t2 please",
            'sprouts\tLike, "Eeeew!"'))
})
