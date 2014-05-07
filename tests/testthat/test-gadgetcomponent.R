context("Gadget components")

test_that("Can generate gadgetcomponent objects", {
    expect_error(
        gadgetcomponent("aardvark"),
        "Unknown likelihood component aardvark")

    expect_equal(
        class(gadgetcomponent("penalty")),
        c("gadgetpenaltycomponent", "gadgetcomponent"))
})

test_that("Can use as.character on gadgetcomponents", {
    expect_equal(
        strsplit(as.character(gadgetcomponent("penalty", name = "wibble", weight = 0.5)), "\n"),
        list(
            "[component]",
            "type\tpenalty",
            "name\twibble",
            "datafile\twibble.penaltyfile",
            "weight\t0.5"))
})
