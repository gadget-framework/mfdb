context("Gadget components")

test_that("Can generate gadget_likelihood_component objects", {
    expect_error(
        gadget_likelihood_component("aardvark"),
        "Unknown likelihood component aardvark")

    expect_equal(
        class(gadget_likelihood_component("penalty")),
        c("gadgetpenaltycomponent", "gadget_likelihood_component"))
})

test_that("Can use as.character on gadget_likelihood_components", {
    expect_equal(
        strsplit(as.character(gadget_likelihood_component("penalty", name = "wibble", weight = 0.5)), "\n"),
        list(
            "[component]",
            "type\tpenalty",
            "name\twibble",
            "datafile\twibble.penaltyfile",
            "weight\t0.5"))
})
