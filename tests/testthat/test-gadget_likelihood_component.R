expect_contains <- function (actual, substr) {
    expect_true(TRUE %in% grepl(substr, actual))
}

###############################################################################
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
            ";",
            ";",
            "[component]",
            "type\tpenalty",
            "name\twibble",
            "datafile\twibble.penaltyfile",
            "weight\t0.5"))
})

###############################################################################
context("Gadget understocking component")

test_that("Can generate an understocking component with default parameters", {
    expect_equal(
        strsplit(as.character(gadget_likelihood_component("understocking")), "\n"),
        list(
            ";",
            ";",
            "[component]",
            "type\tunderstocking",
            "name\tunderstocking",
            "weight\t0"))
})

test_that("Can customise it", {
    expect_equal(
        strsplit(as.character(gadget_likelihood_component("understocking", name = "alfred", weight = 0.3)), "\n"),
        list(
            ";",
            ";",
            "[component]",
            "type\tunderstocking",
            "name\talfred",
            "weight\t0.3"))
})

###############################################################################
context("Gadget catchstatistics component")

test_that("Function either provided explicitly or based on generator", {
    expect_error(
        gadget_likelihood_component("catchstatistics"),
        "No data provided")
    expect_error(
        gadget_likelihood_component("catchstatistics", data = data.frame()),
        "Cannot work out the required function, and data_function not provided")
    expect_error(
        gadget_likelihood_component("catchstatistics", data = structure(data.frame(), generator = "camel")),
        "Unknown generator function camel")

    expect_contains(
        gadget_likelihood_component("catchstatistics",
            data = data.frame(), data_function = "customfunction"),
        "function\tcustomfunction")

    expect_contains(
        gadget_likelihood_component("catchstatistics",
            data = structure(data.frame(), generator = "mfdb_meanlength_stddev")),
        "function\tlengthgivenstddev")
})
