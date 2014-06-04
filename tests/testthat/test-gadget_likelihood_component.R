expect_contains <- function (actual, substr) {
    expect_true(TRUE %in% grepl(substr, actual))
}

###############################################################################
context("Gadget components")

all_components <- c(
        "penalty",
        "understocking",
        "catchstatistics",
        "catchdistribution",
        "stockdistribution")

test_that("Can generate gadget_likelihood_component objects", {
    expect_error(
        gadget_likelihood_component("aardvark"),
        "Unknown likelihood component aardvark")

    expect_equal(
        class(gadget_likelihood_component("penalty")),
        c("gadget_penalty_component", "gadget_likelihood_component"))
})

test_that("Can use as.character on gadget_likelihood_components", {
    expect_equal(
        strsplit(as.character(gadget_likelihood_component("penalty", name = "wibble", weight = 0.5)), "\n"),
        list(
            ";",
            ";",
            "[component]",
            "name\twibble",
            "weight\t0.5",
            "type\tpenalty",
            "datafile\twibble.penaltyfile"))
})

test_that("Name, type and weight behave the same with all components", {
    for (type in all_components) {
        if (type == "catchstatistics") {
            c <- gadget_likelihood_component(type,
                data = data.frame(), data_function = "customfunction")
        } else {
            c <- gadget_likelihood_component(type)
        }

        # Class should match type
        expect_equal(class(c), c(
            paste0("gadget_", type, "_component"),
            "gadget_likelihood_component"))

        # Type and name should match the name we gave
        expect_contains(c, paste0("type\t", type))
        expect_contains(c, paste0("name\t", type))
        # Weight defaults to 0
        expect_contains(c, "weight\t0$")

        # Can customise name & weight
        if (type == "catchstatistics") {
            c <- gadget_likelihood_component(type,
                name = "gerald", weight = 0.542,
                data = data.frame(), data_function = "customfunction")
        } else {
            c <- gadget_likelihood_component(type, name = "gerald", weight = 0.542)
        }
        expect_contains(c, paste0("name\tgerald$"))
        expect_contains(c, "weight\t0.542$")
    }
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
            "name\tunderstocking",
            "weight\t0",
            "type\tunderstocking"))
})

test_that("Can customise it", {
    expect_equal(
        strsplit(as.character(gadget_likelihood_component("understocking", name = "alfred", weight = 0.3)), "\n"),
        list(
            ";",
            ";",
            "[component]",
            "name\talfred",
            "weight\t0.3",
            "type\tunderstocking"))
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
