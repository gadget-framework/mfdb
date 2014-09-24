library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

all_components <- c(
        "penalty",
        "understocking",
        "catchstatistics",
        "catchdistribution",
        "stockdistribution")

section("Can generate gadget_likelihood_component objects", function() {
    expect_error(
        gadget_likelihood_component("aardvark"),
        "Unknown likelihood component aardvark")

    expect_equal(
        class(gadget_likelihood_component("penalty")),
        c("gadget_penalty_component", "gadget_likelihood_component"))
})

section("Can use as.character on gadget_likelihood_components", function() {
    comp <- gadget_likelihood_component("penalty", name = "wibble", weight = 0.5)
    expect_equal(comp$name, "wibble")
    expect_equal(comp$weight, 0.5)
    expect_equal(comp$type, "penalty")
})

section("Name, type and weight behave the same with all components", function() {
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
        expect_equal(c$type, type)
        expect_equal(c$name, type)
        # Weight defaults to 0
        expect_equal(c$weight, 0)

        # Can customise name & weight
        if (type == "catchstatistics") {
            c <- gadget_likelihood_component(type,
                name = "gerald", weight = 0.542,
                data = data.frame(), data_function = "customfunction")
        } else {
            c <- gadget_likelihood_component(type, name = "gerald", weight = 0.542)
        }
        expect_equal(c$name, "gerald")
        expect_equal(c$weight, 0.542)
    }
})

###############################################################################
section("Can generate an understocking component with default parameters", function() {
    comp <- gadget_likelihood_component("understocking")
    expect_equal(comp$name, "understocking")
    expect_equal(comp$weight, 0)
    expect_equal(comp$type, "understocking")
})

section("Can customise it", function() {
    comp <- gadget_likelihood_component("understocking", name = "alfred", weight = 0.3)
    expect_equal(comp$name, "alfred")
    expect_equal(comp$weight, 0.3)
    expect_equal(comp$type, "understocking")
})

###############################################################################
section("Function either provided explicitly or based on generator", function() {
    expect_error(
        gadget_likelihood_component("catchstatistics"),
        "No data provided")
    expect_error(
        gadget_likelihood_component("catchstatistics", data = data.frame()),
        "Cannot work out the required function, and data_function not provided")
    expect_error(
        gadget_likelihood_component("catchstatistics", data = structure(data.frame(), generator = "camel")),
        "Unknown generator function camel")

    expect_equal(
        gadget_likelihood_component("catchstatistics",
            data = data.frame(), data_function = "customfunction")[['function']],
        "customfunction")

    expect_equal(
        gadget_likelihood_component("catchstatistics",
            data = structure(data.frame(), generator = "mfdb_meanlength_stddev"))[['function']],
        "lengthgivenstddev")
})
