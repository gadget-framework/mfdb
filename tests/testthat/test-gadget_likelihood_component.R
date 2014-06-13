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
    comp <- gadget_likelihood_component("penalty", name = "wibble", weight = 0.5)
    expect_equal(comp$name, "wibble")
    expect_equal(comp$weight, 0.5)
    expect_equal(comp$type, "penalty")
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
context("Gadget understocking component")

test_that("Can generate an understocking component with default parameters", {
    comp <- gadget_likelihood_component("understocking")
    expect_equal(comp$name, "understocking")
    expect_equal(comp$weight, 0)
    expect_equal(comp$type, "understocking")
})

test_that("Can customise it", {
    comp <- gadget_likelihood_component("understocking", name = "alfred", weight = 0.3)
    expect_equal(comp$name, "alfred")
    expect_equal(comp$weight, 0.3)
    expect_equal(comp$type, "understocking")
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

    expect_equal(
        gadget_likelihood_component("catchstatistics",
            data = data.frame(), data_function = "customfunction")[['function']],
        "customfunction")

    expect_equal(
        gadget_likelihood_component("catchstatistics",
            data = structure(data.frame(), generator = "mfdb_meanlength_stddev"))[['function']],
        "lengthgivenstddev")
})
