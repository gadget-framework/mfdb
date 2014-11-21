library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

all_components <- c(
        "penalty",
        "understocking",
        "catchstatistics",
        "catchdistribution",
        "stockdistribution")

ok_group("Can generate gadget_likelihood_component objects", {
    expect_error(
        gadget_likelihood_component("aardvark"),
        "Unknown likelihood component aardvark")

    expect_equal(
        class(gadget_likelihood_component("penalty")),
        c("gadget_penalty_component", "gadget_likelihood_component"))
})

ok_group("Can use as.character on gadget_likelihood_components", {
    comp <- gadget_likelihood_component("penalty", name = "wibble", weight = 0.5)
    expect_equal(comp$name, "wibble")
    expect_equal(comp$weight, 0.5)
    expect_equal(comp$type, "penalty")
})

ok_group("Name, type and weight behave the same with all components", {
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

ok_group("Can write likelihood components", {
    # Create a temporary directory, starts off empty
    dir <- tempfile()
    gd <- gadget_directory(dir)
    expect_equal(list.files(dir), character(0))

    # Create some components
    gadget_dir_write(gd, gadget_likelihood_component("understocking", name="head-bone", weight = 0.3))
    expect_equal(list.files(dir), c("likelihood", "main"))
    expect_equal(
        gadget_dir_read(gd, "likelihood")$components,
        list(
            list(),
            component = list(name = "head-bone", weight = 0.3, type = "understocking")))
    ok(cmp_file(gd, "main",
        ver_string,
        "timefile\t",
        "areafile\t",
        "printfiles\t; Required comment",
        "[stock]",
        "[tagging]",
        "[otherfood]",
        "[fleet]",
        "[likelihood]",
        "likelihoodfiles\tlikelihood"))

    gadget_dir_write(gd, gadget_likelihood_component("penalty", name="neck-bone", weight = 0.5))
    expect_equal(list.files(dir), c("likelihood", "main", "neck-bone.penaltyfile"))
    expect_equal(
        gadget_dir_read(gd, "likelihood")$components,
        list(
            list(),
            component = list(name = "head-bone", weight = 0.3, type = "understocking"),
            component = list(name = "neck-bone", weight = 0.5, type = "penalty", datafile = "neck-bone.penaltyfile")))
    ok(cmp_file(gd, "main",
        ver_string,
        "timefile\t",
        "areafile\t",
        "printfiles\t; Required comment",
        "[stock]",
        "[tagging]",
        "[otherfood]",
        "[fleet]",
        "[likelihood]",
        "likelihoodfiles\tlikelihood"))

    # Override one, add another
    gadget_dir_write(gd, gadget_likelihood_component("understocking", name="head-bone", weight = 0.8))
    gadget_dir_write(gd, gadget_likelihood_component("understocking", name="shoulder-bone", weight = 0.2))
    expect_equal(
        gadget_dir_read(gd, "likelihood")$components,
        list(
            list(),
            component = list(name = "head-bone", weight = 0.8, type = "understocking"),
            component = list(name = "neck-bone", weight = 0.5, type = "penalty", datafile = "neck-bone.penaltyfile"),
            component = list(name = "shoulder-bone", weight = 0.2, type = "understocking")))
    ok(cmp_file(gd, "main",
        ver_string,
        "timefile\t",
        "areafile\t",
        "printfiles\t; Required comment",
        "[stock]",
        "[tagging]",
        "[otherfood]",
        "[fleet]",
        "[likelihood]",
        "likelihoodfiles\tlikelihood"))
})

###############################################################################
ok_group("Can generate an understocking component with default parameters", {
    comp <- gadget_likelihood_component("understocking")
    expect_equal(comp$name, "understocking")
    expect_equal(comp$weight, 0)
    expect_equal(comp$type, "understocking")
})

ok_group("Can customise it", {
    comp <- gadget_likelihood_component("understocking", name = "alfred", weight = 0.3)
    expect_equal(comp$name, "alfred")
    expect_equal(comp$weight, 0.3)
    expect_equal(comp$type, "understocking")
})

###############################################################################
ok_group("Function either provided explicitly or based on generator", {
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
