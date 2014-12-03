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
            component = structure(list(name = "head-bone", weight = 0.3, type = "understocking"), preamble = list(""))))
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
    ok(cmp(
        list.files(dir, recursive = TRUE),
        c("Data/neck-bone.penaltyfile", "likelihood", "main")), "Created penaltyfile")
    expect_equal(
        gadget_dir_read(gd, "likelihood")$components,
        list(
            list(),
            component = structure(list(name = "head-bone", weight = 0.3, type = "understocking"), preamble = list("")),
            component = structure(list(name = "neck-bone", weight = 0.5, type = "penalty", datafile = "Data/neck-bone.penaltyfile"), preamble = list(""))))
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
            component = structure(list(name = "head-bone", weight = 0.8, type = "understocking"), preamble = list("")),
            component = structure(list(name = "neck-bone", weight = 0.5, type = "penalty", datafile = "Data/neck-bone.penaltyfile"), preamble = list("")),
            component = structure(list(name = "shoulder-bone", weight = 0.2, type = "understocking"), preamble = list(""))))
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
            data = structure(data.frame(), generator = "mfdb_sample_meanlength_stddev"))[['function']],
        "lengthgivenstddev")
})

###############################################################################
ok_group("Aggregation files", {
    cmp_agg <- function (agg_type, agg, ...) {
        gd <- gadget_directory(tempfile())
        gadget_dir_write(gd, gadget_likelihood_component(
            "catchdistribution",
            name="cd",
            weight = 0.8,
            data = structure(
                data.frame(a = c(1,2,3), b = c(4,5,6)),
                area = if (agg_type == 'area') agg else NULL,
                age = if (agg_type == 'age') agg else NULL,
                length = if (agg_type == 'len') agg else NULL,
                generator = "mfdb_sample_meanlength")
            ))
        do.call(cmp_file, c(
            list(gd, paste0("Aggfiles/catchdistribution.cd.", agg_type, ".agg")),
            list(...)))
    }

    ok(cmp_agg('area', list(divA = c('x', 't'), divB = c('s', 'r')),
        ver_string,
        "divA\t1",
        "divB\t2",
        NULL), "Area aggregation file has subdivisions hidden")

    ok(cmp_agg('age', list(young = 1:4, old = 5:8),
        ver_string,
        "young\t1\t2\t3\t4",
        "old\t5\t6\t7\t8",
        NULL), "Age aggregation matches input")

    ok(cmp_agg('age', 1:4,
        ver_string,
        "1\t1",
        "2\t2",
        "3\t3",
        "4\t4",
        NULL), "1:4 converted into 1 group for each")

    ok(cmp_agg('age', NULL,
        ver_string,
        "all\tX",
        NULL), "Make our best guess at an 'all' aggregation")

    ok(cmp_agg('age', mfdb_unaggregated(),
        ver_string,
        "X\tX",
        NULL), "mfdb_unaggregated")

    ok(cmp_agg('len', mfdb_interval("len", seq(0, 50, by = 10)),
        ver_string,
        "len0\t0\t10",
        "len10\t10\t20",
        "len20\t20\t30",
        "len30\t30\t40",
        "len40\t40\t50",
        NULL), "mfdb_interval")

    ok(cmp_agg('len', mfdb_step_interval("len", to = 30, by = 5),
        ver_string,
        "len0\t0\t5",
        "len5\t5\t10",
        "len10\t10\t15",
        "len15\t15\t20",
        "len20\t20\t25",
        "len25\t25\t30",
        NULL), "mfdb_step_interval")

})
