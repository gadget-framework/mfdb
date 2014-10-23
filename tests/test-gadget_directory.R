library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

section("Will create a directory when creating gadget_directories", {
    expect_error(
        gadget_directory("/dont-run-as-root"),
        "/dont-run-as-root")

    dir <- tempfile()
    expect_true(!file.exists(dir))
    gd <- gadget_directory(dir)
    expect_true(file.exists(dir) && file_test("-d", dir))
})

section("Can write files", {
    # Create a temporary directory, starts off empty
    dir <- tempfile()
    gd <- gadget_directory(dir)
    expect_equal(list.files(dir), character(0))

    # Write out files, come back the same
    gf_animals <- gadget_file("animals", components = list(list(cow = "Daisy", pig = "George")))
    gadget_dir_write(gd, gf_animals)
    expect_equal(gadget_dir_read(gd, "animals"), gf_animals)
    expect_equal(list.files(dir), c("animals"))

    gf_plants <- gadget_file("plants", components = list(list(tree = "The larch", tree = "The pine")))
    gadget_dir_write(gd, gf_plants)
    expect_equal(gadget_dir_read(gd, "plants"), gf_plants)
    expect_equal(list.files(dir), c("animals", "plants"))
})

section("Can write likelihood components", {
    # Create a temporary directory, starts off empty
    dir <- tempfile()
    gd <- gadget_directory(dir)
    expect_equal(list.files(dir), character(0))

    # Create some components
    gadget_dir_write(gd, gadget_likelihood_component("understocking", name="head-bone", weight = 0.3))
    expect_equal(list.files(dir), c("likelihood"))
    expect_equal(
        gadget_dir_read(gd, "likelihood")$components,
        list(
            list(),
            component = list(name = "head-bone", weight = 0.3, type = "understocking")))

    gadget_dir_write(gd, gadget_likelihood_component("penalty", name="neck-bone", weight = 0.5))
    expect_equal(list.files(dir), c("likelihood", "neck-bone.penaltyfile"))
    expect_equal(
        gadget_dir_read(gd, "likelihood")$components,
        list(
            list(),
            component = list(name = "head-bone", weight = 0.3, type = "understocking"),
            component = list(name = "neck-bone", weight = 0.5, type = "penalty", datafile = "neck-bone.penaltyfile")))

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
})
