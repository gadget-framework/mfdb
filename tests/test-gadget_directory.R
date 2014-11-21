library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

ok_group("Will create a directory when creating gadget_directories", {
    expect_error(
        gadget_directory("/dont-run-as-root"),
        "/dont-run-as-root")

    dir <- tempfile()
    expect_true(!file.exists(dir))
    gd <- gadget_directory(dir)
    expect_true(file.exists(dir) && file_test("-d", dir))
})

ok_group("Can write files", {
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
