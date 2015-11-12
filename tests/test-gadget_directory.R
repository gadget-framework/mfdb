library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

ok_group("Will create a directory when creating gadget_directories", {
    expect_error(
        gadget_directory("/dont-run-as-root"),
        "/dont-run-as-root")

    dir <- tempfile()
    ok(!file.exists(dir))
    gd <- gadget_directory(dir)
    ok(file.exists(dir) && file_test("-d", dir))
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

ok_group("Can write files into model subdirectories", {
    dir <- tempfile()
    gd <- gadget_directory(dir)
    ok(cmp(list.files(dir, recursive = TRUE), character(0)), "Directory currently empty")

    gf <- gadget_file("field/animals", components = list(
        list(),
        cows = list("daisy" = 4, "freda" = 2),
        pigs = list("george" = 3, "arnold" = 8)))
    gadget_dir_write(gd, gf)
    ok(cmp(list.files(dir, recursive = TRUE), c(
        "field/animals",
        NULL)), "Wrote our animals file into the field directory")

    gf2 <- gadget_dir_read(gd, "field/animals")
    ok(cmp(gf, gf2), "File read back is identical")

    gf2$components$pigs$george = 99
    gadget_dir_write(gd, gf2)
    ok(cmp(list.files(dir, recursive = TRUE), c(
        "field/animals",
        NULL)), "Altered animals, wrote back in right place")

    gf3 <- gadget_dir_read(gd, "field/animals")
    ok(cmp(gf2, gf3), "Content is correct")
})

ok_group("Can create files in model subdirectories", {
    dir <- tempfile()
    gd <- gadget_directory(dir)
    ok(cmp(list.files(dir, recursive = TRUE), character(0)), "Directory currently empty")

    ok(cmp_error(gadget_dir_read(gd, "farmhouse/kitchen", missing_okay = FALSE), "farmhouse"),
        "Can complain about missing files if we want to")
    gf <- gadget_dir_read(gd, "farmhouse/kitchen")
    ok(cmp(gf$components, list()), "No components yet")

    ok(cmp(list.files(dir, recursive = TRUE), character(0)), "Directory still empty")
    gadget_dir_write(gd, gf)
    ok(cmp(list.files(dir, recursive = TRUE), c("farmhouse/kitchen")), "Wrote file, created directory")
})
