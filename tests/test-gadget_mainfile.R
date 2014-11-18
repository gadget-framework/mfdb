library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

ok_group("gadget_mainfile_update", {
    gd <- gadget_directory(tempfile())

    # Populate our one bit, fill out absolute minimum
    mfdb:::gadget_mainfile_update(gd, areafile = "area")
    ok(cmp_file(gd, "main",
        ver_string,
        "timefile\t",
        "areafile\tarea",
        "printfiles\t; Required comment",
        "[stock]",
        "[tagging]",
        "[otherfood]",
        "[fleet]",
        "[likelihood]"))

    # Add some more values
    mfdb:::gadget_mainfile_update(gd,
        timefile = "timefile",
        printfiles = c("a", "b", "c"),
        stockfiles = 1:3)
    ok(cmp_file(gd, "main",
        ver_string,
        "timefile\ttimefile",
        "areafile\tarea",
        "printfiles\ta\tb\tc",  # NB: We've removed the comment
        "[stock]",
        "stockfiles\t1\t2\t3",
        "[tagging]",
        "[otherfood]",
        "[fleet]",
        "[likelihood]"))

    # Finish it, appending to some
    mfdb:::gadget_mainfile_update(gd,
        timefile = "finefile",
        printfiles = c("c", "d"),
        stockfiles = 1:3,
        tagfiles = "T",
        otherfoodfiles = "OF",
        fleetfiles = "F",
        likelihoodfiles = "LL")
    ok(cmp_file(gd, "main",
        ver_string,
        "timefile\tfinefile",  # NB: We replaced this value
        "areafile\tarea",
        "printfiles\ta\tb\tc\td",  # NB: ...but appended here
        "[stock]",
        "stockfiles\t1\t2\t3",
        "[tagging]",
        "tagfiles\tT",
        "[otherfood]",
        "otherfoodfiles\tOF",
        "[fleet]",
        "fleetfiles\tF",
        "[likelihood]",
        "likelihoodfiles\tLL"))
})
