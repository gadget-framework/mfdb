library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

ok_group("Can write area files", {
    gd <- gadget_directory(tempfile())
    gadget_dir_write(gd, gadget_areafile(data.frame(
        area = c("divA", "divB", "divC"),
        size = c(15, 25, 10),
        stringsAsFactors = FALSE
    ), data.frame(
        year = rep(c(1998), each = 12),
        step = rep(c("q1", "q2", "q3", "q4"), each = 3),
        area = rep(c("divA", "divB", "divC"), times = 4),
        temperature = 1:12,
        stringsAsFactors = FALSE
    )))
    ok(cmp_file(gd, 'main',
        ver_string,
        "timefile\t",
        "areafile\tarea",
        "printfiles\t; Required comment",
        "[stock]","[tagging]","[otherfood]","[fleet]","[likelihood]"
    ))
    ok(cmp_file(gd, 'area',
        ver_string,
        "areas\tdivA\tdivB\tdivC",
        "size\t15\t25\t10",
        "temperature\t",
        "; -- data --",
        "; year\tstep\tarea\ttemperature",
        "1998\tq1\tdivA\t1",
        "1998\tq1\tdivB\t2",
        "1998\tq1\tdivC\t3",
        "1998\tq2\tdivA\t4",
        "1998\tq2\tdivB\t5",
        "1998\tq2\tdivC\t6",
        "1998\tq3\tdivA\t7",
        "1998\tq3\tdivB\t8",
        "1998\tq3\tdivC\t9",
        "1998\tq4\tdivA\t10",
        "1998\tq4\tdivB\t11",
        "1998\tq4\tdivC\t12"
    ))
})
