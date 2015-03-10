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
        mean = 1:12,
        stringsAsFactors = FALSE
    ), area = list(divA = 14, divX = 99, divB = 30, divC = 44, divD = 88)))
    ok(cmp_file(gd, 'main',
        ver_string,
        "timefile\t",
        "areafile\tModelfiles/area",
        "printfiles\t; Required comment",
        "[stock]","[tagging]","[otherfood]","[fleet]","[likelihood]"
    ))
    ok(cmp_file(gd, 'Modelfiles/area',
        ver_string,
        "; divA\tdivX\tdivB\tdivC\tdivD",
        "areas\t1\t2\t3\t4\t5",
        "size\t15\t0\t25\t10\t0",
        "temperature",
        "; -- data --",
        "; year\tstep\tarea\tmean",
        "1998\tq1\t1\t1",
        "1998\tq1\t3\t2",
        "1998\tq1\t4\t3",
        "1998\tq2\t1\t4",
        "1998\tq2\t3\t5",
        "1998\tq2\t4\t6",
        "1998\tq3\t1\t7",
        "1998\tq3\t3\t8",
        "1998\tq3\t4\t9",
        "1998\tq4\t1\t10",
        "1998\tq4\t3\t11",
        "1998\tq4\t4\t12"
    ))
})
