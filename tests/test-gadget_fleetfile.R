library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

ok_group("Total Fleet (no extra fields)", {
    write_component <- function (comp, ...) {
        gd <- gadget_directory(tempfile())
        gadget_dir_write(gd, comp)
        return(gd)
    }

    area_group <- mfdb_group(divA = c('1','2'), divB = c('3'), divC = c('5'))

    gd <- write_component(gadget_fleet_component(
        'totalfleet',
        data = structure(data.frame(
            year = c(1998),
            step = c(1),
            area = c('divA', 'divC'),
            fleet = c('bob'),
            count = c(5),
            stringsAsFactors = FALSE), area = area_group)))
    ok(cmp_file(gd, file.path('Modelfiles', 'fleet.fleet'),
        ver_string,
        "; ",
        "[fleetcomponent]",
        "totalfleet\ttotalfleet",
        "livesonareas\t1\t3",
        "multiplicative\t1",
        "suitability\t",
        "amount\tData/fleet.totalfleet.data",
        NULL), "Totalfleet defaults")
    ok(cmp_file(gd, file.path('Data', 'fleet.totalfleet.data'),
        ver_string,
        "; -- data --",
        "; year\tstep\tarea\tfleet\tcount",
        "1998\t1\t1\tbob\t5",
        "1998\t1\t3\tbob\t5",
        NULL), "Totalfleet defaults data pulls area from data")
    
    gd <- write_component(gadget_fleet_component(
        'totalfleet',
        name = 'barry',
        livesonareas = c('divA', 'divB', 'divC'),
        multiplicative = 8,
        suitability = "function constant 4;",
        data = structure(data.frame(
            year = c(1998:2000),
            step = c(1),
            area = c('divA'),
            fleet = c('bob'),
            count = c(5),
            stringsAsFactors = FALSE), area = area_group)))
    ok(cmp_file(gd, file.path('Modelfiles', 'fleet.fleet'),
        ver_string,
        "; ",
        "[fleetcomponent]",
        "totalfleet\tbarry",
        "livesonareas\t1\t2\t3",
        "multiplicative\t8",
        "suitability\tfunction constant 4;",
        "amount\tData/fleet.barry.data",
        NULL), "Can override livesonareas and multiplicative")
    ok(cmp_file(gd, file.path('Data', 'fleet.barry.data'),
        ver_string,
        "; -- data --",
        "; year\tstep\tarea\tfleet\tcount",
        "1998\t1\t1\tbob\t5",
        "1999\t1\t1\tbob\t5",
        "2000\t1\t1\tbob\t5",
        NULL), "Can override livesonareas and multiplicative (amountfile)")
})

ok_group("Types with extra parameters", {
    gd <- gadget_directory(tempfile())
    area_group <- mfdb_group(divA = c('1','2'), divB = c('3'), divC = c('5'))

    gd <- write_component(gadget_fleet_component(
        'totalfleet',
        name = 'barry',
        suitability = "function constant 4;",
        catchability = list(stockA=4, stockB=5),
        quotafunction = 'simple',
        biomasslevel = c(1000, 2000),
        quotalevel = c(0.1, 0.4, 0.9),
        data = structure(data.frame(
            year = c(1998:2000),
            step = c(1),
            area = c('divA'),
            fleet = c('bob'),
            count = c(5),
            stringsAsFactors = FALSE), area = area_group)))
    ok(cmp_file(gd, file.path('Modelfiles', 'fleet.fleet'),
        ver_string,
        "; ",
        "[fleetcomponent]",
        "totalfleet\tbarry",
        "livesonareas\t1",
        "multiplicative\t1",
        "suitability\tfunction constant 4;",
        "amount\tData/fleet.barry.data",
        NULL), "Totalfleet ignores catchability")

    gd <- write_component(gadget_fleet_component(
        'effortfleet',
        name = 'barry',
        suitability = "function constant 4;",
        catchability = list(stockA=4, stockB=5),
        quotafunction = 'simple',
        biomasslevel = c(1000, 2000),
        quotalevel = c(0.1, 0.4, 0.9),
        data = structure(data.frame(
            year = c(1998:2000),
            step = c(1),
            area = c('divA'),
            fleet = c('bob'),
            count = c(5),
            stringsAsFactors = FALSE), area = area_group)))
    ok(cmp_file(gd, file.path('Modelfiles', 'fleet.fleet'),
        ver_string,
        "; ",
        "[fleetcomponent]",
        "effortfleet\tbarry",
        "livesonareas\t1",
        "multiplicative\t1",
        "suitability\tfunction constant 4;",
        "catchability\t",
        "stockA\t4",
        "stockB\t5",
        "amount\tData/fleet.barry.data",
        NULL), "Effortfleet includes catchability")

    gd <- write_component(gadget_fleet_component(
        'quotafleet',
        name = 'barry',
        suitability = "function constant 4;",
        catchability = list(stockA=4, stockB=5),
        quotafunction = 'simple',
        biomasslevel = c(1000, 2000),
        quotalevel = c(0.1, 0.4, 0.9),
        data = structure(data.frame(
            year = c(1998:2000),
            step = c(1),
            area = c('divA'),
            fleet = c('bob'),
            count = c(5),
            stringsAsFactors = FALSE), area = area_group)))
    ok(cmp_file(gd, file.path('Modelfiles', 'fleet.fleet'),
        ver_string,
        "; ",
        "[fleetcomponent]",
        "quotafleet\tbarry",
        "livesonareas\t1",
        "multiplicative\t1",
        "suitability\tfunction constant 4;",
        "quotafunction\tsimple",
        "biomasslevel\t1000\t2000",
        "quotalevel\t0.1\t0.4\t0.9",
        "amount\tData/fleet.barry.data",
        NULL), "Quotafleet includes quotafunction, biomasslevel, quotalevel")
})

ok_group("Multiple fleet files & mainfile", {
    gd <- gadget_directory(tempfile())
    area_group <- mfdb_group(divA = c('1','2'), divB = c('3'), divC = c('5'))

    gadget_dir_write(gd, gadget_fleet_component(
        'totalfleet',
        name = 'alfred',
        data = structure(data.frame(
            year = c(1998),
            step = c(1),
            area = c(1),
            fleet = c('bob'),
            count = c(5),
            stringsAsFactors = FALSE), area = area_group)))
    ok(cmp_file(gd, 'main',
        ver_string,
        "timefile\t",
        "areafile\t",
        "printfiles\t; Required comment",
        "[stock]",
        "[tagging]",
        "[otherfood]",
        "[fleet]",
        "fleetfiles\tModelfiles/fleet.fleet",
        "[likelihood]",
        NULL), "Added default name to mainfile")

    gadget_dir_write(gd, gadget_fleet_component(
        'totalfleet',
        name = 'alfred',
        livesonareas = c(1),
        data = structure(data.frame(
            year = c(1999),
            step = c(1),
            area = c(1),
            fleet = c('bob'),
            count = c(5),
            stringsAsFactors = FALSE), area = area_group),
        fleetfile = 'otherfleet'))
    ok(cmp_file(gd, 'main',
        ver_string,
        "timefile\t",
        "areafile\t",
        "printfiles\t; Required comment",
        "[stock]",
        "[tagging]",
        "[otherfood]",
        "[fleet]",
        "fleetfiles\tModelfiles/fleet.fleet\tModelfiles/otherfleet.fleet",
        "[likelihood]",
        NULL), "Added otherfleet to mainfile")
    ok(cmp_file(gd, file.path('Modelfiles', 'fleet.fleet'),
        ver_string,
        "; ",
        "[fleetcomponent]",
        "totalfleet\talfred",
        "livesonareas\t1",
        "multiplicative\t1",
        "suitability\t",
        "amount\tData/fleet.alfred.data",
        NULL), "Default fleet.fleet file has first alfred")
    ok(cmp_file(gd, file.path('Modelfiles', 'otherfleet.fleet'),
        ver_string,
        "; ",
        "[fleetcomponent]",
        "totalfleet\talfred",
        "livesonareas\t1",
        "multiplicative\t1",
        "suitability\t",
        "amount\tData/otherfleet.alfred.data",
        NULL), "otherfleet.fleet has alfred with non-clashing data file")

    gadget_dir_write(gd, gadget_fleet_component(
        'totalfleet',
        name = 'alfred',
        livesonareas = c(2),
        data = structure(data.frame(
            year = c(1999),
            step = c(1),
            area = c(1),
            fleet = c('bob'),
            count = c(5),
            stringsAsFactors = FALSE), area = area_group),
        fleetfile = 'otherfleet'))
    ok(cmp_file(gd, 'main',
        ver_string,
        "timefile\t",
        "areafile\t",
        "printfiles\t; Required comment",
        "[stock]",
        "[tagging]",
        "[otherfood]",
        "[fleet]",
        "fleetfiles\tModelfiles/fleet.fleet\tModelfiles/otherfleet.fleet",
        "[likelihood]",
        NULL), "Still 2 fleet files")
    ok(cmp_file(gd, file.path('Modelfiles', 'fleet.fleet'),
        ver_string,
        "; ",
        "[fleetcomponent]",
        "totalfleet\talfred",
        "livesonareas\t1",
        "multiplicative\t1",
        "suitability\t",
        "amount\tData/fleet.alfred.data",
        NULL), "Default fleet.fleet unchanged")
    ok(cmp_file(gd, file.path('Modelfiles', 'otherfleet.fleet'),
        ver_string,
        "; ",
        "[fleetcomponent]",
        "totalfleet\talfred",
        "livesonareas\t2",
        "multiplicative\t1",
        "suitability\t",
        "amount\tData/otherfleet.alfred.data",
        NULL), "otherfleet.fleet has updated alfred")
})

ok_group("Error conditions", {
    area_group <- mfdb_group(divA = c('1','2'), divB = c('3'), divC = c('5'))

    ok(cmp_error(
        gadget_fleet_component('totalfleet'),
        'data'), "Notice missing data")

    ok(cmp_error(
        gadget_fleet_component('totalfleet', livesonareas = c(1), data = structure(data.frame(
            year = c(1998),
            stoop = c(1),
            area = c(1),
            fleet = c('bob'),
            count = c(5),
            stringsAsFactors = FALSE), area = area_group)),
        'stoop'), "Notice incompatible columns")
})
