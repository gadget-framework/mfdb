# Parse a string into a data.frame
table_string <- function (str) {
    read.table(
        textConnection(str),
        blank.lines.skip = TRUE,
        header = TRUE,
        stringsAsFactors = FALSE)
}

mfdb_populate_example_data <- function (mdb) {
    mfdb_import_area(mdb, data.frame(
        id = c(1,2,3),
        name = c('45G01', '45G02', '45G03'),
        size = c(5)))
    mfdb_import_temperature(mdb, data.frame(
        year = rep(c(1998, 1999), each = 12),
        month = c(1:12, 1:12),
        areacell = c(rep('45G01', times = 24)),
        temperature = c(1:12, 25:36)))
    mfdb_import_division(mdb, list(
        divA = c('45G03'),
        divB = c('45G01', '45G02'),
        divC = c('45G01')))

    mfdb_import_vessel_taxonomy(mdb, data.frame(
        name = c('1.RSH', '2.COM'),
        stringsAsFactors = FALSE
    ))
    mfdb_import_survey(mdb, data_source = "fleet_component_example",
        table_string("
year    month   areacell        species vessel  length  age     weight
1998    1       45G01           COD     1.RSH           21      2       210
1998    1       45G02           COD     2.COM           34      3       220
1998    1       45G03           COD     2.COM           34      3       230
1998    1       45G01           COD     2.COM           62      1       320
1998    1       45G02           COD     2.COM           53      1       330
1998    1       45G03           COD     2.COM           54      2       430
1998    1       45G01           COD     1.RSH           28      2       210
1998    1       45G02           COD     2.COM           34      3       220
1998    1       45G03           COD     1.RSH           24      3       230
1998    1       45G01           COD     1.RSH           12      1       320
1998    1       45G02           COD     2.COM           44      1       330
1998    1       45G03           COD     1.RSH           14      2       430
        "))
}
