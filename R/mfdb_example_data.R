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
}
