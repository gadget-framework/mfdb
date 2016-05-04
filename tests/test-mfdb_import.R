library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

mdb <- fake_mdb()
even <- function (x) x %% 2 == 0

# Capture all output, filtering log messages
cap <- function(code) {
    grep("\\d{4}-\\d{2}-\\d{2}", capture.output({
        x <- code
        cat("Return Value:\n")
        str(x)
    }), value = TRUE, invert = TRUE)
}

ok_group("mfdb_import_taxonomy", {
    # Pretend table is empty
    mdb$ret_rows <- data.frame()

    ok(cmp(cap(mfdb:::mfdb_import_taxonomy(mdb, 'gear', data.frame(
        name = c(),
        description = c(),
        stringsAsFactors = TRUE))), c(
        "Return Value:",
        " NULL",
        NULL)), "No input data, nothing to do")

    ok(cmp(cap(mfdb:::mfdb_import_taxonomy(mdb, 'gear', data.frame(
        id = 1:3,
        name = c('RES', 'FEZ', 'DES'),
        description = c('Research', 'Tommy Cooper', 'Encryption'),
        stringsAsFactors = TRUE))), c(
        "SELECT gear_id, name, t_group, description FROM gear ORDER BY 1",
        "INSERT INTO gear (gear_id,name,t_group,description) VALUES (1,'RES',NULL,'Research'),(2,'FEZ',NULL,'Tommy Cooper'),(3,'DES',NULL,'Encryption')",
        "Return Value:",
        " NULL",
        NULL)), "Insert 3 rows into empty table")

    mdb$ret_rows <- data.frame(
        gear_id = 1:3,
        name = c('RES', 'FEZ', 'DES'),
        t_group = NA,
        description = c('Research', 'Tommy Cooper', 'Encryption'),
        stringsAsFactors = FALSE)
    ok(cmp(cap(mfdb:::mfdb_import_taxonomy(mdb, 'gear', data.frame(
        id = 1:3,
        name = c('RES', 'FEZ', 'DES'),
        description = c('Research', 'Tommy Cooper', 'Encryption'),
        stringsAsFactors = TRUE))), c(
        "SELECT gear_id, name, t_group, description FROM gear ORDER BY 1",
        "Return Value:",
        " NULL",
        NULL)), "Identical, nothing happens")

    mdb$ret_rows <- data.frame(
        gear_id = 11:13,
        name = c('RES', 'FEZ', 'DES'),
        t_group = NA,
        description = c('Research', 'Tommy Cooper', 'Encryption'),
        stringsAsFactors = FALSE)
    ok(cmp(cap(mfdb:::mfdb_import_taxonomy(mdb, 'gear', data.frame(
        id = 1:3,
        name = c('RES', 'FEZ', 'DES'),
        description = c('Research', 'Tommy Cooper', 'Encryption'),
        stringsAsFactors = TRUE))), c(
        "SELECT gear_id, name, t_group, description FROM gear ORDER BY 1",
        "Return Value:",
        " NULL",
        NULL)), "Differing IDs don't phase us")

    mdb$ret_rows <- data.frame(
        gear_id = 1:2,
        name = c('RES', 'FEZ'),
        t_group = NA,
        description = c('Research', 'Hat'),
        stringsAsFactors = FALSE)
    ok(cmp(cap(mfdb:::mfdb_import_taxonomy(mdb, 'gear', data.frame(
        id = 1:3,
        name = c('RES', 'FEZ', 'DES'),
        description = c('Research', 'Tommy Cooper', 'Encryption'),
        stringsAsFactors = TRUE))), c(
        "SELECT gear_id, name, t_group, description FROM gear ORDER BY 1",
        "INSERT INTO gear (gear_id,name,t_group,description) VALUES (3,'DES',NULL,'Encryption')",
        "UPDATE gear SET name='FEZ',t_group=NULL,description='Tommy Cooper' WHERE gear_id = 2",
        "Return Value:",
        " NULL",
        NULL)), "Update 1 row, insert a new row")

    mdb$ret_rows <- data.frame(
        gear_id = 1:2,
        name = c('RES', 'FEZ'),
        t_group = NA,
        description = c('Research', 'Hat'),
        stringsAsFactors = FALSE)
    ok(cmp(cap(mfdb:::mfdb_import_taxonomy(mdb, 'gear', data.frame(
        id = 4:6,
        name = c('RES', 'FEZ', 'DES'),
        description = c('Research', 'Tommy Cooper', 'Encryption'),
        stringsAsFactors = TRUE))), c(
        "SELECT gear_id, name, t_group, description FROM gear ORDER BY 1",
        "INSERT INTO gear (gear_id,name,t_group,description) VALUES (6,'DES',NULL,'Encryption')",
        "UPDATE gear SET name='FEZ',t_group=NULL,description='Tommy Cooper' WHERE gear_id = 2",
        "Return Value:",
        " NULL",
        NULL)), "Differing IDs only affect adding new rows")

    mdb$ret_rows <- data.frame(
        gear_id = 1:2,
        name = c('RES', 'FEZ'),
        t_group = c('RES', 'RES'),
        description = c('Research', 'Hat'),
        stringsAsFactors = FALSE)
    ok(cmp(cap(mfdb:::mfdb_import_taxonomy(mdb, 'gear', data.frame(
        id = 4:6,
        name = c('RES', 'FEZ', 'DES'),
        t_group = c('RES', 'RES', 'RES'),
        description = c('Research', 'Hat', 'Encryption'),
        stringsAsFactors = TRUE))), c(
        "SELECT gear_id, name, t_group, description FROM gear ORDER BY 1",
        "INSERT INTO gear (gear_id,name,t_group,description) VALUES (6,'DES','RES','Encryption')",
        "Return Value:",
        " NULL",
        NULL)), "Differing IDs only affect adding new rows")
})

ok_group("sanitise_col", local({
    # Case insensitive matching of column name (but no partial matching)
    ok(cmp(sanitise_col(mdb, data.frame(aaaa=1,b=2,aa=3), 'aaaa'), 1), "Picked correct column (aaaa)")
    ok(cmp(sanitise_col(mdb, data.frame(aaaa=1,b=2,aa=3), 'aa'), 3), "Picked correct column (aa)")
    ok(cmp(sanitise_col(mdb, data.frame(aaaa=9,b=8,aa=7), 'AaAa'), 9), "Case is insensitive (AaAa)")
    ok(cmp(sanitise_col(mdb, data.frame(aaaa=9,b=8,aa=7), 'Aa'), 7), "Case is insensitive (Aa)")

    # Defaults used if column name not mentioned
    ok(cmp_error(sanitise_col(mdb, data.frame(a=9,b=8,c=7), 'bueller'), 'bueller'), "Complained about missing column when no default supplied")
    ok(cmp(sanitise_col(mdb, data.frame(a=9,b=8,c=7), 'x', default = 99), 99), "No column x, used default")
    ok(cmp(sanitise_col(mdb, data.frame(a=9,b=8,x=7), 'x', default = 99), 7), "Column overrode default")

    # Checks test all column content
    ok(cmp(
        sanitise_col(mdb, data.frame(gerald = c(2,4,6,8), freda = c(1,2,3,5)), 'gerald', test = even),
        c(2,4,6,8)), "All of gerald passes test")
    ok(cmp_error(
        sanitise_col(mdb, data.frame(gerald = c(2,4,6,8), freda = c(1,2,3,5)), 'freda', test = even),
        "freda"), "Freda does not")
    ok(cmp_error(
        sanitise_col(mdb, data.frame(gerald = c(2,4,6,8), freda = c(1,2,3,5)), 'freda', test = even),
        "1,3,5$"), "Which items that don't match are mentioned")
    ok(cmp_error(
        sanitise_col(mdb, data.frame(freda = c(1:150)), 'freda', test = even),
        paste(c(seq(1, 50 * 2 - 1, 2), " \\.\\.\\.$"), collapse=",")), "Give up complaining after 50 items")
    
}, asNamespace('mfdb')))
