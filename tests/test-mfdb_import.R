library(mfdb)
library(unittest, quietly = TRUE)
helpers <- c('utils/helpers.R', 'tests/utils/helpers.R') ; source(helpers[file.exists(helpers)])

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
