expect_string <- function(actual, ...) {
    expect_equal(
        as.character(actual),
        paste0(c(...), "\n", collapse = ""))
}

print.stringvec <- function(x, ...) {
    cat(x, sep = "\n")
}

cmp <- function(a, b) {
    if(identical(all.equal(a,b), TRUE)) return(TRUE)

    return(c(
        capture.output(str(a)),
        "... does not equal...",
        capture.output(str(b))
    ))
}

cmp_error <- function(exp, expected_regexp) {
    msg <- tryCatch({exp ; "No error returned"}, error = function(e) e$message)
    if(grepl(expected_regexp, msg)) TRUE else paste0("'", msg, "' should contain '", expected_regexp, "'")
}

expect_equal <- function(actual, expected) {
    ok(cmp(actual, expected), paste0(
        strtrim(gsub("\\s+", " ", deparse(substitute(actual)), perl = TRUE), 30),
        " == ",
        strtrim(gsub("\\s+", " ", deparse(substitute(expected)), perl = TRUE), 30),
        "", collapse=""))
}

expect_error <- function(exp, expected_regexp) {
    ok(cmp_error(exp, expected_regexp), paste0("Error contained '", expected_regexp, "'"))
}

expect_true <- function(exp) {
    ok(exp, "Is true")
}

section <- function (message, fn) {
    cat(paste("#", message, collapse="\n"), "\n")
    fn()
}
