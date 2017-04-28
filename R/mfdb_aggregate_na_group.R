mfdb_na_group <- function (sub_aggregate, na_group) {
    return(structure(
        list(sub_aggregate),
        na_group = na_group,
        class = c("mfdb_na_group", "mfdb_aggregate")))
}

# Wrap select a case statement to pick out NULL
select_clause.mfdb_na_group <- function(mdb, x, col, outputname, group_disabled = FALSE) {
    sub_select <- select_clause(mdb, x[[1]], col, "!fake_out!", group_disabled)
    sub_select <- gsub("\\s+AS\\s+!fake_out!", "", sub_select)

    paste("CASE",
        "WHEN", col, "IS NULL THEN", sql_quote(attr(x, "na_group")),
        "ELSE", sub_select,
        "END AS", outputname)
}

# Wrap where to make sure we still get NULLs
where_clause.mfdb_na_group <- function(mdb, x, col, outputname, group_disabled = FALSE) {
    sub_where <- where_clause(mdb, x[[1]], col, outputname, group_disabled)
    c(
        paste0("(", sub_where, ") OR ", col, " IS NULL"),
        NULL)
}

# Pass through the rest
sample_clause.mfdb_na_group <- function(mdb, x, col, outputname, group_disabled = FALSE) {
    sample_clause(mdb, x[[1]], col, outputname)
}
from_clause.mfdb_na_group <- function(mdb, x, col, outputname, group_disabled = FALSE) {
    from_clause(mdb, x[[1]], col, outputname, group_disabled)
}
agg_summary.mfdb_na_group <- function(mdb, x, col, outputname, data, sample_num) {
    agg_summary(mdb, x[[1]], col, outputname, data, sample_num)
}
