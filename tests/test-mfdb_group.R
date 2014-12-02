library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

ok_group("Can generate groupobjects", {
    expect_equal(
        class(mfdb_group()),
        c("mfdb_group", "mfdb_aggregate"))

    expect_equal(
        class(mfdb_group(cows = c("daisy", "freda"))),
        c("mfdb_group", "mfdb_aggregate"))
})

ok_group("Can generate a numbered group", {
    expect_equal(
        mfdb_group_numbered("age", c(4), c(5)),
        mfdb_group(age1 = c(4), age2 = c(5)))
})

ok_group("Can get a bootstrap group", {
    expect_error(
        mfdb_bootstrap_group(2, "camel"),
        "Second argument should be a mfdb_group")
    expect_error(
        mfdb_bootstrap_group(0, mfdb_group()),
        "Count should be equal or greater than 1")
})

ok_group("Predefined timestep groups", {
    ok(cmp(mfdb_timestep_yearly[[1]], 1:12), "mfdb_timestep_yearly")
    ok(cmp(mfdb_timestep_biannually[[1]], 1:6), "mfdb_timestep_biannually")
    ok(cmp(mfdb_timestep_biannually[[2]], 7:12), "mfdb_timestep_biannually")
    ok(cmp(mfdb_timestep_quarterly[[1]], 1:3), "mfdb_timestep_quarterly")
    ok(cmp(mfdb_timestep_quarterly[[2]], 4:6), "mfdb_timestep_quarterly")
    ok(cmp(mfdb_timestep_quarterly[[3]], 7:9), "mfdb_timestep_quarterly")
    ok(cmp(mfdb_timestep_quarterly[[4]], 10:12), "mfdb_timestep_quarterly")
})

g <- NULL

ok_group("Aggregates with mfdb_group", local({
    g <<- mfdb_group(a = c(1,"two",3), b = c(88))
    ok(cmp(capture.output(pre_query(NULL, g, "out")), c(
        "DROP TABLE temp_out",
        "CREATE  TABLE temp_out (sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value  INT )",
        "INSERT INTO temp_out (sample,name,value) VALUES (0,'a','1'),(0,'a','two'),(0,'a','3'),(0,'b','88')",
        "CREATE INDEX ON temp_out (value,name,sample)",
        "NULL")), "Created temporary table")
    ok(cmp(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), "temp_out.name AS out"), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), "temp_out"), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), "col = temp_out.value"), "Where clause")

    g <<- mfdb_group(a1 = c(1,2,3), badger = c(88, 21), a3 = c(99))
    ok(cmp(capture.output(pre_query(NULL, g, "out")), c(
        "DROP TABLE temp_out",
        "CREATE  TABLE temp_out (sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value  INT )",
        "INSERT INTO temp_out (sample,name,value) VALUES (0,'a1',1),(0,'a1',2),(0,'a1',3),(0,'badger',88),(0,'badger',21),(0,'a3',99)",
        "CREATE INDEX ON temp_out (value,name,sample)",
        "NULL")), "Created temporary table")
    ok(cmp(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), "temp_out.name AS out"), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), "temp_out"), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), "col = temp_out.value"), "Where clause")
}, asNamespace('mfdb')))

ok_group("Aggregates with mfdb_bootstrap_group", local({
    g <<- mfdb_bootstrap_group(2, mfdb_group(camels = c(44), aardvarks = c(88)))
    ok(cmp(capture.output(pre_query(NULL, g, "out")), c(
        "DROP TABLE temp_out",
        "CREATE  TABLE temp_out (sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value  INT )",
        "INSERT INTO temp_out (sample,name,value) VALUES (1,'camels',44),(1,'aardvarks',88),(2,'camels',44),(2,'aardvarks',88)",
        "CREATE INDEX ON temp_out (value,name,sample)",
        "NULL")), "Created temporary table")
    ok(cmp(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), "temp_out.name AS out"), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), "temp_out"), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), "col = temp_out.value"), "Where clause")

    set.seed(123456)
    g <<- mfdb_bootstrap_group(2, mfdb_group(g1 = c(44, 55), g2 = c(88, 99)))
    ok(cmp(capture.output(pre_query(NULL, g, "out")), c(
        "DROP TABLE temp_out",
        "CREATE  TABLE temp_out (sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value  INT )",
        "INSERT INTO temp_out (sample,name,value) VALUES (1,'g1',55),(1,'g1',55),(1,'g2',88),(1,'g2',88),(2,'g1',44),(2,'g1',44),(2,'g2',99),(2,'g2',88)",
        "CREATE INDEX ON temp_out (value,name,sample)",
        "NULL")), "Created temporary table")
    ok(cmp(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), "temp_out.name AS out"), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), "temp_out"), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), "col = temp_out.value"), "Where clause")

    # Test a few more random combinations
    set.seed(8081)
    g <<- mfdb_bootstrap_group(2, mfdb_group(g1 = c(44, 55), g2 = c(88, 99)))
    ok(cmp(capture.output(pre_query(NULL, g, "out")), c(
        "DROP TABLE temp_out",
        "CREATE  TABLE temp_out (sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value  INT )",
        "INSERT INTO temp_out (sample,name,value) VALUES (1,'g1',44),(1,'g1',55),(1,'g2',99),(1,'g2',99),(2,'g1',44),(2,'g1',55),(2,'g2',99),(2,'g2',99)",
        "CREATE INDEX ON temp_out (value,name,sample)",
        "NULL")), "Created temporary table")

    set.seed(203785)
    g <<- mfdb_bootstrap_group(2, mfdb_group(g1 = c(44, 55), g2 = c(88, 99)))
    ok(cmp(capture.output(pre_query(NULL, g, "out")), c(
        "DROP TABLE temp_out",
        "CREATE  TABLE temp_out (sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value  INT )",
        "INSERT INTO temp_out (sample,name,value) VALUES (1,'g1',55),(1,'g1',55),(1,'g2',99),(1,'g2',99),(2,'g1',44),(2,'g1',44),(2,'g2',88),(2,'g2',99)",
        "CREATE INDEX ON temp_out (value,name,sample)",
        "NULL")), "Created temporary table")
}, asNamespace('mfdb')))

ok_group("Aggregates with mfdb_group areas", local({
    # Areas are a special case, they have to be broken down into areacells first
    g <<- mfdb_group(a = c(1,2,3), b = c(88, 89))
    ok(cmp(capture.output(pre_query(NULL, g, "area")), c(
        "DROP TABLE temp_area",
        "CREATE  TABLE temp_area (sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value  INT )",
        "INSERT INTO temp_area SELECT 0 AS sample, 'a' AS name, areacell_id AS value FROM division WHERE case_study_id = -1 AND division IN ('1','2','3')",
        "INSERT INTO temp_area SELECT 0 AS sample, 'b' AS name, areacell_id AS value FROM division WHERE case_study_id = -1 AND division IN ('88','89')",
        "CREATE INDEX ON temp_area (value,name,sample)",
        "NULL")), "Created temporary table")
}, asNamespace('mfdb')))
