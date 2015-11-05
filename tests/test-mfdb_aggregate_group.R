library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

cmp_group <- function (a, b) {
    cmp(a[names(a)], b[names(b)])
}

ok_group("Can generate groupobjects", {
    expect_equal(
        class(mfdb_group()),
        c("mfdb_group", "mfdb_aggregate"))

    expect_equal(
        class(mfdb_group(cows = c("daisy", "freda"))),
        c("mfdb_group", "mfdb_aggregate"))
})

ok_group("Can generate a numbered group", {
    ok(cmp_group(
        mfdb_group_numbered("age", c(4), c(5)),
        mfdb_group(age1 = c(4), age2 = c(5))
        ), "groups match")
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

mdb <- fake_mdb(save_temp_tables = TRUE, case_study_id = -1)
mdb$ret_rows <- data.frame(count = 0)
g <- NULL

ok_group("Aggregates with mfdb_group", local({
    g <<- mfdb_group(a = c(1,"two",3), b = c(88))
    ok(cmp(capture.output(pre_query(mdb, g, "col")), c(
        paste0("SELECT COUNT(*) FROM information_schema.tables WHERE table_schema IN ('fake_schema','fake_temp_schema') AND table_name IN ('", attr(g, 'table_name'), "')"),
        paste0("CREATE  TABLE ", attr(g, 'table_name'), " (sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value  INT )"),
        paste0("INSERT INTO ", attr(g, 'table_name'), " (sample,name,value) VALUES (0,'a','1'),(0,'a','two'),(0,'a','3'),(0,'b','88')"),
        paste0("CREATE INDEX ON ", attr(g, 'table_name'), " (value,name,sample)"),
        NULL)), "Created temporary table")
    ok(cmp(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), paste0(attr(g, 'table_name'), ".name AS out")), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), attr(g, 'table_name')), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), paste0("col = ", attr(g, 'table_name'), ".value")), "Where clause")

    g <<- mfdb_group(a1 = c(1,2,3), badger = c(88, 21), a3 = c(99))
    ok(cmp(capture.output(pre_query(mdb, g, "col")), c(
        paste0("SELECT COUNT(*) FROM information_schema.tables WHERE table_schema IN ('fake_schema','fake_temp_schema') AND table_name IN ('", attr(g, 'table_name'), "')"),
        paste0("CREATE  TABLE ", attr(g, 'table_name'), " (sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value  INT )"),
        paste0("INSERT INTO ", attr(g, 'table_name'), " (sample,name,value) VALUES (0,'a1',1),(0,'a1',2),(0,'a1',3),(0,'badger',88),(0,'badger',21),(0,'a3',99)"),
        paste0("CREATE INDEX ON ", attr(g, 'table_name'), " (value,name,sample)"),
        NULL)), "Created temporary table")
    ok(cmp(sample_clause(mdb, g, "col", "out"), "0"), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), paste0(attr(g, 'table_name'), ".name AS out")), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), attr(g, 'table_name')), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), paste0("col = ", attr(g, 'table_name'), ".value")), "Where clause")
}, asNamespace('mfdb')))

mdb$ret_rows <- data.frame(count = 1)
ok_group("Aggregates with mfdb_group that's already been created", local({
    g <<- mfdb_group(a = c(1,"two",3), b = c(88))
    ok(cmp(capture.output(pre_query(mdb, g, "col")), c(
        paste0("SELECT COUNT(*) FROM information_schema.tables WHERE table_schema IN ('fake_schema','fake_temp_schema') AND table_name IN ('", attr(g, 'table_name'), "')"),
        NULL)), "Don't recreate temporary table")
}, asNamespace('mfdb')))
mdb$ret_rows <- data.frame(count = 0)

ok_group("Aggregates with mfdb_bootstrap_group", local({
    g <<- mfdb_bootstrap_group(2, mfdb_group(camels = c(44), aardvarks = c(88)))
    ok(cmp(capture.output(pre_query(mdb, g, "col")), c(
        paste0("SELECT COUNT(*) FROM information_schema.tables WHERE table_schema IN ('fake_schema','fake_temp_schema') AND table_name IN ('", attr(g, 'table_name'), "')"),
        paste0("CREATE  TABLE ", attr(g, 'table_name'), " (sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value  INT )"),
        paste0("INSERT INTO ", attr(g, 'table_name'), " (sample,name,value) VALUES (1,'camels',44),(1,'aardvarks',88),(2,'camels',44),(2,'aardvarks',88)"),
        paste0("CREATE INDEX ON ", attr(g, 'table_name'), " (value,name,sample)"),
        NULL)), "Created temporary table")
    ok(cmp(sample_clause(mdb, g, "col", "out"), paste0(attr(g, 'table_name'), ".sample")), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), paste0(attr(g, 'table_name'), ".name AS out")), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), attr(g, 'table_name')), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), paste0("col = ", attr(g, 'table_name'), ".value")), "Where clause")
    ok(cmp(agg_summary(mdb, g, 'col', 'out', data.frame(bssample = "1.1.1"), 1), list(
        camels = 44,
        aardvarks = 88
    )), "Aggregation summary (sample 1)")
    ok(cmp(agg_summary(mdb, g, 'col', 'out', data.frame(bssample = "2.2.2"), 2), list(
        camels = 44,
        aardvarks = 88
    )), "Aggregation summary (sample 2)")

    g <<- mfdb_bootstrap_group(2, mfdb_group(g1 = c(44, 55), g2 = c(88, 99)), seed = 123456)
    ok(cmp(capture.output(pre_query(mdb, g, "col")), c(
        paste0("SELECT COUNT(*) FROM information_schema.tables WHERE table_schema IN ('fake_schema','fake_temp_schema') AND table_name IN ('", attr(g, 'table_name'), "')"),
        paste0("CREATE  TABLE ", attr(g, 'table_name'), " (sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value  INT )"),
        paste0("INSERT INTO ", attr(g, 'table_name'), " (sample,name,value) VALUES (1,'g1',55),(1,'g1',55),(1,'g2',88),(1,'g2',88),(2,'g1',44),(2,'g1',44),(2,'g2',99),(2,'g2',88)"),
        paste0("CREATE INDEX ON ", attr(g, 'table_name'), " (value,name,sample)"),
        NULL)), "Created temporary table")
    ok(cmp(sample_clause(mdb, g, "col", "out"), paste0(attr(g, 'table_name'), ".sample")), "Sample clause")
    ok(cmp(select_clause(mdb, g, "col", "out"), paste0(attr(g, 'table_name'), ".name AS out")), "Select clause")
    ok(cmp(from_clause(mdb, g, "col", "out"), attr(g, 'table_name')), "From clause")
    ok(cmp(where_clause(mdb, g, "col", "out"), paste0("col = ", attr(g, 'table_name'), ".value")), "Where clause")
    ok(cmp(agg_summary(mdb, g, 'col', 'out', data.frame(bssample = "1.1.1"), 1), list(
        g1 = c(55, 55),
        g2 = c(88, 88)
    )), "Aggregation summary (sample 1)")
    ok(cmp(agg_summary(mdb, g, 'col', 'out', data.frame(bssample = "2.0.0"), 2), list(
        g1 = c(44, 44),
        g2 = c(99, 88)
    )), "Aggregation summary (sample 2)")

    # Test a few more random combinations
    g <<- mfdb_bootstrap_group(2, mfdb_group(g1 = c(44, 55), g2 = c(88, 99)), seed = 8081)
    ok(cmp(capture.output(pre_query(mdb, g, "col")), c(
        paste0("SELECT COUNT(*) FROM information_schema.tables WHERE table_schema IN ('fake_schema','fake_temp_schema') AND table_name IN ('", attr(g, 'table_name'), "')"),
        paste0("CREATE  TABLE ", attr(g, 'table_name'), " (sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value  INT )"),
        paste0("INSERT INTO ", attr(g, 'table_name'), " (sample,name,value) VALUES (1,'g1',44),(1,'g1',55),(1,'g2',99),(1,'g2',99),(2,'g1',44),(2,'g1',55),(2,'g2',99),(2,'g2',99)"),
        paste0("CREATE INDEX ON ", attr(g, 'table_name'), " (value,name,sample)"),
        NULL)), "Created temporary table")

    g <<- mfdb_bootstrap_group(2, mfdb_group(g1 = c(44, 55), g2 = c(88, 99)), seed = 203785)
    ok(cmp(capture.output(pre_query(mdb, g, "col")), c(
        paste0("SELECT COUNT(*) FROM information_schema.tables WHERE table_schema IN ('fake_schema','fake_temp_schema') AND table_name IN ('", attr(g, 'table_name'), "')"),
        paste0("CREATE  TABLE ", attr(g, 'table_name'), " (sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value  INT )"),
        paste0("INSERT INTO ", attr(g, 'table_name'), " (sample,name,value) VALUES (1,'g1',55),(1,'g1',55),(1,'g2',99),(1,'g2',99),(2,'g1',44),(2,'g1',44),(2,'g2',88),(2,'g2',99)"),
        paste0("CREATE INDEX ON ", attr(g, 'table_name'), " (value,name,sample)"),
        NULL)), "Created temporary table")
    ok(cmp(agg_summary(mdb, g, 'col', 'out', data.frame(bssample = "0.1"), 1), list(
        g1 = c(55, 55),
        g2 = c(99, 99)
    )), "Aggregation summary (sample 1)")
    ok(cmp(agg_summary(mdb, g, 'col', 'out', data.frame(bssample = "0.2"), 2), list(
        g1 = c(44, 44),
        g2 = c(88, 99)
    )), "Aggregation summary (sample 2)")
}, asNamespace('mfdb')))

ok_group("Aggregates with mfdb_group areas", local({
    # Areas are a special case, they have to be broken down into areacells first
    g <<- mfdb_group(a = c(1,2,3), b = c(88, 89))
    ok(cmp(capture.output(pre_query(mdb, g, "c.areacell_id")), c(
        paste0("SELECT COUNT(*) FROM information_schema.tables WHERE table_schema IN ('fake_schema','fake_temp_schema') AND table_name IN ('", attr(g, 'table_name'), "')"),
        paste0("CREATE  TABLE ", attr(g, 'table_name'), " (sample INT DEFAULT 1 NOT NULL, name VARCHAR(10), value  INT )"),
        paste0("INSERT INTO ", attr(g, 'table_name'), " SELECT 0 AS sample, 'a' AS name, areacell_id AS value FROM division WHERE case_study_id = -1 AND division IN ('1','2','3')"),
        paste0("INSERT INTO ", attr(g, 'table_name'), " SELECT 0 AS sample, 'b' AS name, areacell_id AS value FROM division WHERE case_study_id = -1 AND division IN ('88','89')"),
        paste0("CREATE INDEX ON ", attr(g, 'table_name'), " (value,name,sample)"),
        NULL)), "Created temporary table")
}, asNamespace('mfdb')))

ok_group("Seed handling", {
    # Helper to define groups with a given seed
    orig_group <- mfdb_group(g1 = c(44, 55), g2 = c(88, 99))
    bs_group <- function (seed) { mfdb_bootstrap_group(2, orig_group, seed = seed) }

    # Repeatedly manage to make the same sample group (also tested by above tests)
    ok(cmp(bs_group(99), bs_group(99)), "Forced seed creates the same group")

    # Seed before == seed after
    old_seed <- .Random.seed
    bs_group(433)
    ok(cmp(old_seed, .Random.seed), "The PRNG state got restored with set seed")
    old_seed <- .Random.seed
    bs_group(NULL)
    ok(cmp(old_seed, .Random.seed), "The PRNG state got restored with NULL seed")

    # Works if there is no seed set yet
    remove(".Random.seed", pos = globalenv())
    ok(cmp_group(bs_group(123456), bs_group(123456)), "Managed to generate groups before seed is set")
})
