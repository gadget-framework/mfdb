library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

ok_group("mfdb", {
    attempts <- list()
    mock_functions('DBI', list(
        dbConnect = function(...) {
            args <- list(...)
            args$drv <- class(args$drv)[[1]]
            attempts <<- c(attempts, list(args))
            stop("Grigh, database")
        }
    ), {
        attempts <<- list()
        ok(cmp_error(mfdb('Test'), 'database'), "Didn't connect to db")
        ok(cmp(attempts, list(
            list(host = "/tmp", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(host = "/var/tmp", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(host = "/var/run/postgresql", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(host = "localhost", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(host = "localhost", user = "mf", password = "mf", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(host = "/tmp/pg_mfdb", dbname = "mf", drv = 'PostgreSQLDriver')
        )), "Tried default connections")

        attempts <<- list()
        ok(cmp_error(mfdb('Test', db_params = list(dbname = "mfdb")), 'database'), "Didn't connect to db")
        ok(cmp(attempts, list(
            list(dbname = "mfdb", host = "/tmp", drv = 'PostgreSQLDriver'),
            list(dbname = "mfdb", host = "/var/tmp", drv = 'PostgreSQLDriver'),
            list(dbname = "mfdb", host = "/var/run/postgresql", drv = 'PostgreSQLDriver'),
            list(dbname = "mfdb", host = "localhost", drv = 'PostgreSQLDriver'),
            list(dbname = "mfdb", host = "localhost", user = "mf", password = "mf", drv = 'PostgreSQLDriver'),
            list(dbname = "mfdb", host = "/tmp/pg_mfdb", drv = 'PostgreSQLDriver')
        )), "Tried a custom dbname")

        attempts <<- list()
        ok(cmp_error(mfdb('Test', db_params = list(user = "frank", password = "frank")), 'database'), "Didn't connect to db")
        ok(cmp(attempts, list(
            list(user = "frank", password = "frank", host = "/tmp", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(user = "frank", password = "frank", host = "/var/tmp", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(user = "frank", password = "frank", host = "/var/run/postgresql", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(user = "frank", password = "frank", host = "localhost", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(user = "frank", password = "frank", host = "localhost", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(user = "frank", password = "frank", host = "/tmp/pg_mfdb", dbname = "mf", drv = 'PostgreSQLDriver')
        )), "Tried custom user/pass")

        attempts <<- list()
        ok(cmp_error(mfdb('Test', db_params = c(user = "frank", password = "frank")), 'database'), "Didn't connect to db")
        ok(cmp(attempts, list(
            list(user = "frank", password = "frank", host = "/tmp", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(user = "frank", password = "frank", host = "/var/tmp", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(user = "frank", password = "frank", host = "/var/run/postgresql", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(user = "frank", password = "frank", host = "localhost", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(user = "frank", password = "frank", host = "localhost", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(user = "frank", password = "frank", host = "/tmp/pg_mfdb", dbname = "mf", drv = 'PostgreSQLDriver')
        )), "Tried custom user/pass with vector")

        attempts <<- list()
        ok(cmp_error(mfdb('Test', db_params = list(host = "db.com", user = "frank", password = "frank")), 'database'), "Didn't connect to db")
        ok(cmp(attempts, list(
            list(host = "db.com", user = "frank", password = "frank", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(host = "db.com", user = "frank", password = "frank", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(host = "db.com", user = "frank", password = "frank", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(host = "db.com", user = "frank", password = "frank", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(host = "db.com", user = "frank", password = "frank", dbname = "mf", drv = 'PostgreSQLDriver'),
            list(host = "db.com", user = "frank", password = "frank", dbname = "mf", drv = 'PostgreSQLDriver')
        )), "Overrode host/user/pass")
    })
})

ok_group("mfdb_insert", {
    mfdb_insert <- function (table_name, data_in, returning = "", extra = c()) {
        return(capture.output({
            out <- mfdb:::mfdb_insert(fake_mdb(), table_name, data_in, returning, extra)
            cat("Returned:", out, "\n")
        }))
    }

    ok(cmp(mfdb_insert("moo", list(moo_id = 8, oink = "a", baa = 78)), c(
        "INSERT INTO moo (moo_id,oink,baa) VALUES (8,'a',78)",
        "Returned: 1 ")),
        "Insert single row")

    ok(cmp(mfdb_insert("moo", data.frame(moo_id = c(1,2,3), oink = c("x","y","z"), bah = 43:45)), c(
        "INSERT INTO moo (moo_id,oink,bah) VALUES (1,'x',43),(2,'y',44),(3,'z',45)",
        "Returned: 3 ")),
        "Insert a data frame in one batch")

    ok(cmp(mfdb_insert("moo", data.frame(moo_id = 1:2005, oink = 1:2005)), c(
        paste0("INSERT INTO moo (moo_id,oink) VALUES ", paste0(vapply(1:999, function (i) { paste0("(", i, ",", i, ")")}, ""), collapse = ",")),
        paste0("INSERT INTO moo (moo_id,oink) VALUES ", paste0(vapply(1000:1999, function (i) { paste0("(", i, ",", i, ")")}, ""), collapse = ",")),
        paste0("INSERT INTO moo (moo_id,oink) VALUES ", paste0(vapply(2000:2005, function (i) { paste0("(", i, ",", i, ")")}, ""), collapse = ",")),
        "Returned: 2005 ")),
        "Insert multiple batches, inserts for each batch gets summed")

    ok(cmp(mfdb_insert("moo", data.frame(moo_id = 1:2005, oink = 1:2005), returning = 'moo_id'), c(
        paste0("INSERT INTO moo (moo_id,oink) VALUES ", paste0(vapply(1:999, function (i) { paste0("(", i, ",", i, ")")}, ""), collapse = ","), " RETURNING moo_id"),
        paste0("INSERT INTO moo (moo_id,oink) VALUES ", paste0(vapply(1000:1999, function (i) { paste0("(", i, ",", i, ")")}, ""), collapse = ","), " RETURNING moo_id"),
        paste0("INSERT INTO moo (moo_id,oink) VALUES ", paste0(vapply(2000:2005, function (i) { paste0("(", i, ",", i, ")")}, ""), collapse = ","), " RETURNING moo_id"),
        "Returned: 999 1000 6 ")),
        "Insert multiple batches, get back a vector (this is the closest we get to returning)")

    ok(cmp(mfdb_insert("moo", list(moo_id = 8, oink = "x"), returning = 'moo_id'), c(
        "INSERT INTO moo (moo_id,oink) VALUES (8,'x') RETURNING moo_id",
        "Returned: 1 ")), # TODO: This needs to actually test RETURNING
        "Insert single row, returning something")

    ok(cmp(mfdb_insert("moo", list(moo_id = 8, oink = "a"), extra = c("aardvark" = 99)), c(
        "INSERT INTO moo (moo_id,oink,aardvark) VALUES (8,'a',99)",
        "Returned: 1 ")),
        "Insert single row, with extra data")

    ok(cmp(mfdb_insert("moo", data.frame(moo_id = c(1,2,3), oink = c("x","y","z")), extra = c("aardvark" = 99)), c(
        "INSERT INTO moo (moo_id,oink,aardvark) VALUES (1,'x',99),(2,'y',99),(3,'z',99)",
        "Returned: 3 ")),
        "Insert a data frame in one batch")

})

ok_group("mfdb_update", {
    mfdb_update <- function (table_name, data_in, returning = "", extra = c(), where = c()) {
        return(capture.output({
            out <- mfdb:::mfdb_update(fake_mdb(), table_name, data_in, returning, extra, where)
            cat("Returned:", out, "\n")
        }))
    }

    ok(cmp(mfdb_update("moo", list(moo_id = 8, oink = "a", baa = 78)), c(
        "UPDATE moo SET oink='a',baa=78 WHERE moo_id = 8",
        "Returned: 1 ")),
        "update single row")

    ok(cmp(mfdb_update("moo", data.frame(moo_id = c(1,2,3), oink = c("x","y","z"), baa = 78)), c(
        "UPDATE moo SET oink='x',baa=78 WHERE moo_id = 1",
        "UPDATE moo SET oink='y',baa=78 WHERE moo_id = 2",
        "UPDATE moo SET oink='z',baa=78 WHERE moo_id = 3",
        "Returned: 3 ")),
        "update multiple rows")

    ok(cmp(mfdb_update("moo", list(moo_id = 8, oink = "a", baa = 78), extra = list(badger = 'mo'), where = list(case_study_id = 34)), c(
        "UPDATE moo SET oink='a',baa=78,badger='mo' WHERE moo_id = 8 AND case_study_id=34",
        "Returned: 1 ")),
        "update single row, with extras and where")

})

ok_group("mfdb_disable_constraints", {
    disable_constraints <- function(table_name, code_block) {
        mdb <- fake_mdb()
        mdb$ret_rows <- data.frame(
            name = rep(c("const1", "const2", "const3"), length(table_name)),
            table_name = rep(table_name, each = 3),
            definition = rep(c("a", "b", "c"), length(table_name)),
            stringsAsFactors = FALSE)
        out <- capture.output(tryCatch({
            out <- mfdb:::mfdb_disable_constraints(mdb, table_name, code_block)
            cat("Returned:", out, "\n")
        }, error = function (e) e$message))

        ok(grepl("SELECT.*pg_get_constraintdef", out[[1]]), "First item selects constraints")
        return(out[-1])
    }

    ok(cmp(disable_constraints("tbl1", cat("executing code block\n")), c(
        "ALTER TABLE fake_schema.tbl1 DROP CONSTRAINT const3",
        "ALTER TABLE fake_schema.tbl1 DROP CONSTRAINT const2",
        "ALTER TABLE fake_schema.tbl1 DROP CONSTRAINT const1",
        "executing code block",
        "ALTER TABLE fake_schema.tbl1 ADD CONSTRAINT const1 a",
        "ALTER TABLE fake_schema.tbl1 ADD CONSTRAINT const2 b",
        "ALTER TABLE fake_schema.tbl1 ADD CONSTRAINT const3 c",
        "Returned: ")), "Removed and replaced constraints when successful")

    ok(cmp(disable_constraints("tbl1", stop("Oh noes")), c(
        "ALTER TABLE fake_schema.tbl1 DROP CONSTRAINT const3",
        "ALTER TABLE fake_schema.tbl1 DROP CONSTRAINT const2",
        "ALTER TABLE fake_schema.tbl1 DROP CONSTRAINT const1",
        "ALTER TABLE fake_schema.tbl1 ADD CONSTRAINT const1 a",
        "ALTER TABLE fake_schema.tbl1 ADD CONSTRAINT const2 b",
        "ALTER TABLE fake_schema.tbl1 ADD CONSTRAINT const3 c",
        "[1] \"Oh noes\"")), "Removed and replaced constraints when something went wrong")

    ok(cmp(disable_constraints(c("tbl1", "tbl2"), stop("Oh noes")), c(
        "ALTER TABLE fake_schema.tbl2 DROP CONSTRAINT const3",
        "ALTER TABLE fake_schema.tbl2 DROP CONSTRAINT const2",
        "ALTER TABLE fake_schema.tbl2 DROP CONSTRAINT const1",
        "ALTER TABLE fake_schema.tbl1 DROP CONSTRAINT const3",
        "ALTER TABLE fake_schema.tbl1 DROP CONSTRAINT const2",
        "ALTER TABLE fake_schema.tbl1 DROP CONSTRAINT const1",
        "ALTER TABLE fake_schema.tbl1 ADD CONSTRAINT const1 a",
        "ALTER TABLE fake_schema.tbl1 ADD CONSTRAINT const2 b",
        "ALTER TABLE fake_schema.tbl1 ADD CONSTRAINT const3 c",
        "ALTER TABLE fake_schema.tbl2 ADD CONSTRAINT const1 a",
        "ALTER TABLE fake_schema.tbl2 ADD CONSTRAINT const2 b",
        "ALTER TABLE fake_schema.tbl2 ADD CONSTRAINT const3 c",
        "[1] \"Oh noes\"")), "Removed and replaced constraints when something went wrong")
})

ok_group("mfdb_table_exists", {
    table_exists <- function(table_name, ret) {
        mdb <- fake_mdb()
        mdb$ret_rows <- ret
        out <- capture.output(tryCatch({
            out <- mfdb:::mfdb_table_exists(mdb, table_name)
            cat("Returned:", out, "\n")
        }, error = function (e) e$message))
        return(out)
    }

    ok(cmp(table_exists("carol", ret = data.frame(count = 1)), c(
        "SELECT COUNT(*) FROM information_schema.tables WHERE (table_schema IN ('fake_schema') OR table_schema = (SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema())) AND table_name IN ('carol')",
        "Returned: TRUE ",
        NULL)), "SQL looks good")

    ok(cmp(table_exists("carol", ret = data.frame(count = 0)), c(
        "SELECT COUNT(*) FROM information_schema.tables WHERE (table_schema IN ('fake_schema') OR table_schema = (SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema())) AND table_name IN ('carol')",
        "Returned: FALSE ",
        NULL)), "Can alter return value")

    ok(cmp(table_exists(c("frank", "carol"), ret = data.frame(count = c(0, 1))), c(
        "SELECT COUNT(*) FROM information_schema.tables WHERE (table_schema IN ('fake_schema') OR table_schema = (SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema())) AND table_name IN ('frank','carol')",
        "Returned: FALSE TRUE ",
        NULL)), "Vectorises")
})
