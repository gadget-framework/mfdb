library(mfdb)
library(unittest, quietly = TRUE)
helpers <- c('utils/helpers.R', 'tests/utils/helpers.R') ; source(helpers[file.exists(helpers)])

ok_group("sql_quote", {
    sql_quote <- mfdb:::sql_quote
    ok(cmp_error(sql_quote(c()), "empty"), "Empty vector results in error")

    ok(sql_quote("") == "''")
    ok(sql_quote("3") == "'3'")
    ok(sql_quote(4) == "4")
    ok(sql_quote("Greengrocer's") == "'Greengrocer''s'")

    ok(sql_quote("3", always_bracket = TRUE) == "('3')")
    ok(sql_quote(c(1,2), always_bracket = TRUE) == "(1,2)")
    ok(sql_quote(c(1,2), brackets = "[]") == "[1,2]")
   
    ok(sql_quote(c(1, 2, 3)) == "(1,2,3)")
    ok(sql_quote(c("a", "bee's", "c")) == "('a','bee''s','c')")
    ok(sql_quote(c(1, NA, 3)) == "(1,NULL,3)")

    ok(sql_quote(1, always_quote = TRUE) == "'1'")
    ok(sql_quote(1:5, always_quote = TRUE) == "('1','2','3','4','5')")
})

ok_group("sql_create_index", {
    ci <- mfdb:::sql_create_index
    ok(cmp(ci("tbl", "col"), "CREATE INDEX ON tbl (col)"))
    ok(cmp(ci("tbl", c("A", "B")), "CREATE INDEX ON tbl (A,B)"))
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
    default_constraint_list <- function(table_name) data.frame(
        name = rep(c("const1", "const2", "const3"), length(table_name)),
        table_name = rep(table_name, each = 3),
        definition = rep(c("a", "b", "c"), length(table_name)),
        stringsAsFactors = FALSE)

    disable_constraints <- function(table_name, code_block, am_owner = 1, constraint_list = default_constraint_list(table_name)) {
        mdb <- fake_mdb()
        mdb$ret_rows <- list(
            "tableowner = current_user" = data.frame(count = am_owner),
            "SELECT relname" = constraint_list
        )
        out <- capture.output(tryCatch({
            out <- mfdb:::mfdb_disable_constraints(mdb, table_name, code_block)
            cat("Returned:", out, "\n")
        }, error = function (e) e$message))

        if (am_owner == 0) {
            ok(grepl("SELECT.*current_user", out[[1]]), "First query selects ownership")
            return(out[2:length(out)])
        }
        ok(grepl("SELECT.*current_user", out[[1]]), "First query selects ownership")
        ok(grepl("SELECT.*pg_get_constraintdef", out[[2]]), "Second query selects constraints")
        return(out[3:length(out)])
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

    ok(cmp(disable_constraints("tbl1", cat("executing code block\n"), constraint_list = data.frame()), c(
        "executing code block",
        "Returned: ")), "Still works when no constraints exist")

    out <- disable_constraints("tbl1", cat("executing code block\n"), am_owner = 0)
    ok(grepl('Not owner of table', out[[1]], fixed = TRUE), "Left constraints alone when not owner")
    ok(cmp(disable_constraints("tbl1", cat("executing code block\n"), am_owner = 0), c(
        out[[1]],
        "executing code block",
        "Returned: ")), "Still executed code block")

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
