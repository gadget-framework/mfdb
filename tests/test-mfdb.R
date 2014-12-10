library(mfdb)
library(unittest, quietly = TRUE)
source('utils/helpers.R')


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
