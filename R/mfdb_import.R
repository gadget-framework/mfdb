# Import a lookup table e.g. mfdb_import_identifiers("species", read.csv('species.csv'))
mfdb_import_identifiers <- function (mfdb, table_name, new_data) {
    # Is table_name one of the recognised tables?
    if (!(table_name %in% c("institute", "fleet", "gear", "vessel", "market_category", "sampling_type", "species"))) {
        stop("Unknown identifier table ", table_name)
    }

    # Fetch all existing names
    existing = unlist(dbFetch(dbSendQuery(mfdb$db, paste0("SELECT name FROM ", table_name))))

    # Either add or update rows. Removing is risky, since we might have dependent data
    dbGetQuery(mfdb$db, "BEGIN TRANSACTION")
    for (name in unlist(new_data[1])) {
        desc <- as.character(new_data[new_data[1] == name,2])
        if (name %in% existing) {
            dbSendQuery(mfdb$db, paste0(
                "UPDATE ", table_name,
                " SET description = ", sql_quote(desc),
                " WHERE name = ", sql_quote(name)))
        } else {
            dbSendQuery(mfdb$db, paste0(
                "INSERT INTO ", table_name,
                " (name, description) VALUES ",
                " (", sql_quote(name), ",", sql_quote(desc), ")"))
        }
    }
    dbCommit(mfdb$db)
    invisible(TRUE)
}
