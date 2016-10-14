mfdb_share_with <- function (mdb, user_or_role, query = TRUE, import = FALSE) {
    # Normalise inputs, have to be booleans
    import <- isTRUE(import)
    query <- isTRUE(query)

    mfdb_perms <- function (mdb, role, obj, perms) {
        perm <- function (cmd, perms) {
            mfdb_send(mdb,
                cmd, " ", paste(perms, collapse = ","),
                " ON ALL ", obj,
                " IN SCHEMA ", mdb$schema,
                ifelse(cmd == "GRANT", " TO ", " FROM "), role)
            mfdb_send(mdb,
                "ALTER DEFAULT PRIVILEGES IN SCHEMA ", mdb$schema,
                " ", cmd, " ", paste(perms, collapse = ","),
                " ON ", obj,
                ifelse(cmd == "GRANT", " TO ", " FROM "), role)
        }

        if (length(perms[perms]) > 0) {
            perm("GRANT", names(perms[perms]))
        }
        if (length(perms[!perms]) > 0) {
            perm("REVOKE", names(perms[!perms]))
        }
    }

    mfdb_transaction(mdb, {
        for (u in user_or_role) {
            mfdb_send(mdb,
                ifelse(query || import, "GRANT", "REVOKE"),
                " USAGE ON SCHEMA ", mdb$schema,
                ifelse(query || import, " TO ", " FROM "), u);

            mfdb_perms(mdb, u, 'TABLES', c(
                SELECT = query || import,
                INSERT = import,
                UPDATE = import,
                DELETE = import,
                REFERENCES = import))
            mfdb_perms(mdb, u, 'SEQUENCES', c(
                SELECT = query || import,
                USAGE = import,
                UPDATE = import))
            mfdb_perms(mdb, u, 'FUNCTIONS', c(
                EXECUTE = query || import))
        }
    })
}
