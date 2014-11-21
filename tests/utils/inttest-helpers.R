library(DBI)
library(RPostgreSQL)

db_params <- list(dbname = "mf", host = "/tmp/pg_mfdb")

db_connection <- function() {
    do.call(dbConnect, c(list(drv = PostgreSQL()), db_params))
}

# Remove all tables in reverse order
remove_mfdb_tables <- function(conn) {
    tables <- unlist(strsplit('sample species sex survey sampling_type vessel gear institute temperature division areacell case_study mfdb_schema', ' '))
    for(t in tables) {
        print(paste("Removing table", t))
        tryCatch(dbSendQuery(conn, paste0("DROP TABLE ", t, " CASCADE")), error = function(e) {
            if(grepl("does not exist", e$message)) return();
            stop(e)
        })
    }
    invisible(TRUE)
}
