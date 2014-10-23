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

# Make some obvious time groupings
step_year <- mfdb_group(year = 1:12)
step_halves <- mfdb_group(h1 = 1:6, h2 = 7:12)
step_quarters <- mfdb_group(q1 = 1:3, q2 = 4:6, q3 = 7:9, q4 = 10:12)
