library(DBI)
library(RPostgreSQL)

db_params <- list(dbname = "mf", host = "/tmp/pg_mfdb")

db_connection <- function() {
    do.call(dbConnect, c(list(drv = PostgreSQL()), db_params))
}
