library(DBI)
library(RPostgreSQL)

db_params <- list(dbname = "mf_inttest")

db_connection <- function() {
    do.call(dbConnect, c(list(drv = PostgreSQL()), db_params))
}
