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

        attempts <<- list()
        ok(cmp_error(mfdb('Test', db_params = list(host = "mfdb.rhi.hi.is", user = "polly", password = "ppwd")), 'database'), "Didn't connect to db")
        ok(cmp(attempts, list(
            list(host = "mfdb.rhi.hi.is", user = "polly", password = "ppwd", dbname = "dbname=mf sslmode=require", drv = 'PostgreSQLDriver')
        )), "Used special mfdb server parameters")
    })
})
