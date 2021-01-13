library(mfdb)
library(unittest, quietly = TRUE)
helpers <- c('utils/helpers.R', 'tests/utils/helpers.R') ; source(helpers[file.exists(helpers)])

ok_group("mfdb", {
    attempts <- list()
    mock_functions('DBI', list(
        dbConnect = function(...) {
            args <- list(...)
            args$drv <- class(args$drv)[[1]]
            pq_driver <<- args$drv
            attempts <<- c(attempts, list(args))
            stop("Grigh, database")
        }
    ), {
        attempts <<- list()
        ok(cmp_error(mfdb('Test'), 'database'), "Didn't connect to db")
        ok(cmp(attempts, list(
            list(host = "/tmp", dbname = "mf", drv = pq_driver),
            list(host = "/var/tmp", dbname = "mf", drv = pq_driver),
            list(host = "/var/run/postgresql", dbname = "mf", drv = pq_driver),
            list(host = "localhost", dbname = "mf", drv = pq_driver),
            list(host = "localhost", user = "mf", password = "mf", dbname = "mf", drv = pq_driver),
            list(host = "/tmp/pg_mfdb", dbname = "mf", drv = pq_driver)
        )), "Tried default connections")

        attempts <<- list()
        ok(cmp_error(mfdb('Test', db_params = list(dbname = "mfdb")), 'database'), "Didn't connect to db")
        ok(cmp(attempts, list(
            list(dbname = "mfdb", host = "/tmp", drv = pq_driver),
            list(dbname = "mfdb", host = "/var/tmp", drv = pq_driver),
            list(dbname = "mfdb", host = "/var/run/postgresql", drv = pq_driver),
            list(dbname = "mfdb", host = "localhost", drv = pq_driver),
            list(dbname = "mfdb", host = "localhost", user = "mf", password = "mf", drv = pq_driver),
            list(dbname = "mfdb", host = "/tmp/pg_mfdb", drv = pq_driver)
        )), "Tried a custom dbname")

        attempts <<- list()
        ok(cmp_error(mfdb('Test', db_params = list(user = "frank", password = "frank")), 'database'), "Didn't connect to db")
        ok(cmp(attempts, list(
            list(user = "frank", password = "frank", host = "/tmp", dbname = "mf", drv = pq_driver),
            list(user = "frank", password = "frank", host = "/var/tmp", dbname = "mf", drv = pq_driver),
            list(user = "frank", password = "frank", host = "/var/run/postgresql", dbname = "mf", drv = pq_driver),
            list(user = "frank", password = "frank", host = "localhost", dbname = "mf", drv = pq_driver),
            list(user = "frank", password = "frank", host = "localhost", dbname = "mf", drv = pq_driver),
            list(user = "frank", password = "frank", host = "/tmp/pg_mfdb", dbname = "mf", drv = pq_driver)
        )), "Tried custom user/pass")

        attempts <<- list()
        ok(cmp_error(mfdb('Test', db_params = c(user = "frank", password = "frank")), 'database'), "Didn't connect to db")
        ok(cmp(attempts, list(
            list(user = "frank", password = "frank", host = "/tmp", dbname = "mf", drv = pq_driver),
            list(user = "frank", password = "frank", host = "/var/tmp", dbname = "mf", drv = pq_driver),
            list(user = "frank", password = "frank", host = "/var/run/postgresql", dbname = "mf", drv = pq_driver),
            list(user = "frank", password = "frank", host = "localhost", dbname = "mf", drv = pq_driver),
            list(user = "frank", password = "frank", host = "localhost", dbname = "mf", drv = pq_driver),
            list(user = "frank", password = "frank", host = "/tmp/pg_mfdb", dbname = "mf", drv = pq_driver)
        )), "Tried custom user/pass with vector")

        attempts <<- list()
        ok(cmp_error(mfdb('Test', db_params = list(host = "db.com", user = "frank", password = "frank")), 'database'), "Didn't connect to db")
        ok(cmp(attempts, list(
            list(host = "db.com", user = "frank", password = "frank", dbname = "mf", drv = pq_driver),
            list(host = "db.com", user = "frank", password = "frank", dbname = "mf", drv = pq_driver),
            list(host = "db.com", user = "frank", password = "frank", dbname = "mf", drv = pq_driver),
            list(host = "db.com", user = "frank", password = "frank", dbname = "mf", drv = pq_driver),
            list(host = "db.com", user = "frank", password = "frank", dbname = "mf", drv = pq_driver),
            list(host = "db.com", user = "frank", password = "frank", dbname = "mf", drv = pq_driver)
        )), "Overrode host/user/pass")

        attempts <<- list()
        ok(cmp_error(mfdb('Test', db_params = list(host = "mfdb.rhi.hi.is", user = "polly", password = "ppwd")), 'database'), "Didn't connect to db")
        ok(cmp(attempts, list(
            list(host = "mfdb.rhi.hi.is", user = "polly", password = "ppwd", dbname = "dbname=mf sslmode=require", drv = pq_driver)
        )), "Used special mfdb server parameters")

        old_force <- Sys.getenv('MFDB_FORCE_AVAILABLE')
        Sys.setenv(MFDB_FORCE_AVAILABLE = "")
        ok(cmp(mfdb(check_db_available = T), FALSE), "Didn't error with check_db_available, just return FALSE")
        Sys.setenv(MFDB_FORCE_AVAILABLE = "T")
        ok(cmp(mfdb(check_db_available = T), TRUE), "MFDB_FORCE_AVAILABLE on, returned TRUE anyway")
        Sys.setenv(MFDB_FORCE_AVAILABLE = old_force)
    })
})
