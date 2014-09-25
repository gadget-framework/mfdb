#        areas = c(),	# c('101', '101:1001'), e.g. Will group at most granular
#        timesteps = mfdb_group("ts", c(1,2,3),c(4,5,6)), groupings of months,
#        todo = NULL) {
mfdb <- function(db_connection = NULL, defaultparams = list(), save_tables = FALSE) {
    if (is.null(db_connection)) {
        db_connection <- dbConnect(PostgreSQL(),
                dbname="dw0605",
                host="/tmp/")
    }
    invisible(structure(list(
            logger = getLogger('mfdb'),
            defaultparams = c(defaultparams, list(
                    timesteps = mfdb_group(year = c(1,2,3,4,5,6,7,8,9,10,11,12)))),
            save_tables = save_tables,
            db = db_connection), class = "mfdb"))
}
