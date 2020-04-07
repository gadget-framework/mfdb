# Dump all useful data into directory
mfdb_cs_dump <- function(mdb, out_location) {
    # If asking for a .tar.gz, then output first into a temp directory, then tar it up
    tar_extn <- regmatches(out_location, regexpr("\\.tgz|\\.tar(\\.gz|\\.bz2|\\.xz)?$", out_location))
    if (isTRUE(nzchar(tar_extn))) {
        temp_output <- tempfile("mfdb_cs_dump")
        mfdb_cs_dump(mdb, temp_output)

        old_dir <- getwd()
        tryCatch({
            cat("", file = out_location)  # normalizePath requires existant file
            out_tar <- normalizePath(out_location)
            setwd(temp_output)

            ret <- tar(out_tar, ".", compression = c(
                ".tgz" = "gzip",
                ".tar" = FALSE,
                ".tar.gz" = "gzip",
                ".tar.bzip2" = "bzip2",
                ".tar.xz" = "xz",
                NULL)[[tar_extn]], tar = "internal")
        }, finally = setwd(old_dir))
        return(invisible(NULL))
    }

    if(!utils::file_test("-d", out_location)) tryCatch(dir.create(out_location), warning = function(w) {
        stop(paste("Could not create output directory:", conditionMessage(w)))
    })
    if(!utils::file_test("-d", out_location)) stop(paste("Could not create output directory", out_location))

    for (table_name in c(mfdb_taxonomy_tables, mfdb_measurement_tables)) {
        mdb$logger$info(paste0("Dumping table ", table_name))
        mfdb_send(mdb,
            "SELECT * FROM ", table_name,
            result = function (data_out, offset) {
                write.table(
                    data_out,
                    file = file.path(out_location, table_name),
                    append = (offset > 0),
                    col.names = (offset == 0),
                    fileEncoding = "UTF-8")
        })
    }
    invisible(NULL)
}

# Read data back out again
mfdb_cs_restore <- function(mdb, in_location) {
    # If in_location is a tarball, untar it then carry on
    tar_extn <- regmatches(in_location, regexpr("\\.tgz|\\.tar(\\.gz|\\.bz2|\\.xz)?$", in_location))
    if (isTRUE(nzchar(tar_extn))) {
        temp_output <- tempfile("mfdb_cs_restore") 
        untar(in_location, exdir = temp_output)
        in_location <- temp_output
    }
    
    # Read in table, returning empty data frame if file is empty
    read_data <- function (table_name) {
        tryCatch(read.table(
            file = file.path(in_location, table_name),
            header = TRUE,
            fileEncoding = "UTF-8"), error = function (e) {
                if (grepl("first five rows are empty", e$message)) {
                    return(data.frame())
                }
                stop(e)
            })
    }

    mfdb_transaction(mdb, mfdb_disable_constraints(mdb, mfdb_measurement_tables, {
        # Prey is indirectly linked to case_study, so need to do this first
        mfdb_send(mdb, "DELETE FROM prey")

        # Delete from everything else
        for (table_name in rev(c(mfdb_taxonomy_tables, mfdb_measurement_tables))) {
            mdb$logger$debug(paste0("Emptying ", table_name))
            if (table_name != 'prey') {
                mfdb_send(mdb, "DELETE FROM ", table_name,
                    NULL)
            }
        }

        for (table_name in c(mfdb_taxonomy_tables, mfdb_measurement_tables)) {
            mdb$logger$debug(paste0("Restoring table ", table_name))
            data_in <- read_data(table_name)
            id_col <- paste0(table_name, '_id')
            if (nrow(data_in) == 0) next

            # Set offset for autoincrement IDs
            if (table_name %in% mfdb_measurement_tables) {
                tbl_max <- mfdb_fetch(mdb,
                    "SELECT MAX(", id_col, ")",
                    " FROM ", table_name)[1,1]
                if (is.na(tbl_max)) tbl_max <- 0
                if (table_name == "predator") {
                    predator_offset <- 0 - min(data_in[[id_col]]) + tbl_max + 1
                }
                data_in[[id_col]] <- data_in[[id_col]] - min(data_in[[id_col]]) + tbl_max + 1
            }
            # Prey also needs predator_id's offsetting
            if (table_name == "prey") {
                data_in$predator_id <- data_in$predator_id + predator_offset
            }

            # Copy data to temporary table, then insert it
            mfdb_bulk_copy(mdb, table_name, data_in, function (temp_tbl) {
                mfdb_send(mdb,
                    "INSERT INTO ", table_name,
                    " (", paste(names(data_in), collapse=","), ")",
                    " SELECT ", paste(names(data_in), collapse=","),
                    " FROM ", temp_tbl,
                    NULL)
            })

            # Update sequence with new maximum
            mfdb_fetch(mdb,
                "SELECT pg_catalog.setval(",
                "pg_get_serial_sequence(", sql_quote(table_name), ",", sql_quote(id_col),"),",
                "MAX(", id_col, ")) FROM ", table_name)

            # Get rid of data and free some memory
            rm(data_in)
            gc()
        }
    }))
    invisible(NULL)
}
