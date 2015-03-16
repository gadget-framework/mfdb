# Dump all useful data into directory
mfdb_cs_dump <- function(mdb, directory) {
    if(!utils::file_test("-d", directory)) tryCatch(dir.create(directory), warning = function(w) {
        stop(paste("Could not create output directory:", conditionMessage(w)))
    })
    if(!utils::file_test("-d", directory)) stop(paste("Could not create output directory", directory))

    for (table_name in c(mfdb_cs_taxonomy, mfdb_measurement_tables)) {
        mdb$logger$info(paste0("Dumping table ", table_name))
        data_out <- mfdb_fetch(mdb,
            "SELECT * FROM ", table_name,
            ifelse(
                table_name == "prey",
                " WHERE predator_id IN (SELECT predator_id FROM predator WHERE case_study_id = ",
                " WHERE (case_study_id = "),
            sql_quote(mdb$case_study_id), ")")

        write.table(
            data_out,
            file = file.path(directory, table_name),
            fileEncoding = "UTF-8")
    }
}

# Read data back out again
mfdb_cs_restore <- function(mdb, directory) {
    # Read in table, returning empty data frame if file is empty
    read_data <- function (table_name) {
        tryCatch(read.table(
            file = file.path(directory, table_name),
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
        mfdb_send(mdb, "DELETE FROM prey",
            " WHERE predator_id IN (",
            "SELECT predator_id FROM predator WHERE case_study_id = ", sql_quote(mdb$case_study_id),
            ")")

        # Delete from everything else
        for (table_name in rev(c(mfdb_cs_taxonomy, mfdb_measurement_tables))) {
            mdb$logger$info(paste0("Emptying ", table_name))
            if (table_name != 'prey') {
                mfdb_send(mdb, "DELETE FROM ", table_name,
                    " WHERE case_study_id = ", sql_quote(mdb$case_study_id),
                    NULL)
            }
        }

        for (table_name in c(mfdb_cs_taxonomy, mfdb_measurement_tables)) {
            mdb$logger$info(paste0("Restoring table ", table_name))
            data_in <- read_data(table_name)
            if (nrow(data_in) == 0) next

            # Override any case study column with our current one
            if ('case_study_id' %in% colnames(data_in)) {
                data_in$case_study_id <- mdb$case_study_id
            }

            # Measurement tables have an autoincrement ID, don't import that
            if (table_name %in% mfdb_measurement_tables) {
                data_in <- data_in[,names(data_in) != paste0(table_name, '_id')]
            }
            # TODO: We're also blatting predator_id in the process, which
            #   breaks the foreign key
            # TODO: Get max for primary key, min for data_in, bump values onto end
            # TODO: Remember predator_id offset for prey
            # TODO: Update sequence with new MAX

            # Copy data to temporary table, then insert it
            mfdb_bulk_copy(mdb, table_name, data_in, function (temp_tbl) {
                mfdb_send(mdb,
                    "INSERT INTO ", table_name,
                    " (", paste(names(data_in), collapse=","), ")",
                    " SELECT ", paste(names(data_in), collapse=","),
                    " FROM ", temp_tbl,
                    NULL)
            })
        }
    }))
}
