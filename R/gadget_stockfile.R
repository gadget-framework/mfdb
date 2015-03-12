gadget_stockfile_extremes <- function (stock_name, data) {
    out <- list(
        stockname = stock_name)
    for (col in c('age', 'length')) {
        if (!(col %in% colnames(data))) {
            stop("Data missing column ", col)
        }
    }

    structure(
        list(list(
            stockname = stock_name,
            minage = min(vapply(attr(data, 'age'), min, 0)),
            maxage = max(vapply(attr(data, 'age'), max, 0)),
            minlength = min(vapply(attr(data, 'length'), min, 0)),
            maxlength = max(vapply(attr(data, 'length'), max, 0)))),
        stock_name = paste0(stock_name, collapse = ""),
        class = c("gadget_stockfile_extremes", "gadget_stockfile"))
}

gadget_stockfile_refweight <- function (stock_name, data) {
    for (col in c('length', 'mean')) {
        if (!(col %in% colnames(data))) {
            stop("Data missing column ", col)
        }
    }

    # Sort incoming data, then regroup
    refwgt <- data[order(data$length), c('length', 'mean')]
    refwgt <- data.frame(
        length = vapply(attr(data, 'length')[refwgt$length], min, 0), # Grouping -> minimum value
        weight = refwgt$mean,  # Assuming it's mean weight here
        stringsAsFactors = TRUE)

    structure(
        list(list(
            dl = min(vapply(attr(data, 'length'), diff, 0)),
            refweightfile = gadget_file(paste0('Modelfiles/', stock_name, '.refwgt'), data = refwgt))),
        stock_name = paste0(stock_name, collapse = ""),
        class = c("gadget_stockfile_refweight", "gadget_stockfile"))
}

gadget_dir_write.gadget_stockfile <- function(gd, obj) {
    # Read in any existing stock file
    stock_filename <- file.path("Modelfiles", attr(obj, 'stock_name'))
    gadget_stockfile <- gadget_dir_read(
        gd,
        stock_filename,
        missing_okay = TRUE,
        file_type = "bare_component")

    # Update component we refer to
    for (i in seq_len(length(obj))) {
        name <- names(obj)[[i]]
        if (is.null(name)) {
            name <- 1 # The null component is the first one
            if (length(gadget_stockfile$components) == 0) {
                gadget_stockfile$components <- list(list())
            }
        }
        # Combine 2 lists, using values from obj in preference to existing values
        gadget_stockfile$components[[name]] <- c(obj[[i]], gadget_stockfile$components[[name]])[union(
            names(gadget_stockfile$components[[name]]),
            names(obj[[i]]))]
    }

    # Write it back
    gadget_dir_write(gd, gadget_stockfile)
    gadget_mainfile_update(gd, stockfiles = stock_filename)
    invisible(NULL)
}
