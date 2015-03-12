gadget_stockfile_extremes <- function (stock_name, data) {
    out <- list(
        stockname = stock_name)
    for (col in c('age', 'length')) {
        if (!(col %in% colnames(data))) {
            stop("Data missing column ", col)
        } else if (data[1, col] == 'all') {
            # Get min/max from agg_summary
            summary <- attr(data, col)[['all']]
            out[[paste0("min", col)]] <- summary[[1]]
            out[[paste0("max", col)]] <- summary[[2]]
        } else {
            out[[paste0("min", col)]] <- min(data[[col]])
            out[[paste0("max", col)]] <- max(data[[col]])
        }
    }
    structure(
        list(out),
        stock_name = paste0(stock_name, collapse = ""),
        class = c("gadget_stockfile_extremes", "gadget_stockfile"))
}

gadget_stockfile_refweight <- function (stock_name, data) {
    for (col in c('length', 'mean')) {
        if (!(col %in% colnames(data))) {
            stop("Data missing column ", col)
        }
    }
    structure(
        list(list(refweightfile = gadget_file(
            paste0('Modelfiles/', stock_name, '.refwgt'),
            data = data[,c('length', 'mean')]))),
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
        combined <- c(obj[[i]], gadget_stockfile$components[[name]])
        gadget_stockfile$components[[name]] <- combined[!duplicated(names(combined))]
    }

    # Write it back
    gadget_dir_write(gd, gadget_stockfile)
    gadget_mainfile_update(gd, stockfiles = stock_filename)
    invisible(NULL)
}
