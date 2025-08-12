build_modis_clean <- function(
    modis_vars,
    raw_dir = "./raw_data/modis",
    out_dir = "./clean_data/modis_clean",
    start_date = "2010-01-01",
    end_date = "2024-12-31",
    fun_agg = "mean",
    overwrite = TRUE,
    verbose = TRUE
) {
    # Ensure output directory exists
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    # Dates vector (daily)
    dates <- seq(as.Date(start_date), as.Date(end_date), by = "1 day")

    # Simple results log
    results <- list()

    for (product in names(modis_vars)) {
        # Gather all HDF files for this product
        product_files <- list.files(
            raw_dir,
            pattern = paste0("^", product, ".*\\.(hdf|HDF)$"),
            full.names = TRUE,
            recursive = TRUE
        )

        if (length(product_files) == 0L) {
            if (verbose) {
                message(
                    "No files found for product ",
                    product,
                    " in ",
                    raw_dir,
                    ". Skipping."
                )
            }
            next
        }

        # Iterate over variables for this product
        for (var in modis_vars[[product]]) {
            # Make a subfolder per product/variable
            var_dir <- file.path(out_dir, product, var)
            dir.create(var_dir, recursive = TRUE, showWarnings = FALSE)

            for (d in dates) {
                date_str <- format(as.Date(d), "%Y-%m-%d")
                if (verbose) {
                    message(
                        "Processing ",
                        var,
                        " from ",
                        product,
                        " for date ",
                        date_str
                    )
                }

                # Try to process and write
                res <- tryCatch(
                    {
                        r <- process_modis_merge(
                            path = product_files,
                            date = date_str,
                            subdataset = var,
                            fun_agg = fun_agg
                        )

                        save_path <- file.path(
                            var_dir,
                            paste0(product, "_", var, "_", date_str, ".tif")
                        )
                        terra::writeRaster(r, save_path, overwrite = overwrite)

                        list(
                            status = "ok",
                            path = save_path,
                            error = NA_character_
                        )
                    },
                    error = function(e) {
                        if (verbose) {
                            message(
                                "Failed for ",
                                var,
                                " on ",
                                date_str,
                                ": ",
                                e$message
                            )
                        }
                        list(
                            status = "error",
                            path = NA_character_,
                            error = e$message
                        )
                    }
                )

                results[[length(results) + 1L]] <- data.frame(
                    product = product,
                    variable = var,
                    date = date_str,
                    status = res$status,
                    path = res$path,
                    error = res$error,
                    stringsAsFactors = FALSE
                )
            }
        }
    }

    # Return a data.frame log (invisible so it doesn't spam the console)
    invisible(do.call(rbind, results))
}
