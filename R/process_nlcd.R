clean_nlcd <- function(
    products = c(
        "Land Cover",
        "Land Cover Change",
        "Land Cover Confidence",
        "Fractional Impervious Surface",
        "Impervious Descriptor",
        "Spectral Change Day of Year"
    ),
    years = 2010:2023,
    raw_dir = "./raw_data/nlcd",
    out_dir = "./clean_data/nlcd_clean",
    overwrite = TRUE,
    verbose = TRUE
) {
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    written <- character(0)
    log_df <- data.frame(
        product = character(0),
        year = integer(0),
        status = character(0),
        note = character(0),
        output_file = character(0),
        stringsAsFactors = FALSE
    )

    for (prod in products) {
        prod_folder <- gsub(" ", "_", prod)
        prod_out_dir <- file.path(out_dir, prod_folder)
        if (!dir.exists(prod_out_dir)) {
            dir.create(prod_out_dir, recursive = TRUE, showWarnings = FALSE)
        }

        prod_raw_dir <- file.path(raw_dir, prod_folder)

        for (yr in years) {
            out_path <- file.path(
                prod_out_dir,
                sprintf("%s_%d_processed.tif", prod_folder, yr)
            )
            if (verbose) {
                message("Processing ", prod, " for year ", yr)
            }

            # Idempotent skip
            if (!overwrite && file.exists(out_path)) {
                out_norm <- normalizePath(
                    out_path,
                    winslash = "/",
                    mustWork = FALSE
                )
                if (verbose) {
                    message("  exists; skipped: ", out_norm)
                }
                written <- c(written, out_norm)
                log_df <- rbind(
                    log_df,
                    data.frame(
                        product = prod,
                        year = yr,
                        status = "skipped_existing",
                        note = "exists; skipped",
                        output_file = out_norm,
                        stringsAsFactors = FALSE
                    )
                )
                next
            }

            # Quick raw path check
            if (!dir.exists(prod_raw_dir)) {
                if (verbose) {
                    warning("  raw path missing: ", prod_raw_dir)
                }
                log_df <- rbind(
                    log_df,
                    data.frame(
                        product = prod,
                        year = yr,
                        status = "missing_input",
                        note = paste0("no dir: ", prod_raw_dir),
                        output_file = "",
                        stringsAsFactors = FALSE
                    )
                )
                next
            }

            res <- tryCatch(
                {
                    r <- process_nlcd(path = prod_raw_dir, year = yr)
                    if (!inherits(r, "SpatRaster")) {
                        stop("process_nlcd did not return a SpatRaster")
                    }
                    if (terra::nlyr(r) == 0) {
                        stop("SpatRaster has 0 layers")
                    }
                    terra::writeRaster(r, out_path, overwrite = TRUE)
                    list(ok = TRUE, nlyr = terra::nlyr(r))
                },
                error = function(e) list(ok = FALSE, msg = conditionMessage(e))
            )

            if (isTRUE(res$ok)) {
                out_norm <- normalizePath(
                    out_path,
                    winslash = "/",
                    mustWork = FALSE
                )
                if (verbose) {
                    message("  Saved: ", out_norm, " (layers=", res$nlyr, ")")
                }
                written <- c(written, out_norm)
                log_df <- rbind(
                    log_df,
                    data.frame(
                        product = prod,
                        year = yr,
                        status = "ok",
                        note = paste0("layers=", res$nlyr),
                        output_file = out_norm,
                        stringsAsFactors = FALSE
                    )
                )
            } else {
                if (verbose) {
                    warning("  Failed: ", res$msg)
                }
                log_df <- rbind(
                    log_df,
                    data.frame(
                        product = prod,
                        year = yr,
                        status = "error",
                        note = res$msg,
                        output_file = "",
                        stringsAsFactors = FALSE
                    )
                )
            }
        }
    }

    # Log CSV + return all paths for {targets}
    log_csv <- file.path(out_dir, "nlcd_log.csv")
    utils::write.csv(log_df, log_csv, row.names = FALSE)
    unique(c(written, normalizePath(log_csv, winslash = "/", mustWork = FALSE)))
}
