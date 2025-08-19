clean_koppen_geiger <- function(
    kg_files = c(
        "raw_data/koppen_geiger/data_files/Beck_KG_V1_present_0p083.tif",
        "raw_data/koppen_geiger/data_files/Beck_KG_V1_present_conf_0p083.tif"
    ),
    out_dir = "./clean_data/koppen_geiger_clean",
    overwrite = TRUE,
    verbose = TRUE
) {
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    written <- character(0)
    log_df <- data.frame(
        input_file = character(0),
        status = character(0),
        note = character(0),
        output_file = character(0),
        stringsAsFactors = FALSE
    )

    for (kg_path in kg_files) {
        if (verbose) {
            message("Processing: ", kg_path)
        }

        if (!file.exists(kg_path)) {
            if (verbose) {
                warning("  missing input: ", kg_path)
            }
            log_df <- rbind(
                log_df,
                data.frame(
                    input_file = kg_path,
                    status = "missing_input",
                    note = "file not found",
                    output_file = "",
                    stringsAsFactors = FALSE
                )
            )
            next
        }

        filename <- basename(kg_path)
        out_path <- file.path(
            out_dir,
            gsub("\\.tif$", "_processed.tif", filename)
        )
        out_norm <- normalizePath(out_path, winslash = "/", mustWork = FALSE)

        # Idempotent skip
        if (!overwrite && file.exists(out_path)) {
            if (verbose) {
                message("  exists; skipped: ", out_norm)
            }
            written <- c(written, out_norm)
            log_df <- rbind(
                log_df,
                data.frame(
                    input_file = kg_path,
                    status = "skipped_existing",
                    note = "exists; skipped",
                    output_file = out_norm,
                    stringsAsFactors = FALSE
                )
            )
            next
        }

        res <- tryCatch(
            {
                r <- process_koppen_geiger(path = kg_path)
                if (!inherits(r, "SpatRaster")) {
                    stop("process_koppen_geiger did not return a SpatRaster")
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
            if (verbose) {
                message("  Saved: ", out_norm, " (layers=", res$nlyr, ")")
            }
            written <- c(written, out_norm)
            log_df <- rbind(
                log_df,
                data.frame(
                    input_file = kg_path,
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
                    input_file = kg_path,
                    status = "error",
                    note = res$msg,
                    output_file = "",
                    stringsAsFactors = FALSE
                )
            )
        }
    }

    # Write log CSV and return paths for {targets}
    log_csv <- file.path(out_dir, "koppen_geiger_log.csv")
    utils::write.csv(log_df, log_csv, row.names = FALSE)

    unique(c(written, normalizePath(log_csv, winslash = "/", mustWork = FALSE)))
}
