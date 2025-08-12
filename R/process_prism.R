clean_prism_normals <- function(
    elements = c(
        "ppt",
        "tmin",
        "tmax",
        "tmean",
        "tdmean",
        "vpdmin",
        "vpdmax",
        "solslope",
        "soltotal",
        "solclear",
        "soltrans"
    ),
    months = sprintf("%02d", 1:12),
    input_dir = "./raw_data/prism/data_files",
    out_dir = "./clean_data/prism_clean",
    overwrite = TRUE,
    verbose = TRUE
) {
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # M-code map (from your notes)
    m_codes <- c(
        ppt = "M4",
        tmin = "M5",
        tmax = "M5",
        tmean = "M5",
        tdmean = "M5",
        vpdmin = "M5",
        vpdmax = "M5",
        solslope = "M3",
        soltotal = "M3",
        solclear = "M3",
        soltrans = "M3"
    )

    written <- character(0)
    log_df <- data.frame(
        element = character(0),
        month = character(0),
        status = character(0),
        note = character(0),
        output_file = character(0),
        stringsAsFactors = FALSE
    )

    for (element in elements) {
        m_code <- unname(m_codes[[element]])
        if (is.null(m_code) || is.na(m_code)) {
            warning("No M-code for element: ", element, "; skipping all months")
            next
        }

        for (month in months) {
            # Raw filename pattern e.g. PRISM_ppt_30yr_normal_800mM4_01_bil.bil
            filename <- paste0(
                "PRISM_",
                element,
                "_30yr_normal_800m",
                m_code,
                "_",
                month,
                "_bil.bil"
            )
            file_path <- file.path(input_dir, filename)

            out_file <- file.path(out_dir, paste0(element, "_", month, ".tif"))
            if (verbose) {
                message("Processing ", element, " month ", month)
            }

            # Skip if exists and not overwriting
            if (!overwrite && file.exists(out_file)) {
                out_norm <- normalizePath(
                    out_file,
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
                        element = element,
                        month = month,
                        status = "skipped_existing",
                        note = "exists; skipped",
                        output_file = out_norm,
                        stringsAsFactors = FALSE
                    )
                )
                next
            }

            # Input existence check
            if (!file.exists(file_path)) {
                if (verbose) {
                    warning("  file not found: ", file_path)
                }
                log_df <- rbind(
                    log_df,
                    data.frame(
                        element = element,
                        month = month,
                        status = "missing_input",
                        note = file_path,
                        output_file = "",
                        stringsAsFactors = FALSE
                    )
                )
                next
            }

            res <- tryCatch(
                {
                    r <- process_prism(
                        path = file_path,
                        element = element,
                        time = month
                    )
                    if (!inherits(r, "SpatRaster")) {
                        stop("process_prism did not return a SpatRaster")
                    }
                    if (terra::nlyr(r) == 0) {
                        stop("SpatRaster has 0 layers")
                    }
                    terra::writeRaster(r, out_file, overwrite = TRUE)
                    list(ok = TRUE, nlyr = terra::nlyr(r))
                },
                error = function(e) list(ok = FALSE, msg = conditionMessage(e))
            )

            if (isTRUE(res$ok)) {
                out_norm <- normalizePath(
                    out_file,
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
                        element = element,
                        month = month,
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
                        element = element,
                        month = month,
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
    log_csv <- file.path(out_dir, "prism_log.csv")
    utils::write.csv(log_df, log_csv, row.names = FALSE)
    unique(c(written, normalizePath(log_csv, winslash = "/", mustWork = FALSE)))
}
