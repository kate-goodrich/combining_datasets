clean_terraclimate <- function(
    variables = c(
        "aet",
        "def",
        "swe",
        "q",
        "soil",
        "PDSI",
        "pet",
        "ppt",
        "srad",
        "tmax",
        "tmin",
        "vap",
        "vpd",
        "ws"
    ),
    date_range = c("2010-01-01", "2024-12-31"),
    raw_dir = "./raw_data/terraclimate",
    out_dir = "./clean_data/terraclimate_clean",
    overwrite = TRUE,
    verbose = TRUE
) {
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    written <- character(0)
    log_df <- data.frame(
        variable = character(0),
        status = character(0),
        note = character(0),
        output_file = character(0),
        stringsAsFactors = FALSE
    )

    for (var in variables) {
        in_path <- file.path(raw_dir, var)
        out_file <- file.path(out_dir, paste0(var, "_processed.tif"))

        if (verbose) {
            message("Processing variable: ", var)
        }

        # Idempotent skip
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
                    variable = var,
                    status = "skipped_existing",
                    note = "exists; skipped",
                    output_file = out_norm,
                    stringsAsFactors = FALSE
                )
            )
            next
        }

        # Raw path guard (optional but helpful)
        if (!dir.exists(in_path)) {
            if (verbose) {
                warning("  raw dir missing: ", in_path)
            }
            log_df <- rbind(
                log_df,
                data.frame(
                    variable = var,
                    status = "missing_input",
                    note = paste0("no dir: ", in_path),
                    output_file = "",
                    stringsAsFactors = FALSE
                )
            )
            next
        }

        res <- tryCatch(
            {
                r <- process_terraclimate(
                    date = date_range,
                    variable = var,
                    path = in_path
                )
                if (!inherits(r, "SpatRaster")) {
                    stop("process_terraclimate did not return a SpatRaster")
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
                    variable = var,
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
                    variable = var,
                    status = "error",
                    note = res$msg,
                    output_file = "",
                    stringsAsFactors = FALSE
                )
            )
        }
    }

    # Write log CSV + return paths for {targets}
    log_csv <- file.path(out_dir, "terraclimate_log.csv")
    utils::write.csv(log_df, log_csv, row.names = FALSE)
    unique(c(written, normalizePath(log_csv, winslash = "/", mustWork = FALSE)))
}
