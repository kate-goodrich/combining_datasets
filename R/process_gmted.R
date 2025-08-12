clean_gmted <- function(
    statistics = c(
        "Breakline Emphasis",
        "Systematic Subsample",
        "Median Statistic",
        "Minimum Statistic",
        "Mean Statistic",
        "Maximum Statistic",
        "Standard Deviation Statistic"
    ),
    filenames = c(
        "be30_grd",
        "ds30_grd",
        "md30_grd",
        "mi30_grd",
        "mn30_grd",
        "mx30_grd",
        "sd30_grd"
    ),
    resolution = "30 arc-seconds",
    raw_dir = "./raw_data/gmted/data_files",
    out_dir = "./clean_data/gmted_clean",
    overwrite = TRUE,
    verbose = TRUE
) {
    stopifnot(length(statistics) == length(filenames))
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    written <- character(0)
    log_df <- data.frame(
        statistic = character(0),
        filename = character(0),
        status = character(0),
        note = character(0),
        output_file = character(0),
        stringsAsFactors = FALSE
    )

    for (i in seq_along(statistics)) {
        stat <- statistics[i]
        base <- filenames[i]

        input_path <- file.path(raw_dir, base)
        output_file <- file.path(out_dir, paste0(base, "_clean.tif"))

        if (verbose) {
            message("Processing: ", stat, " (", base, ")")
        }

        # Idempotent skip
        if (!overwrite && file.exists(output_file)) {
            note <- "exists; skipped"
            if (verbose) {
                message("  ", note, ": ", output_file)
            }
            out_norm <- normalizePath(
                output_file,
                winslash = "/",
                mustWork = FALSE
            )
            written <- c(written, out_norm)
            log_df <- rbind(
                log_df,
                data.frame(
                    statistic = stat,
                    filename = base,
                    status = "skipped_existing",
                    note = note,
                    output_file = out_norm,
                    stringsAsFactors = FALSE
                )
            )
            next
        }

        res <- tryCatch(
            {
                r <- process_gmted(
                    variable = c(stat, resolution),
                    path = input_path
                )
                if (!inherits(r, "SpatRaster")) {
                    stop("process_gmted did not return a SpatRaster")
                }
                if (terra::nlyr(r) == 0) {
                    stop("SpatRaster has 0 layers")
                }
                terra::writeRaster(r, output_file, overwrite = TRUE)
                list(ok = TRUE, file = output_file, nlyr = terra::nlyr(r))
            },
            error = function(e) {
                list(ok = FALSE, msg = conditionMessage(e))
            }
        )

        if (isTRUE(res$ok)) {
            out_norm <- normalizePath(
                res$file,
                winslash = "/",
                mustWork = FALSE
            )
            written <- c(written, out_norm)
            if (verbose) {
                message("  Saved: ", out_norm, " (layers=", res$nlyr, ")")
            }
            log_df <- rbind(
                log_df,
                data.frame(
                    statistic = stat,
                    filename = base,
                    status = "ok",
                    note = paste0("layers=", res$nlyr),
                    output_file = out_norm,
                    stringsAsFactors = FALSE
                )
            )
        } else {
            if (verbose) {
                warning("  Failed: ", stat, " (", base, "): ", res$msg)
            }
            log_df <- rbind(
                log_df,
                data.frame(
                    statistic = stat,
                    filename = base,
                    status = "error",
                    note = res$msg,
                    output_file = "",
                    stringsAsFactors = FALSE
                )
            )
        }
    }

    # Write a log CSV and include it in returned paths
    log_csv <- file.path(out_dir, "gmted_log.csv")
    utils::write.csv(log_df, log_csv, row.names = FALSE)
    written <- unique(c(
        written,
        normalizePath(log_csv, winslash = "/", mustWork = FALSE)
    ))

    return(written)
}
