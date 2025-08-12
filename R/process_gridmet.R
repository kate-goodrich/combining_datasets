clean_gridmet <- function(
    statistics = c(
        "Reference alfalfa evaportranspiration",
        "Reference grass evaportranspiration",
        "Precipitation",
        "Maximum Near-Surface Relative Humidity",
        "Minimum Near-Surface Relative Humidity",
        "Near-Surface Specific Humidity",
        "Surface Downwelling Solar Radiation",
        "Wind direction at 10 m",
        "Minimum Near-Surface Air Temperature",
        "Maximum Near-Surface Air Temperature",
        "Wind speed at 10 m"
    ),
    filenames = c(
        "etr",
        "pet",
        "pr",
        "rmax",
        "rmin",
        "sph",
        "srad",
        "th",
        "tmmn",
        "tmmx",
        "vs"
    ),
    date_range = c("2010-01-01", "2024-12-31"),
    raw_dir = "./raw_data/gridmet",
    out_dir = "./clean_data/gridmet_clean",
    overwrite = TRUE,
    verbose = TRUE
) {
    stopifnot(length(statistics) == length(filenames))
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    written <- character(0)
    log_df <- data.frame(
        variable = character(0),
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
            message("Processing: ", base, " | Variable: ", stat)
        }

        if (!overwrite && file.exists(output_file)) {
            note <- "exists; skipped"
            out_norm <- normalizePath(
                output_file,
                winslash = "/",
                mustWork = FALSE
            )
            if (verbose) {
                message("  ", note, ": ", out_norm)
            }
            written <- c(written, out_norm)
            log_df <- rbind(
                log_df,
                data.frame(
                    variable = stat,
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
                r <- process_gridmet(
                    date = date_range,
                    variable = stat,
                    path = input_path
                )
                if (!inherits(r, "SpatRaster")) {
                    stop("process_gridmet did not return a SpatRaster")
                }
                if (terra::nlyr(r) == 0) {
                    stop("SpatRaster has 0 layers")
                }
                terra::writeRaster(r, output_file, overwrite = TRUE)
                list(ok = TRUE, file = output_file, nlyr = terra::nlyr(r))
            },
            error = function(e) list(ok = FALSE, msg = conditionMessage(e))
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
                    variable = stat,
                    filename = base,
                    status = "ok",
                    note = paste0("layers=", res$nlyr),
                    output_file = out_norm,
                    stringsAsFactors = FALSE
                )
            )
        } else {
            if (verbose) {
                warning("  Failed: ", base, " — ", res$msg)
            }
            log_df <- rbind(
                log_df,
                data.frame(
                    variable = stat,
                    filename = base,
                    status = "error",
                    note = res$msg,
                    output_file = "",
                    stringsAsFactors = FALSE
                )
            )
        }
    }

    log_csv <- file.path(out_dir, "gridmet_log.csv")
    utils::write.csv(log_df, log_csv, row.names = FALSE)
    written <- unique(c(
        written,
        normalizePath(log_csv, winslash = "/", mustWork = FALSE)
    ))
    return(written)
}
