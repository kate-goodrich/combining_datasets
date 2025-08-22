clean_tri <- function(
    years = 2010:2024,
    variables = c(
        1,
        12,
        13,
        14,
        20,
        29,
        34,
        36,
        38,
        39,
        40,
        47,
        48,
        49,
        50,
        51,
        -52,
        -53,
        -65,
        88,
        107
    ),
    raw_dir = "./raw_data/tri",
    out_dir = "./clean_data/tri_clean",
    overwrite = TRUE,
    verbose = TRUE
) {
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    written <- character(0)
    log_df <- data.frame(
        year = integer(0),
        status = character(0),
        note = character(0),
        output_file = character(0),
        stringsAsFactors = FALSE
    )

    for (yr in years) {
        if (verbose) {
            message("Processing TRI data for year: ", yr)
        }
        out_file <- file.path(out_dir, sprintf("tri_%d_processed.gpkg", yr))

        # Idempotent skip if file exists and not overwriting
        if (!overwrite && file.exists(out_file)) {
            note <- "exists; skipped"
            if (verbose) {
                message("  ", note, ": ", out_file)
            }
            log_df <- rbind(
                log_df,
                data.frame(
                    year = yr,
                    status = "skipped_existing",
                    note = note,
                    output_file = normalizePath(
                        out_file,
                        winslash = "/",
                        mustWork = FALSE
                    ),
                    stringsAsFactors = FALSE
                )
            )
            written <- c(
                written,
                normalizePath(out_file, winslash = "/", mustWork = FALSE)
            )
            next
        }

        # Do the work
        res <- tryCatch(
            {
                tri_data <- process_tri(
                    path = raw_dir,
                    year = yr,
                    variables = variables
                )

                if (!inherits(tri_data, "SpatVector")) {
                    stop("process_tri did not return a SpatVector")
                }

                terra::writeVector(tri_data, out_file, overwrite = TRUE)
                list(ok = TRUE, n = nrow(tri_data), file = out_file)
            },
            interrupt = function(e) {
                list(ok = NA, interrupt = TRUE, msg = "User interrupt")
            },
            error = function(e) {
                list(ok = FALSE, msg = conditionMessage(e))
            }
        )

        # Handle outcomes
        if (isTRUE(res$ok)) {
            if (verbose) {
                message("  Saved: ", res$file, " (", res$n, " features)")
            }
            written <- c(
                written,
                normalizePath(res$file, winslash = "/", mustWork = FALSE)
            )
            log_df <- rbind(
                log_df,
                data.frame(
                    year = yr,
                    status = "ok",
                    note = paste0("features=", res$n),
                    output_file = normalizePath(
                        res$file,
                        winslash = "/",
                        mustWork = FALSE
                    ),
                    stringsAsFactors = FALSE
                )
            )
        } else if (!is.null(res$interrupt) && res$interrupt) {
            if (verbose) {
                message(
                    "  Detected user interrupt; stopping after year ",
                    yr,
                    "."
                )
            }
            log_df <- rbind(
                log_df,
                data.frame(
                    year = yr,
                    status = "interrupted",
                    note = "User interrupt",
                    output_file = "",
                    stringsAsFactors = FALSE
                )
            )
            break
        } else {
            if (verbose) {
                warning("  Failed year ", yr, ": ", res$msg)
            }
            log_df <- rbind(
                log_df,
                data.frame(
                    year = yr,
                    status = "error",
                    note = res$msg,
                    output_file = "",
                    stringsAsFactors = FALSE
                )
            )
        }
    }

    # Write log CSV and include it in returned file list
    log_csv <- file.path(out_dir, "tri_log.csv")
    utils::write.csv(log_df, log_csv, row.names = FALSE)
    written <- unique(c(
        written,
        normalizePath(log_csv, winslash = "/", mustWork = FALSE)
    ))

    return(written)
}
