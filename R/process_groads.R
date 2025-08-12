clean_groads <- function(
    in_path = "./raw_data/groads/data_files/groads-v1-americas-shp/gROADS-v1-americas.shp",
    out_dir = "./clean_data/groads_clean",
    out_name = "groads_clean.gpkg",
    overwrite = TRUE,
    verbose = TRUE
) {
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }
    out_file <- file.path(out_dir, out_name)

    log_df <- data.frame(
        status = character(0),
        note = character(0),
        output_file = character(0),
        stringsAsFactors = FALSE
    )

    # Idempotent skip
    if (!overwrite && file.exists(out_file)) {
        out_norm <- normalizePath(out_file, winslash = "/", mustWork = FALSE)
        if (verbose) {
            message("Exists; skipped: ", out_norm)
        }
        log_df <- rbind(
            log_df,
            data.frame(
                status = "skipped_existing",
                note = "exists; skipped",
                output_file = out_norm,
                stringsAsFactors = FALSE
            )
        )
    } else {
        res <- tryCatch(
            {
                v <- process_groads(path = in_path)
                if (!inherits(v, "SpatVector")) {
                    stop("process_groads did not return a SpatVector")
                }
                if (nrow(v) == 0) {
                    stop("SpatVector has 0 features")
                }
                terra::writeVector(v, filename = out_file, overwrite = TRUE)
                list(ok = TRUE, n = nrow(v))
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
                message("Saved: ", out_norm, " (features=", res$n, ")")
            }
            log_df <- rbind(
                log_df,
                data.frame(
                    status = "ok",
                    note = paste0("features=", res$n),
                    output_file = out_norm,
                    stringsAsFactors = FALSE
                )
            )
        } else {
            warning("Failed: ", res$msg)
            log_df <- rbind(
                log_df,
                data.frame(
                    status = "error",
                    note = res$msg,
                    output_file = "",
                    stringsAsFactors = FALSE
                )
            )
        }
    }

    # Write a CSV log and return file paths for {targets}
    log_csv <- file.path(out_dir, "groads_log.csv")
    utils::write.csv(log_df, log_csv, row.names = FALSE)

    unique(c(
        normalizePath(out_file, winslash = "/", mustWork = FALSE)[file.exists(
            out_file
        )],
        normalizePath(log_csv, winslash = "/", mustWork = FALSE)
    ))
}
