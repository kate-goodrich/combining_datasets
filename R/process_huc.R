clean_huc_layers <- function(
    gdb_path = "./raw_data/huc/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb",
    layers = c(
        "BurnAddWaterbody",
        "LandSea",
        "Catchment",
        "CatchmentSP",
        "NHDArea",
        "NHDWaterbody",
        "HUC12"
    ),
    huc_level = "huc12",
    out_dir = "clean_data/huc_clean",
    overwrite = TRUE,
    verbose = TRUE
) {
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    written <- character(0)
    log_df <- data.frame(
        layer = character(0),
        status = character(0),
        note = character(0),
        output_file = character(0),
        stringsAsFactors = FALSE
    )

    for (layer_name in layers) {
        if (verbose) {
            message("Processing layer: ", layer_name)
        }
        out_file <- file.path(out_dir, paste0(layer_name, ".gpkg"))

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
                    layer = layer_name,
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
                v <- process_huc(
                    path = gdb_path,
                    layer_name = layer_name,
                    huc_level = huc_level
                )
                if (!inherits(v, "SpatVector")) {
                    stop("process_huc did not return a SpatVector")
                }
                if (nrow(v) == 0) {
                    stop("SpatVector has 0 features")
                }

                # Optional: filter invalid geometries if available
                valid_idx <- tryCatch(
                    {
                        if ("is.valid" %in% getNamespaceExports("terra")) {
                            terra::is.valid(v)
                        } else {
                            rep(TRUE, nrow(v))
                        }
                    },
                    error = function(e) rep(TRUE, nrow(v))
                )

                if (any(valid_idx)) {
                    v <- v[valid_idx, ]
                }

                terra::writeVector(
                    v,
                    out_file,
                    filetype = "GPKG",
                    overwrite = TRUE
                )
                list(ok = TRUE, n = nrow(v), file = out_file)
            },
            error = function(e) list(ok = FALSE, msg = conditionMessage(e))
        )

        if (isTRUE(res$ok)) {
            out_norm <- normalizePath(
                res$file,
                winslash = "/",
                mustWork = FALSE
            )
            if (verbose) {
                message("  Saved: ", out_norm, " (features=", res$n, ")")
            }
            written <- c(written, out_norm)
            log_df <- rbind(
                log_df,
                data.frame(
                    layer = layer_name,
                    status = "ok",
                    note = paste0("features=", res$n),
                    output_file = out_norm,
                    stringsAsFactors = FALSE
                )
            )
        } else {
            if (verbose) {
                warning("  Failed: ", layer_name, " — ", res$msg)
            }
            log_df <- rbind(
                log_df,
                data.frame(
                    layer = layer_name,
                    status = "error",
                    note = res$msg,
                    output_file = "",
                    stringsAsFactors = FALSE
                )
            )
        }
    }

    # Write log CSV and return paths for {targets}
    log_csv <- file.path(out_dir, "huc_log.csv")
    utils::write.csv(log_df, log_csv, row.names = FALSE)
    unique(c(written, normalizePath(log_csv, winslash = "/", mustWork = FALSE)))
}
