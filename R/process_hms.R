clean_hms <- function(
    years = 2010:2014,
    months = 1:12,
    raw_dir = "./raw_data/hms/data_files",
    out_dir = "./clean_data/hms_clean",
    overwrite = TRUE,
    verbose = TRUE
) {
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # base month lengths; adjust Feb for leap years on the fly
    days_in_month <- c(
        "01" = 31,
        "02" = 28,
        "03" = 31,
        "04" = 30,
        "05" = 31,
        "06" = 30,
        "07" = 31,
        "08" = 31,
        "09" = 30,
        "10" = 31,
        "11" = 30,
        "12" = 31
    )

    written <- character(0)
    log_df <- data.frame(
        year = integer(0),
        month = character(0),
        status = character(0),
        note = character(0),
        output_file = character(0),
        stringsAsFactors = FALSE
    )

    for (year in years) {
        for (month in months) {
            mstr <- sprintf("%02d", month)
            last_day <- if (
                mstr == "02" &&
                    ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0))
            ) {
                29
            } else {
                days_in_month[[mstr]]
            }
            if (verbose) {
                message("Processing month: ", year, "-", mstr)
            }

            out_file <- file.path(
                out_dir,
                sprintf("hms_%d_%s.gpkg", year, mstr)
            )

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
                        year = year,
                        month = mstr,
                        status = "skipped_existing",
                        note = "exists; skipped",
                        output_file = out_norm,
                        stringsAsFactors = FALSE
                    )
                )
                next
            }

            all_days <- sprintf("%04d-%02d-%02d", year, month, 1:last_day)
            month_results <- list()

            for (day in all_days) {
                hms_day <- tryCatch(
                    process_hms(date = c(day, day), path = raw_dir),
                    error = function(e) {
                        if (verbose) {
                            message("  skipped ", day, " (", e$message, ")")
                        }
                        NULL
                    }
                )

                if (inherits(hms_day, "SpatVector")) {
                    # geometry validity (safe fallback to all TRUE if method not available)
                    valid_idx <- tryCatch(
                        {
                            if ("is.valid" %in% getNamespaceExports("terra")) {
                                terra::is.valid(hms_day)
                            } else {
                                rep(TRUE, nrow(hms_day))
                            }
                        },
                        error = function(e) rep(TRUE, nrow(hms_day))
                    )

                    if (length(valid_idx) == nrow(hms_day) && any(valid_idx)) {
                        month_results[[length(month_results) + 1]] <- hms_day[
                            valid_idx,
                        ]
                    }
                }
            }

            # Combine and write month
            if (length(month_results) > 0) {
                combined <- tryCatch(
                    {
                        do.call(terra::rbind, month_results)
                    },
                    error = function(e) {
                        if (verbose) {
                            warning(
                                "  rbind failed for ",
                                year,
                                "-",
                                mstr,
                                ": ",
                                e$message
                            )
                        }
                        NULL
                    }
                )

                if (!is.null(combined) && nrow(combined) > 0) {
                    tryCatch(
                        {
                            terra::writeVector(
                                combined,
                                out_file,
                                filetype = "GPKG",
                                overwrite = TRUE
                            )
                            out_norm <- normalizePath(
                                out_file,
                                winslash = "/",
                                mustWork = FALSE
                            )
                            if (verbose) {
                                message(
                                    "  Saved: ",
                                    out_norm,
                                    " (features=",
                                    nrow(combined),
                                    ")"
                                )
                            }
                            written <- c(written, out_norm)
                            log_df <- rbind(
                                log_df,
                                data.frame(
                                    year = year,
                                    month = mstr,
                                    status = "ok",
                                    note = paste0("features=", nrow(combined)),
                                    output_file = out_norm,
                                    stringsAsFactors = FALSE
                                )
                            )
                        },
                        error = function(e) {
                            if (verbose) {
                                warning("  write failed: ", e$message)
                            }
                            log_df <- rbind(
                                log_df,
                                data.frame(
                                    year = year,
                                    month = mstr,
                                    status = "write_error",
                                    note = e$message,
                                    output_file = "",
                                    stringsAsFactors = FALSE
                                )
                            )
                        }
                    )
                } else {
                    if (verbose) {
                        message(
                            "  No valid features after combine in ",
                            year,
                            "-",
                            mstr
                        )
                    }
                    log_df <- rbind(
                        log_df,
                        data.frame(
                            year = year,
                            month = mstr,
                            status = "empty_combined",
                            note = "no valid features after combine",
                            output_file = "",
                            stringsAsFactors = FALSE
                        )
                    )
                }
            } else {
                if (verbose) {
                    message("  No valid smoke plumes in ", year, "-", mstr)
                }
                log_df <- rbind(
                    log_df,
                    data.frame(
                        year = year,
                        month = mstr,
                        status = "no_valid_days",
                        note = "no valid daily SpatVectors",
                        output_file = "",
                        stringsAsFactors = FALSE
                    )
                )
            }
        }
    }

    # Write log CSV and return paths for {targets}
    log_csv <- file.path(out_dir, "hms_log.csv")
    utils::write.csv(log_df, log_csv, row.names = FALSE)
    unique(c(written, normalizePath(log_csv, winslash = "/", mustWork = FALSE)))
}
