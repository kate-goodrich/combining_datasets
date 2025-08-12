clean_merra2 <- function(
    years = 2011:2024,
    var_map = list(
        # Aerosol Diagnostics
        BCSMASS = "tavg1_2d_aer_Nx",
        DUSMASS25 = "tavg1_2d_aer_Nx",
        SSSMASS = "tavg1_2d_aer_Nx",
        TOTEXTTAU = "tavg1_2d_aer_Nx",

        # Surface Flux Diagnostics
        EVAP = "tavg1_2d_flx_Nx",
        PRECSNO = "tavg1_2d_flx_Nx",
        PRECTOTCORR = "tavg1_2d_flx_Nx",
        SPEED = "tavg1_2d_flx_Nx",
        Z0M = "tavg1_2d_flx_Nx",
        PBLH = "tavg1_2d_flx_Nx",

        # Land Surface Forcings
        LWGAB = "tavg1_2d_lfo_Nx",

        # Land Surface Diagnostics
        GRN = "tavg1_2d_lnd_Nx",
        GWETROOT = "tavg1_2d_lnd_Nx",
        LAI = "tavg1_2d_lnd_Nx",

        # Radiation Diagnostics
        ALBEDO = "tavg1_2d_rad_Nx",
        SWGDN = "tavg1_2d_rad_Nx",
        LWGNT = "tavg1_2d_rad_Nx",
        CLDTOT = "tavg1_2d_rad_Nx",

        # Single-Level Diagnostics
        QV2M = "tavg1_2d_slv_Nx",
        PS = "tavg1_2d_slv_Nx",
        SLP = "tavg1_2d_slv_Nx",
        T2M = "tavg1_2d_slv_Nx",
        T2MDEW = "tavg1_2d_slv_Nx",
        U10M = "tavg1_2d_slv_Nx",
        V10M = "tavg1_2d_slv_Nx",
        TS = "tavg1_2d_slv_Nx"
    ),
    raw_dir = "./raw_data/merra2",
    out_dir = "./clean_data/merra2_clean",
    overwrite = TRUE,
    use_collection_by_year = TRUE,
    verbose = TRUE
) {
    stopifnot(is.list(var_map), length(var_map) > 0)
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Helper picks collection by year if your process_merra2() supports it
    collection_for_year <- function(y) {
        if (y <= 1991) {
            "MERRA2_100"
        } else if (y <= 2000) {
            "MERRA2_200"
        } else if (y <= 2010) {
            "MERRA2_300"
        } else {
            "MERRA2_400"
        }
    }

    # Storage
    all_files <- character(0)
    log_df <- data.frame(
        variable = character(0),
        folder = character(0),
        year = integer(0),
        status = character(0),
        note = character(0),
        stringsAsFactors = FALSE
    )

    for (var in names(var_map)) {
        folder <- var_map[[var]]
        in_path <- file.path(raw_dir, folder)

        for (year in years) {
            date_range <- c(
                sprintf("%04d-01-01", year),
                sprintf("%04d-12-31", year)
            )
            out_file <- file.path(
                out_dir,
                sprintf("%s_%s_%04d.tif", var, folder, year)
            )

            if (verbose) {
                message("Processing ", var, " (", folder, "), year ", year)
            }

            # Fast existence check to avoid processing empty years
            files_year <- list.files(
                in_path,
                pattern = paste0("\\.", year, "(\\d{2}){2}\\.nc4$"),
                full.names = TRUE,
                recursive = TRUE
            )

            if (length(files_year) == 0) {
                note <- paste0("No source files under ", in_path)
                if (verbose) {
                    warning("Skipping: ", var, " ", year, " — ", note)
                }
                log_df <- rbind(
                    log_df,
                    data.frame(
                        variable = var,
                        folder = folder,
                        year = year,
                        status = "skipped_no_input",
                        note = note,
                        stringsAsFactors = FALSE
                    )
                )
                next
            }

            # Run processing
            res <- tryCatch(
                {
                    if (use_collection_by_year) {
                        coll <- collection_for_year(year)
                        process_merra2(
                            date = date_range,
                            variable = var,
                            path = in_path,
                            collection = coll
                        )
                    } else {
                        process_merra2(
                            date = date_range,
                            variable = var,
                            path = in_path
                        )
                    }
                },
                error = function(e) e
            )

            if (inherits(res, "error")) {
                note <- conditionMessage(res)
                if (verbose) {
                    warning("Failed: ", var, " ", year, " — ", note)
                }
                log_df <- rbind(
                    log_df,
                    data.frame(
                        variable = var,
                        folder = folder,
                        year = year,
                        status = "error",
                        note = note,
                        stringsAsFactors = FALSE
                    )
                )
                next
            }

            # Empty guard
            if (
                is.null(res) ||
                    (inherits(res, "SpatRaster") && terra::nlyr(res) == 0)
            ) {
                note <- "Empty result (NULL or 0 layers)"
                if (verbose) {
                    warning("Empty: ", var, " ", year, " — ", note)
                }
                log_df <- rbind(
                    log_df,
                    data.frame(
                        variable = var,
                        folder = folder,
                        year = year,
                        status = "empty",
                        note = note,
                        stringsAsFactors = FALSE
                    )
                )
                next
            }

            # Write output
            tryCatch(
                {
                    terra::writeRaster(res, out_file, overwrite = overwrite)
                    all_files <- c(all_files, out_file)
                    log_df <- rbind(
                        log_df,
                        data.frame(
                            variable = var,
                            folder = folder,
                            year = year,
                            status = "ok",
                            note = out_file,
                            stringsAsFactors = FALSE
                        )
                    )
                    if (verbose) message("Saved: ", out_file)
                },
                error = function(e) {
                    note <- conditionMessage(e)
                    if (verbose) {
                        warning("Write failed: ", var, " ", year, " — ", note)
                    }
                    log_df <<- rbind(
                        log_df,
                        data.frame(
                            variable = var,
                            folder = folder,
                            year = year,
                            status = "write_error",
                            note = note,
                            stringsAsFactors = FALSE
                        )
                    )
                }
            )
        }
    }

    # Deduplicate and normalize paths for targets
    all_files <- unique(normalizePath(
        all_files,
        winslash = "/",
        mustWork = FALSE
    ))

    # OPTIONAL: write a CSV log so we can return *only* file paths for a single target
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }
    log_csv <- file.path(out_dir, "merra2_log.csv")
    utils::write.csv(log_df, log_csv, row.names = FALSE)
    all_files <- c(
        all_files,
        normalizePath(log_csv, winslash = "/", mustWork = FALSE)
    )

    # Return just file paths so a single target can use format = "file"
    return(all_files)
}
