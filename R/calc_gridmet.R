gridmet_zonal <- function(
    tif_dir,
    level = c("county", "tract", "zip"),
    zones_gpkg = "clean_data/county_census_zip/canonical_2024.gpkg",
    zone_layer = NULL,
    id_col = "geoid",
    files_pattern = "\\.tif$",
    aggregate_to = c("annual", "monthly", "overall"),
    chunk_size = 365, # retained but rarely needed now
    write_csv = NULL,
    exclude_vars = c("pet", "srad") # <— exclude from GridMET output
) {
    level <- match.arg(level)
    aggregate_to <- match.arg(aggregate_to)
    if (is.null(zone_layer)) {
        zone_layer <- switch(
            level,
            county = "counties_500k",
            tract = "tracts_500k",
            zip = "zctas_500k"
        )
    }

    # ---- helpers ----
    dates_from_names <- function(nm) {
        d_try <- suppressWarnings(as.Date(sub("^X", "", gsub("\\.", "-", nm))))
        if (any(!is.na(d_try))) {
            return(d_try)
        }
        m <- stringr::str_match(nm, ".*_(\\d{8})$")[, 2]
        out <- rep(as.Date(NA), length(nm))
        ok <- !is.na(m)
        out[ok] <- as.Date(m[ok], "%Y%m%d")
        out
    }
    layer_dates <- function(r) {
        tt <- tryCatch(terra::time(r), error = function(e) NULL)
        if (!is.null(tt) && length(tt) == terra::nlyr(r)) {
            return(tibble::tibble(
                layer = names(r),
                date = as.Date(tt, tz = "UTC")
            ))
        }
        tibble::tibble(layer = names(r), date = dates_from_names(names(r)))
    }
    var_from_r <- function(r) tolower(sub("_\\d{8}$", "", names(r)[1])) # ensure lower-case

    # Optional: fast variable name from filename to skip early (speeds things up)
    var_from_path <- function(path) {
        bn <- basename(path)
        # typical pattern like "srad_YYYYMMDD.tif" or "pet_YYYYMMDD.tif"
        m <- stringr::str_match(bn, "^([a-zA-Z0-9]+)_")[, 2]
        tolower(ifelse(is.na(m), tools::file_path_sans_ext(bn), m))
    }

    # ---- zones ----
    zones <- sf::st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid()
    if (!id_col %in% names(zones)) {
        stop(
            "Column '",
            id_col,
            "' not found in zones layer '",
            zone_layer,
            "'.",
            call. = FALSE
        )
    }

    # ---- list tifs ----
    tifs <- list.files(tif_dir, pattern = files_pattern, full.names = TRUE)
    if (length(tifs) == 0L) {
        stop(
            "No GeoTIFFs found in '",
            tif_dir,
            "' matching '",
            files_pattern,
            "'.",
            call. = FALSE
        )
    }

    # Early skip (faster): drop files whose inferred var is in exclude list
    if (length(exclude_vars)) {
        inferred <- vapply(tifs, var_from_path, character(1))
        keep <- !(inferred %in% tolower(exclude_vars))
        tifs <- tifs[keep]
    }
    if (length(tifs) == 0L) {
        # Nothing to process after exclusion
        out <- tibble::tibble(
            !!id_col := zones[[id_col]],
            var = character(),
            year = integer(),
            month = integer(),
            value = double()
        )[0, ]
        if (!is.null(write_csv)) {
            readr::write_csv(out, write_csv)
        }
        return(out)
    }

    # ---- per-file worker (pre-aggregate in raster space) ----
    means_for_file <- function(tif, zones_sf, id_col, aggregate_to) {
        r <- terra::rast(tif)
        vname <- var_from_r(r)

        # Hard skip (defensive): if the raster-derived var is excluded, return empty
        if (length(exclude_vars) && vname %in% tolower(exclude_vars)) {
            return(tibble::tibble(
                !!id_col := zones_sf[[id_col]],
                var = character(),
                year = integer(),
                month = integer(),
                value = double()
            )[0, ])
        }

        zones_r <- sf::st_transform(zones_sf, terra::crs(r))

        # dates for layers
        key <- layer_dates(r)
        if (anyNA(key$date)) {
            stop(
                "Some layer dates are NA in ",
                basename(tif),
                ". Ensure time axis or layer names end with _YYYYMMDD.",
                call. = FALSE
            )
        }

        # area weights (true cell areas; CRS-agnostic)
        w <- terra::cellSize(r, unit = "m")

        # limit to 2010-2024 for "overall"
        yrs_all <- as.integer(format(key$date, "%Y"))
        keep_overall <- yrs_all >= 2010 & yrs_all <= 2024

        if (terra::nlyr(r) == 1L) {
            vals <- exactextractr::exact_extract(
                r,
                zones_r,
                fun = "weighted_mean",
                weights = w
            )
            dt <- key$date[1]
            if (aggregate_to == "annual") {
                return(tibble::tibble(
                    !!id_col := zones_sf[[id_col]],
                    var = vname,
                    year = as.integer(format(dt, "%Y")),
                    value = as.numeric(vals)
                ))
            } else if (aggregate_to == "monthly") {
                return(tibble::tibble(
                    !!id_col := zones_sf[[id_col]],
                    var = vname,
                    year = as.integer(format(dt, "%Y")),
                    month = as.integer(format(dt, "%m")),
                    value = as.numeric(vals)
                ))
            } else {
                if (dt < as.Date("2010-01-01") || dt > as.Date("2024-12-31")) {
                    return(tibble::tibble(
                        !!id_col := zones_sf[[id_col]],
                        var = vname,
                        value = NA_real_
                    ))
                }
                return(tibble::tibble(
                    !!id_col := zones_sf[[id_col]],
                    var = vname,
                    value = as.numeric(vals)
                ))
            }
        }

        # multi-layer stacks
        if (aggregate_to == "annual") {
            idx <- as.integer(format(key$date, "%Y"))
            r_agg <- terra::tapp(r, index = idx, fun = mean, na.rm = TRUE)
            uyrs <- sort(unique(idx))
            names(r_agg) <- paste0("y", uyrs)

            vals <- exactextractr::exact_extract(
                r_agg,
                zones_r,
                fun = "weighted_mean",
                weights = terra::cellSize(r_agg, unit = "m")
            )
            df <- tibble::as_tibble(vals)
            names(df) <- names(r_agg)
            df[[id_col]] <- zones_sf[[id_col]]

            long <- tidyr::pivot_longer(
                df,
                -tidyr::all_of(id_col),
                names_to = "layer",
                values_to = "value"
            ) |>
                dplyr::mutate(
                    year = as.integer(sub("^y", "", layer)),
                    var = vname
                ) |>
                dplyr::select(dplyr::all_of(id_col), var, year, value)
            return(long)
        } else if (aggregate_to == "monthly") {
            ym <- as.integer(format(key$date, "%Y")) *
                100L +
                as.integer(format(key$date, "%m"))
            r_agg <- terra::tapp(r, index = ym, fun = mean, na.rm = TRUE)
            uym <- sort(unique(ym))
            names(r_agg) <- paste0("m", uym)

            vals <- exactextractr::exact_extract(
                r_agg,
                zones_r,
                fun = "weighted_mean",
                weights = terra::cellSize(r_agg, unit = "m")
            )
            df <- tibble::as_tibble(vals)
            names(df) <- names(r_agg)
            df[[id_col]] <- zones_sf[[id_col]]

            long <- tidyr::pivot_longer(
                df,
                -tidyr::all_of(id_col),
                names_to = "layer",
                values_to = "value"
            ) |>
                dplyr::mutate(
                    key = as.integer(sub("^m", "", layer)),
                    year = key %/% 100L,
                    month = key %% 100L,
                    var = vname
                ) |>
                dplyr::select(dplyr::all_of(id_col), var, year, month, value)
            return(long)
        } else {
            # overall
            if (!any(keep_overall)) {
                return(tibble::tibble(
                    !!id_col := zones_sf[[id_col]],
                    var = vname,
                    value = NA_real_
                ))
            }
            r_keep <- r[[which(keep_overall)]]
            r_mean <- terra::app(r_keep, fun = mean, na.rm = TRUE)
            vals <- exactextractr::exact_extract(
                r_mean,
                zones_r,
                fun = "weighted_mean",
                weights = terra::cellSize(r_mean, unit = "m")
            )
            return(tibble::tibble(
                !!id_col := zones_sf[[id_col]],
                var = vname,
                value = as.numeric(vals)
            ))
        }
    }

    # ---- run per file, binding much smaller pieces ----
    out <- purrr::map_dfr(
        tifs,
        means_for_file,
        zones_sf = zones,
        id_col = id_col,
        aggregate_to = aggregate_to
    )

    # ---- write / return ----
    if (!is.null(write_csv)) {
        readr::write_csv(out, write_csv)
    }
    out
}
