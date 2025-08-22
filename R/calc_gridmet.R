gridmet_zonal <- function(
    tif_dir,
    level = c("county", "tract"),
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    zone_layer = NULL,
    id_col = "geoid",
    files_pattern = "\\.tif$",
    aggregate_to = c("annual", "monthly"),
    chunk_size = 365, # retained but rarely needed now
    write_csv = NULL
) {
    level <- match.arg(level)
    aggregate_to <- match.arg(aggregate_to)
    if (is.null(zone_layer)) {
        zone_layer <- if (level == "county") "counties_500k" else "tracts_500k"
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
            return(tibble(layer = names(r), date = as.Date(tt, tz = "UTC")))
        }
        tibble(layer = names(r), date = dates_from_names(names(r)))
    }
    var_from_r <- function(r) sub("_\\d{8}$", "", names(r)[1])

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

    # ---- per-file worker (pre-aggregate in raster space) ----
    means_for_file <- function(tif, zones_sf, id_col, aggregate_to) {
        r <- terra::rast(tif)
        vname <- var_from_r(r)
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

        if (terra::nlyr(r) == 1L) {
            # single-layer: treat as one "month" (or annual) datapoint
            vals <- exactextractr::exact_extract(r, zones_r, "mean")
            dt <- key$date[1]
            out <- tibble(
                !!id_col := zones_sf[[id_col]],
                var = vname,
                date = dt,
                value = as.numeric(vals)
            )
            if (aggregate_to == "annual") {
                out <- out |>
                    mutate(year = as.integer(format(date, "%Y"))) |>
                    select(all_of(id_col), var, year, value)
            } else {
                out <- out |>
                    mutate(
                        year = as.integer(format(date, "%Y")),
                        month = as.integer(format(date, "%m"))
                    ) |>
                    select(all_of(id_col), var, year, month, value)
            }
            return(out)
        }

        # Build index for tapp
        yrs <- as.integer(format(key$date, "%Y"))
        if (aggregate_to == "annual") {
            idx <- yrs
            r_agg <- terra::tapp(r, index = idx, fun = mean, na.rm = TRUE)
            # name layers as YYYYY
            uyrs <- sort(unique(yrs))
            names(r_agg) <- paste0("y", uyrs)
            # exact extract on the small stack
            vals <- exactextractr::exact_extract(r_agg, zones_r, "mean")
            df <- as_tibble(vals)
            names(df) <- names(r_agg)
            df[[id_col]] <- zones_sf[[id_col]]
            long <- pivot_longer(
                df,
                -all_of(id_col),
                names_to = "layer",
                values_to = "value"
            ) |>
                mutate(year = as.integer(sub("^y", "", layer)), var = vname) |>
                select(all_of(id_col), var, year, value)
            return(long)
        } else {
            # monthly: index by year*100 + month to keep months separate
            ym <- as.integer(format(key$date, "%Y")) *
                100L +
                as.integer(format(key$date, "%m"))
            r_agg <- terra::tapp(r, index = ym, fun = mean, na.rm = TRUE)
            uym <- sort(unique(ym))
            names(r_agg) <- paste0("m", uym) # e.g., m201601

            vals <- exactextractr::exact_extract(r_agg, zones_r, "mean")
            df <- as_tibble(vals)
            names(df) <- names(r_agg)
            df[[id_col]] <- zones_sf[[id_col]]
            long <- pivot_longer(
                df,
                -all_of(id_col),
                names_to = "layer",
                values_to = "value"
            ) |>
                mutate(
                    key = as.integer(sub("^m", "", layer)),
                    year = key %/% 100L,
                    month = key %% 100L,
                    var = vname
                ) |>
                select(all_of(id_col), var, year, month, value)
            return(long)
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
        # write once at the end (now much smaller)
        readr::write_csv(out, write_csv)
    }
    out
}
