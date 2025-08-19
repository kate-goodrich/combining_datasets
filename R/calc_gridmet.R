# gridmet_zonal.R
# One function for county/tract zonal means from gridMET (or similar) GeoTIFFs
# Compatible with {targets}. No side effects unless write_csv is provided.

.libPaths("/usr/local/lib/R/site-library")

library(terra) # raster IO
library(sf) # vector IO
library(exactextractr) # fast, area-weighted zonal stats
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(tidyr)

gridmet_zonal <- function(
    tif_dir,
    level = c("county", "tract"),
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    zone_layer = NULL, # if NULL, inferred from `level`
    id_col = "geoid",
    files_pattern = "\\.tif$",
    aggregate_to = c("annual", "monthly"), # <- updated
    chunk_size = 365, # layers per chunk for multi-layer rasters
    write_csv = NULL # optional: path to write CSV
) {
    # ---- Argument handling ----
    level <- match.arg(level)
    aggregate_to <- match.arg(aggregate_to)
    if (is.null(zone_layer)) {
        zone_layer <- if (level == "county") "counties_500k" else "tracts_500k"
    }

    # ---- Dependency checks ----
    reqs <- c(
        "terra",
        "sf",
        "exactextractr",
        "dplyr",
        "stringr",
        "purrr",
        "readr",
        "tidyr"
    )
    ok <- vapply(reqs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
    if (!all(ok)) {
        stop(
            "Missing packages: ",
            paste(reqs[!ok], collapse = ", "),
            call. = FALSE
        )
    }

    # ---- Helpers ----
    dates_from_names <- function(nm) {
        # Try several common encodings in layer names
        # 1) X2011.12.31 or 2011.12.31 -> 2011-12-31
        d_try <- suppressWarnings(as.Date(sub("^X", "", gsub("\\.", "-", nm))))
        if (any(!is.na(d_try))) {
            return(d_try)
        }

        # 2) suffix _YYYYMMDD
        m <- stringr::str_match(nm, ".*_(\\d{8})$")[, 2]
        out <- rep(as.Date(NA), length(nm))
        ok <- !is.na(m)
        out[ok] <- as.Date(m[ok], "%Y%m%d")
        out
    }

    date_from_path <- function(path) {
        # Pull YYYY-MM-DD or YYYYMMDD from the *filename*
        b <- basename(path)
        m <- stringr::str_extract(b, "\\d{4}[-_.]?\\d{2}[-_.]?\\d{2}")
        if (is.na(m)) {
            return(as.Date(NA))
        }
        m <- gsub("[-_.]", "", m)
        as.Date(m, "%Y%m%d")
    }

    layer_dates <- function(r) {
        tt <- tryCatch(terra::time(r), error = function(e) NULL)
        if (!is.null(tt) && length(tt) == terra::nlyr(r)) {
            # Keep UTC to avoid off-by-one around midnight
            return(tibble::tibble(
                layer = names(r),
                date = as.Date(tt, tz = "UTC")
            ))
        }
        tibble::tibble(layer = names(r), date = dates_from_names(names(r)))
    }

    var_from_r <- function(r) sub("_\\d{8}$", "", names(r)[1])

    means_for_file <- function(tif, zones_sf, id_col, chunk_size) {
        r <- terra::rast(tif)
        zones_r <- sf::st_transform(zones_sf, terra::crs(r))
        vname <- var_from_r(r)

        # --- Single-layer fast path ---
        if (terra::nlyr(r) == 1) {
            vals <- exactextractr::exact_extract(r, zones_r, "mean")
            d <- layer_dates(r)$date[1]
            if (is.na(d)) {
                d <- date_from_path(tif)
            } # fallback to filename
            return(dplyr::tibble(
                !!id_col := zones_sf[[id_col]],
                date = d,
                var = vname,
                value = as.numeric(vals)
            ))
        }

        # --- Multi-layer path (chunked) ---
        key_all <- layer_dates(r) # (layer, date)
        idx <- seq_len(terra::nlyr(r))
        chunks <- split(idx, ceiling(idx / chunk_size))

        out <- vector("list", length(chunks))
        for (i in seq_along(chunks)) {
            ids <- chunks[[i]]
            r_chunk <- r[[ids]]

            vals <- exactextractr::exact_extract(r_chunk, zones_r, "mean")
            df <- tibble::as_tibble(vals)

            # Force layer columns to match raster layer names exactly
            lyr_names <- names(r_chunk)
            if (ncol(df) != length(lyr_names)) {
                stop(
                    "Unexpected exact_extract() shape: got ",
                    ncol(df),
                    " columns for ",
                    length(lyr_names),
                    " layers."
                )
            }
            names(df) <- lyr_names

            df[[id_col]] <- zones_sf[[id_col]]

            long <- tidyr::pivot_longer(
                df,
                cols = dplyr::all_of(lyr_names),
                names_to = "layer",
                values_to = "value"
            )

            key <- tibble::tibble(layer = lyr_names) |>
                dplyr::left_join(key_all, by = "layer")

            out[[i]] <- long |>
                dplyr::left_join(key, by = "layer") |>
                dplyr::transmute(
                    !!id_col := .data[[id_col]],
                    date,
                    var = vname,
                    value = .data[["value"]]
                )
        }

        dplyr::bind_rows(out)
    }

    # ---- Read zones ----
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

    # ---- List GeoTIFFs ----
    tifs <- list.files(tif_dir, pattern = files_pattern, full.names = TRUE)
    if (length(tifs) == 0L) {
        stop(
            "No GeoTIFFs found in '",
            tif_dir,
            "' matching pattern '",
            files_pattern,
            "'.",
            call. = FALSE
        )
    }

    # ---- Extract (zone x date x var) ----
    res <- purrr::map_dfr(
        tifs,
        means_for_file,
        zones_sf = zones,
        id_col = id_col,
        chunk_size = chunk_size
    )

    # Both monthly and annual need valid dates
    if (anyNA(res$date)) {
        stop(
            "Some layer dates are NA. Ensure the raster has a time axis or layer names end with _YYYYMMDD.",
            call. = FALSE
        )
    }

    # ---- Aggregate ----
    if (aggregate_to == "annual") {
        out <- res |>
            dplyr::mutate(year = as.integer(format(.data$date, "%Y"))) |>
            dplyr::group_by(.data[[id_col]], .data$var, .data$year) |>
            dplyr::summarise(
                value = mean(.data$value, na.rm = TRUE),
                .groups = "drop"
            ) |>
            dplyr::arrange(.data[[id_col]], .data$var, .data$year)
    } else {
        # monthly
        out <- res |>
            dplyr::mutate(
                year = as.integer(format(.data$date, "%Y")),
                month = as.integer(format(.data$date, "%m"))
            ) |>
            dplyr::group_by(
                .data[[id_col]],
                .data$var,
                .data$year,
                .data$month
            ) |>
            dplyr::summarise(
                value = mean(.data$value, na.rm = TRUE),
                .groups = "drop"
            ) |>
            dplyr::arrange(.data[[id_col]], .data$var, .data$year, .data$month)
    }

    # ---- Optional write ----
    if (!is.null(write_csv)) {
        readr::write_csv(out, write_csv)
    }

    out
}
