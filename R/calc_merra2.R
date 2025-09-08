merra2_summary <- function(
    tif_dir = "clean_data/merra2_clean",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = c("county", "tract", "zip"),
    zone_layer = NULL,
    agg = c("annual", "monthly", "overall"),
    id_col = "geoid",
    write_csv = NULL
) {
    level <- match.arg(level)
    agg <- match.arg(agg)

    if (is.null(zone_layer)) {
        zone_layer <- switch(
            level,
            county = "counties_500k",
            tract = "tracts_500k",
            zip = "zctas_500k"
        )
    }

    # Load zones and ensure valid geometry
    zones <- sf::st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid() |>
        dplyr::select(!!rlang::sym(id_col))

    # Helper: parse filename metadata
    parse_var_year <- function(bn) {
        tibble::tibble(
            file = bn,
            var = stringr::str_extract(bn, "^[^_]+"),
            year = as.integer(stringr::str_match(bn, "(\\d{4})\\.tif$")[, 2])
        )
    }

    # Summarize one raster file
    summarise_one_file <- function(tif_path, agg_local) {
        bn <- basename(tif_path)
        meta <- parse_var_year(bn)
        r <- terra::rast(tif_path)
        zones_rcrs <- sf::st_transform(zones, terra::crs(r))

        timestamps <- terra::time(r)

        if (agg_local == "annual") {
            # mean over all time layers in the file (year)
            r_agg <- terra::app(r, mean, na.rm = TRUE)
            w <- terra::cellSize(r_agg, unit = "m")

            vals <- exactextractr::exact_extract(
                r_agg,
                zones_rcrs,
                fun = "weighted_mean",
                weights = w,
                progress = FALSE
            )

            return(tibble::tibble(
                !!id_col := zones_rcrs[[id_col]],
                var = meta$var,
                year = meta$year,
                value = as.numeric(vals)
            ))
        } else if (agg_local == "monthly") {
            if (is.null(timestamps)) {
                stop("Monthly aggregation requires embedded timestamps.")
            }

            month_keys <- format(timestamps, "%Y-%m")
            month_groups <- split(seq_along(timestamps), month_keys)

            r_months <- terra::tapp(
                r,
                index = month_groups,
                fun = mean,
                na.rm = TRUE
            )
            names(r_months) <- names(month_groups)

            # weights (same grid for each monthly layer)
            w <- terra::cellSize(r_months[[1]], unit = "m")

            df <- purrr::map_dfr(
                seq_len(terra::nlyr(r_months)),
                function(i) {
                    vals <- exactextractr::exact_extract(
                        r_months[[i]],
                        zones_rcrs,
                        fun = "weighted_mean",
                        weights = w,
                        progress = FALSE
                    )
                    tibble::tibble(
                        !!id_col := zones_rcrs[[id_col]],
                        var = meta$var,
                        year = as.integer(substr(names(r_months)[i], 1, 4)),
                        month = as.integer(substr(names(r_months)[i], 6, 7)),
                        value = as.numeric(vals)
                    )
                }
            )
            return(df)
        }
    }

    tif_files <- list.files(tif_dir, pattern = "\\.tif$", full.names = TRUE)
    if (length(tif_files) == 0L) {
        stop("No .tif files found in '", tif_dir, "'.", call. = FALSE)
    }

    # For 'overall', compute annual first, then average across 2010–2024
    agg_local <- if (agg == "overall") "annual" else agg

    results <- purrr::map_dfr(
        tif_files,
        summarise_one_file,
        agg_local = agg_local
    )

    if (agg == "overall") {
        results <- results |>
            dplyr::filter(year >= 2010, year <= 2024) |>
            dplyr::group_by(.data[[id_col]], var) |>
            dplyr::summarise(
                value = mean(value, na.rm = TRUE),
                .groups = "drop"
            ) |>
            dplyr::arrange(.data[[id_col]], var)
    }

    if (!is.null(write_csv)) {
        readr::write_csv(results, write_csv)
    }

    return(results)
}
