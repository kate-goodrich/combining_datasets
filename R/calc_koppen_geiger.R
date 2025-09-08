koppen_geiger_summary <- function(
    raster_path_koppen = "clean_data/koppen_geiger_clean/Beck_KG_V1_present_0p083_processed.tif",
    raster_path_conf = "clean_data/koppen_geiger_clean/Beck_KG_V1_present_conf_0p083_processed.tif",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = c("county", "tract", "zip"),
    zone_layer = NULL,
    id_col = "geoid",
    write_csv = NULL
) {
    level <- match.arg(level)
    if (is.null(zone_layer)) {
        zone_layer <- switch(
            level,
            county = "counties_500k",
            tract = "tracts_500k",
            zip = "zctas_500k"
        )
    }

    # --- Required packages ---
    reqs <- c(
        "terra",
        "sf",
        "exactextractr",
        "dplyr",
        "tidyr",
        "readr",
        "tibble"
    )
    ok <- vapply(reqs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
    if (!all(ok)) {
        stop(
            "Missing packages: ",
            paste(reqs[!ok], collapse = ", "),
            call. = FALSE
        )
    }

    # --- Read spatial zones ---
    zones <- sf::st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid() |>
        dplyr::select(!!id_col := dplyr::all_of(id_col))

    # --- Load rasters ---
    koppen_rast <- terra::rast(raster_path_koppen)
    conf_rast <- terra::rast(raster_path_conf)

    # --- Reproject zones to match raster CRS ---
    zones <- sf::st_transform(zones, terra::crs(koppen_rast))

    # --- Extract Koppen code proportions (per zone) ---
    koppen_props_list <- exactextractr::exact_extract(
        koppen_rast,
        zones,
        function(values, coverage_fraction) {
            mask <- values != 0
            values <- values[mask]
            coverage_fraction <- coverage_fraction[mask]
            if (!length(values)) {
                return(NULL)
            }
            counts <- tapply(coverage_fraction, values, sum)
            counts / sum(counts)
        }
    )

    # --- Extract confidence (weighted mean by coverage fraction) ---
    avg_conf <- exactextractr::exact_extract(
        conf_rast,
        zones,
        function(values, coverage_fraction) {
            if (all(is.na(values))) {
                return(NA_real_)
            }
            stats::weighted.mean(values, coverage_fraction, na.rm = TRUE)
        }
    )

    # --- Build long-form tibble ---
    props_df <- tibble::tibble(
        !!id_col := zones[[id_col]],
        props = koppen_props_list
    ) |>
        tidyr::unnest_wider(props, names_sep = "_") |>
        tidyr::pivot_longer(
            cols = tidyselect::starts_with("props_"),
            names_prefix = "props_",
            names_to = "koppen_code",
            values_to = "value",
            values_drop_na = TRUE
        ) |>
        dplyr::mutate(
            var = paste0("koppen_", koppen_code),
            value = as.numeric(value)
        ) |>
        dplyr::select(!!id_col, var, value)

    # --- Confidence as separate variable ---
    conf_df <- tibble::tibble(
        !!id_col := zones[[id_col]],
        var = "koppen_confidence",
        value = avg_conf
    )

    # --- Final table (static -> tag as overall) ---
    out <- dplyr::bind_rows(props_df, conf_df) |>
        dplyr::mutate(agg = "overall") |>
        dplyr::arrange(.data[[id_col]], var)

    # --- Optional write ---
    if (!is.null(write_csv)) {
        readr::write_csv(out, write_csv)
    }

    out
}
