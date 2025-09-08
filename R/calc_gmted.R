static_zonal_summary <- function(
    tif_dir,
    level = c("county", "tract", "zip"),
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    zone_layer = NULL,
    id_col = "geoid",
    files_pattern = "\\.tif$",
    write_csv = NULL,
    add_overall_label = TRUE
) {
    # --- Argument validation ---
    level <- match.arg(level)
    if (is.null(zone_layer)) {
        zone_layer <- switch(
            level,
            county = "counties_500k",
            tract = "tracts_500k",
            zip = "zctas_500k"
        )
    }

    # --- Check required packages ---
    reqs <- c(
        "terra",
        "sf",
        "exactextractr",
        "dplyr",
        "purrr",
        "readr",
        "stringr",
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

    # --- Helper: variable name from filename ---
    var_from_path <- function(p) {
        nm <- tools::file_path_sans_ext(basename(p))
        sub("_clean$", "", nm) # remove trailing "_clean" if present
    }

    # --- Core extractor function (area-weighted, CRS-agnostic) ---
    zonal_mean_static <- function(tif, zones_sf, id_col) {
        r <- terra::rast(tif)

        # Turn negatives to NA (per your prior convention)
        r <- terra::ifel(r < 0, NA, r)

        # True cell-area weights (m^2) -> correct in lon/lat or projected
        w <- terra::cellSize(r, unit = "m")

        # Reproject zones to raster CRS for extraction
        zones_r <- sf::st_transform(zones_sf, terra::crs(r))

        vals <- exactextractr::exact_extract(
            r,
            zones_r,
            fun = "weighted_mean",
            weights = w
        )

        tibble::tibble(
            !!id_col := zones_sf[[id_col]],
            var = var_from_path(tif),
            value = as.numeric(vals)
        )
    }

    # --- Read zone geometries ---
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

    # --- List all static raster files ---
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

    # --- Extract all static rasters and bind results ---
    res <- purrr::map_dfr(
        tifs,
        zonal_mean_static,
        zones_sf = zones,
        id_col = id_col
    )

    # Optional label to align with annual/monthly/overall pipelines
    if (isTRUE(add_overall_label)) {
        res$agg <- "overall"
    }

    # --- Optional write ---
    if (!is.null(write_csv)) {
        readr::write_csv(res, write_csv)
    }

    res
}
