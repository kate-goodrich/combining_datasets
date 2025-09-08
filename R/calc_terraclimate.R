summarize_terraclimate <- function(
    tif_dir = "clean_data/terraclimate_clean",
    county_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = c("county", "tract", "zip"),
    agg = c("annual", "monthly", "overall"),
    zone_layer = NULL,
    id_col = "geoid",
    write_csv = NULL
) {
    # --- Match args and infer layer if needed ---
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

    # --- Load zones and ensure valid geometry ---
    zones <- sf::st_read(county_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid()
    if (!(id_col %in% names(zones))) {
        stop("id_col not found in zones layer.")
    }

    # --- Helpers ---
    std_layer_names <- function(nm) {
        out <- sub("^.*?(\\d{6})$", "\\1", nm) # prefer YYYYMM
        bad <- !grepl("^\\d{6}$", out)
        out[bad] <- sub("^.*?(\\d{4})$", "\\1", nm[bad]) # fallback YYYY
        out
    }
    layer_years <- function(r) {
        as.integer(substr(std_layer_names(names(r)), 1, 4))
    }
    annualize <- function(r) {
        yrs <- layer_years(r)
        ra <- terra::tapp(r, index = yrs, fun = mean, na.rm = TRUE)
        names(ra) <- as.character(sort(unique(yrs)))
        ra
    }
    var_from_path <- function(p) {
        nm <- tools::file_path_sans_ext(basename(p))
        sub("_processed$", "", nm)
    }

    # --- Zonal summary for one file ---
    summarise_one <- function(tif_path, zones_ll, agg_mode) {
        r <- terra::rast(tif_path)
        zones_proj <- sf::st_transform(zones_ll, terra::crs(r))

        r_use <- switch(
            agg_mode,
            "monthly" = {
                names(r) <- std_layer_names(names(r))
                r
            },
            "annual" = annualize(r),
            "overall" = annualize(r) # compute annual first; overall later
        )

        # Clamp negatives to zero
        r_use <- terra::ifel(r_use < 0, 0, r_use)

        # Area weights (m^2); one weights raster is fine for a stack on same grid
        w <- terra::cellSize(r_use[[1]], unit = "m")

        # Area-weighted mean extraction
        mat <- exactextractr::exact_extract(
            r_use,
            zones_proj,
            fun = "weighted_mean",
            weights = w,
            progress = FALSE
        )
        mat <- tibble::as_tibble(mat)
        colnames(mat) <- names(r_use)

        base <- tibble::tibble(!!id_col := zones_proj[[id_col]]) |>
            dplyr::bind_cols(mat) |>
            tidyr::pivot_longer(
                -dplyr::all_of(id_col),
                names_to = if (agg_mode == "monthly") "date" else "year",
                values_to = "value"
            ) |>
            dplyr::mutate(
                var = var_from_path(tif_path),
                year = if (agg_mode == "monthly") {
                    as.integer(substr(.data$date, 1, 4))
                } else {
                    as.integer(.data$year)
                },
                month = if (agg_mode == "monthly") {
                    as.integer(substr(.data$date, 5, 6))
                } else {
                    NA_integer_
                }
            )

        if (agg_mode == "overall") {
            # keep only 2010–2024 then average across years per id x var
            base |>
                dplyr::filter(year >= 2010, year <= 2024) |>
                dplyr::group_by(.data[[id_col]], var) |>
                dplyr::summarise(
                    value = mean(value, na.rm = TRUE),
                    .groups = "drop"
                ) |>
                dplyr::select(dplyr::all_of(id_col), var, value)
        } else if (agg_mode == "annual") {
            base |>
                dplyr::select(dplyr::all_of(id_col), year, var, value)
        } else {
            # monthly
            base |>
                dplyr::select(dplyr::all_of(id_col), year, month, var, value)
        }
    }

    # --- List all .tif files ---
    tifs <- list.files(tif_dir, pattern = "\\.tif$", full.names = TRUE)
    if (length(tifs) == 0L) {
        stop("No .tif files found in '", tif_dir, "'.")
    }

    # --- Run zonal summaries ---
    df <- purrr::map_dfr(tifs, summarise_one, zones_ll = zones, agg_mode = agg)

    # --- Optionally write to disk ---
    if (!is.null(write_csv)) {
        readr::write_csv(df, write_csv)
    }

    df
}
