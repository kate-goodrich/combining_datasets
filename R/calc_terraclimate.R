summarize_terraclimate <- function(
    tif_dir = "clean_data/terraclimate_clean",
    county_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = c("county", "tract"),
    agg = c("annual", "monthly"),
    zone_layer = NULL,
    id_col = "geoid",
    write_csv = NULL
) {
    # --- Match args and infer layer if needed ---
    level <- match.arg(level)
    agg <- match.arg(agg)
    if (is.null(zone_layer)) {
        zone_layer <- if (level == "county") "counties_500k" else "tracts_500k"
    }

    # --- Load zones and ensure valid geometry ---
    zones <- sf::st_read(county_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid()

    # --- Helpers ---
    std_layer_names <- function(nm) {
        out <- sub("^.*?(\\d{6})$", "\\1", nm) # YYYYMM
        out[!grepl("^\\d{6}$", out)] <- sub(
            "^.*?(\\d{4})$",
            "\\1",
            nm[!grepl("^\\d{6}$", out)]
        ) # YYYY fallback
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
    summarise_one <- function(tif_path, zones_ll) {
        r <- terra::rast(tif_path)
        zones_proj <- sf::st_transform(zones_ll, terra::crs(r))

        r_use <- switch(
            agg,
            "monthly" = {
                names(r) <- std_layer_names(names(r))
                r
            },
            "annual" = annualize(r)
        )

        # Clamp negatives to zero
        r_use <- terra::ifel(r_use < 0, 0, r_use)

        mat <- exactextractr::exact_extract(
            r_use,
            zones_proj,
            "mean",
            progress = FALSE
        )
        mat <- tibble::as_tibble(mat)
        colnames(mat) <- names(r_use)

        out <- tibble::tibble(!!id_col := zones_proj[[id_col]]) |>
            dplyr::bind_cols(mat) |>
            tidyr::pivot_longer(
                -dplyr::all_of(id_col),
                names_to = if (agg == "monthly") "date" else "year",
                values_to = "value"
            ) |>
            dplyr::mutate(
                var = var_from_path(tif_path),
                year = if (agg == "monthly") {
                    as.integer(substr(.data$date, 1, 4))
                } else {
                    as.integer(.data$year)
                },
                month = if (agg == "monthly") {
                    as.integer(substr(.data$date, 5, 6))
                } else {
                    NA_integer_
                }
            ) |>
            dplyr::select(dplyr::any_of(c(
                id_col,
                "year",
                if (agg == "monthly") "month",
                "var",
                "value"
            )))

        return(out)
    }

    # --- List all .tif files ---
    tifs <- list.files(tif_dir, pattern = "\\.tif$", full.names = TRUE)

    # --- Run zonal summaries ---
    df <- purrr::map_dfr(tifs, summarise_one, zones_ll = zones)

    # --- Optionally write to disk ---
    if (!is.null(write_csv)) {
        readr::write_csv(df, write_csv)
    }

    return(df)
}
