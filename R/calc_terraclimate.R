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

    # --- Helper: extract years from layer names like "ppt_201001" ---
    layer_years <- function(r) {
        nm <- names(r)
        ym <- sub("^[^_]*_", "", nm)
        as.integer(substr(ym, 1, 4))
    }

    # --- Collapse monthly → annual ---
    annualize <- function(r) {
        yrs <- layer_years(r)
        ra <- terra::tapp(r, index = yrs, fun = mean, na.rm = TRUE)
        names(ra) <- as.character(sort(unique(yrs)))
        ra
    }

    # --- Clean var name from filename ---
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
            "monthly" = r,
            "annual" = annualize(r)
        )

        mat <- exactextractr::exact_extract(
            r_use,
            zones_proj,
            "mean",
            progress = FALSE
        )
        mat <- tibble::as_tibble(mat)
        colnames(mat) <- names(r_use)

        out <- tibble(!!id_col := zones_proj[[id_col]]) |>
            bind_cols(mat) |>
            pivot_longer(
                -all_of(id_col),
                names_to = if (agg == "monthly") "date" else "year",
                values_to = "value"
            ) |>
            mutate(
                var = var_from_path(tif_path),
                year = if (agg == "monthly") {
                    as.integer(substr(date, 1, 4))
                } else {
                    as.integer(year)
                },
                date = if (agg == "monthly") date else NULL
            ) |>
            select(any_of(c(id_col, "year", "date", "var", "value")))

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
