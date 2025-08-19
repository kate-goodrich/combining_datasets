zonal_means_from_tifs <- function(
    input_dir = "clean_data/nlcd_clean",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = c("county", "tract"),
    zone_layer = NULL,
    agg = c("annual", "monthly"),
    id_col = "geoid",
    file_pattern = "_processed\\.tif$",
    write_csv = NULL
) {
    level <- match.arg(level)
    agg <- match.arg(agg)

    if (is.null(zone_layer)) {
        zone_layer <- if (level == "county") "counties_500k" else "tracts_500k"
    }

    # --- Load polygons ---
    zones <- sf::st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid()

    if (!(id_col %in% names(zones))) {
        abort(paste0(
            "id_col '",
            id_col,
            "' not found in zones: available = ",
            paste(names(zones), collapse = ", ")
        ))
    }
    zones <- dplyr::select(zones, !!sym(id_col), geom = geometry)

    # --- Collect files ---
    tif_files <- list.files(
        input_dir,
        pattern = file_pattern,
        recursive = TRUE,
        full.names = TRUE
    )
    if (length(tif_files) == 0L) {
        abort(paste0(
            "No TIFFs found in ",
            input_dir,
            " matching pattern ",
            file_pattern
        ))
    }

    # --- Helpers ---
    parse_variable <- function(path) {
        base <- tools::file_path_sans_ext(basename(path))
        base <- sub("_processed$", "", base)
        base <- sub("_(19|20)\\d{4}$", "", base) # strip trailing year
        paste0("annual_", tolower(base))
    }

    parse_year <- function(path) {
        as.integer(stringr::str_extract(basename(path), "(19|20)\\d{2}"))
    }

    # Extract for one file
    extract_one <- function(tif_path) {
        r <- terra::rast(tif_path)
        zones_proj <- sf::st_transform(zones, terra::crs(r))

        vals <- exactextractr::exact_extract(r, zones_proj, "mean")

        tibble(
            !!id_col := zones_proj[[id_col]],
            level = level,
            variable = parse_variable(tif_path),
            year = parse_year(tif_path),
            value = as.numeric(vals)
        )
    }

    raw_df <- purrr::map_dfr(tif_files, extract_one)

    # --- Aggregate/format ---
    if (agg == "annual") {
        out <- raw_df |>
            group_by(!!sym(id_col), level, variable, year) |>
            summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
            mutate(month = NA_integer_) |>
            relocate(!!sym(id_col), level, variable, year, month, value)
    } else {
        # replicate annual values across 12 months
        annual <- raw_df |>
            group_by(!!sym(id_col), level, variable, year) |>
            summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
        out <- annual |>
            group_by(!!sym(id_col), level, variable, year) |>
            tidyr::expand(month = 1:12) |>
            ungroup() |>
            left_join(annual, by = c(id_col, "level", "variable", "year")) |>
            relocate(!!sym(id_col), level, variable, year, month, value)
    }

    if (!is.null(write_csv)) {
        dir.create(dirname(write_csv), recursive = TRUE, showWarnings = FALSE)
        readr::write_csv(out, write_csv)
    }

    out
}
