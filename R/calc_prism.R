prism_normals_from_tifs <- function(
    input_dir = "clean_data/prism_clean",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = c("county", "tract"),
    zone_layer = NULL,
    agg = c("annual", "monthly"),
    id_col = "geoid",
    file_pattern = "\\.tif$",
    write_csv = NULL
) {
    level <- match.arg(level)
    agg <- match.arg(agg)

    if (is.null(zone_layer)) {
        zone_layer <- if (level == "county") "counties_500k" else "tracts_500k"
    }

    # --- Load zones ---
    zones <- sf::st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid()
    if (!(id_col %in% names(zones))) {
        abort(paste0(
            "id_col '",
            id_col,
            "' not found in zones. Available: ",
            paste(names(zones), collapse = ", ")
        ))
    }
    zones <- dplyr::select(zones, !!sym(id_col))
    zones$geom <- sf::st_geometry(zones)

    # --- Index files ---
    tif_paths <- list.files(
        input_dir,
        pattern = file_pattern,
        full.names = TRUE,
        recursive = TRUE
    )
    if (length(tif_paths) == 0L) {
        abort(paste0(
            "No TIFFs found in ",
            input_dir,
            " matching pattern ",
            file_pattern
        ))
    }

    # --- Parse var & month from filename ---
    parse_meta <- function(path) {
        fn <- tools::file_path_sans_ext(basename(path))
        tibble(
            file = path,
            variable = tolower(stringr::str_extract(fn, "^[^_]+")), # "ppt"
            month = as.integer(stringr::str_extract(fn, "(?<=_)\\d{2}$")) # "01"->1
        )
    }
    meta <- purrr::map_dfr(tif_paths, parse_meta)

    # --- Extract county/tract means for one file ---
    extract_one <- function(path) {
        r <- terra::rast(path)
        zp <- sf::st_transform(zones, terra::crs(r))
        vals <- exactextractr::exact_extract(r, zp, "mean")
        tibble(
            !!id_col := zp[[id_col]],
            value = as.numeric(vals)
        )
    }

    # --- Compute per-file, attach metadata ---
    normals_long <- purrr::pmap_dfr(
        list(meta$file, meta$variable, meta$month),
        function(path, var, mon) {
            df <- extract_one(path)
            df$variable <- var
            df$month <- mon
            df
        }
    ) |>
        mutate(level = level) |>
        relocate(!!sym(id_col), level, variable, month, value)

    # --- Aggregate/shape ---
    out <-
        if (agg == "monthly") {
            normals_long |>
                arrange(!!sym(id_col), variable, month)
        } else {
            # annual = mean over months 1..12
            normals_long |>
                group_by(!!sym(id_col), level, variable) |>
                summarise(
                    value = mean(value, na.rm = TRUE),
                    .groups = "drop"
                ) |>
                mutate(year = NA_integer_, month = NA_integer_) |>
                relocate(!!sym(id_col), level, variable, year, month, value)
        }

    if (!is.null(write_csv)) {
        dir.create(dirname(write_csv), recursive = TRUE, showWarnings = FALSE)
        readr::write_csv(out, write_csv)
    }

    out
}
