prism_normals_from_tifs <- function(
    input_dir = "clean_data/prism_clean",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = c("county", "tract", "zip"),
    zone_layer = NULL,
    agg = c("annual", "monthly"),
    id_col = "geoid",
    file_pattern = "\\.tif$",
    write_csv = NULL # NULL = no write; TRUE = auto-name; character = custom path
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

    # --- Load zones ---
    zones <- sf::st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid()
    if (!(id_col %in% names(zones))) {
        rlang::abort(paste0(
            "id_col '",
            id_col,
            "' not found in zones. Available: ",
            paste(names(zones), collapse = ", ")
        ))
    }
    zones <- dplyr::select(zones, !!rlang::sym(id_col))
    zones$geom <- sf::st_geometry(zones)

    # --- Index files ---
    tif_paths <- list.files(
        input_dir,
        pattern = file_pattern,
        full.names = TRUE,
        recursive = TRUE
    )
    if (length(tif_paths) == 0L) {
        rlang::abort(paste0(
            "No TIFFs found in ",
            input_dir,
            " matching pattern ",
            file_pattern
        ))
    }

    # --- Parse var & month from filename ---
    parse_meta <- function(path) {
        fn <- tools::file_path_sans_ext(basename(path))
        tibble::tibble(
            file = path,
            variable = tolower(stringr::str_extract(fn, "^[^_]+")),
            month = as.integer(stringr::str_extract(fn, "(?<=_)\\d{2}$"))
        )
    }
    meta <- purrr::map_dfr(tif_paths, parse_meta)

    # --- Extract means for one file (area-weighted) ---
    extract_one <- function(path) {
        r <- terra::rast(path)
        zp <- sf::st_transform(zones, terra::crs(r))
        w <- terra::cellSize(r, unit = "m")
        vals <- exactextractr::exact_extract(
            r,
            zp,
            fun = "weighted_mean",
            weights = w,
            progress = FALSE
        )
        tibble::tibble(
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
        dplyr::mutate(level = level)

    # --- Aggregate/shape ---
    out <- if (agg == "monthly") {
        normals_long |>
            dplyr::arrange(!!rlang::sym(id_col), .data$variable, .data$month) |>
            dplyr::mutate(year = "normal") |>
            dplyr::select(
                !!rlang::sym(id_col),
                .data$year,
                .data$level,
                .data$variable,
                .data$month,
                .data$value
            )
    } else {
        normals_long |>
            dplyr::group_by(
                !!rlang::sym(id_col),
                .data$level,
                .data$variable
            ) |>
            dplyr::summarise(
                value = mean(.data$value, na.rm = TRUE),
                .groups = "drop"
            ) |>
            dplyr::mutate(year = "normal") |>
            dplyr::select(
                !!rlang::sym(id_col),
                .data$year,
                .data$level,
                .data$variable,
                .data$value
            )
    }

    # --- Rename tmin/tmax to *_norm ---
    out <- out |>
        dplyr::mutate(
            variable = dplyr::case_when(
                .data$variable == "tmin" ~ "tmin_norm",
                .data$variable == "tmax" ~ "tmax_norm",
                TRUE ~ .data$variable
            )
        )

    # --- Handle writing ---
    write_path <- NULL
    if (isTRUE(write_csv)) {
        # auto-name: normal_<agg>_<level>_prism_normals.csv
        fname <- sprintf("normal_%s_%s_prism_normals.csv", agg, level)
        write_path <- file.path("summary_sets", fname)
    } else if (is.character(write_csv) && nzchar(write_csv[1])) {
        write_path <- write_csv
    }

    if (!is.null(write_path)) {
        dir.create(dirname(write_path), recursive = TRUE, showWarnings = FALSE)
        readr::write_csv(out, write_path)
    }

    out
}
