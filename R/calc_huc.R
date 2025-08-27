# ---- Compatibility: unary union helper ----
.unary_union <- local({
    if ("st_unary_union" %in% getNamespaceExports("sf")) {
        function(x) sf::st_unary_union(x)
    } else {
        function(x) sf::st_union(x)
    }
})

# ---- Main function ----
huc_coverage_long <- function(
    gpkg_dir = "clean_data/huc_clean",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = c("county", "tract"),
    zone_layer = NULL,
    id_col = "geoid",
    include_datasets = NULL,
    write_csv = NULL,
    log_failed = "logs/huc_intersection_failures.txt"
) {
    level <- match.arg(level)
    if (is.null(zone_layer)) {
        zone_layer <- if (level == "county") "counties_500k" else "tracts_500k"
    }

    # Precision settings based on level
    prec_val <- if (level == "county") 1e4 else 1e5

    # ---- Read and prepare zones ----
    zones <- sf::st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid() |>
        dplyr::select(!!id_col) |>
        sf::st_transform(5070)

    stopifnot(id_col %in% names(zones))

    zones_area_tbl <- zones |>
        dplyr::mutate(
            zone_area_km2 = as.numeric(units::set_units(
                sf::st_area(zones),
                km^2
            ))
        ) |>
        sf::st_drop_geometry() |>
        dplyr::select(!!id_col, zone_area_km2)

    # ---- List GPKG datasets ----
    gpkg_files <- list.files(gpkg_dir, pattern = "\\.gpkg$", full.names = TRUE)
    if (!is.null(include_datasets)) {
        base_names <- tools::file_path_sans_ext(basename(gpkg_files))
        gpkg_files <- gpkg_files[base_names %in% include_datasets]
    }
    if (length(gpkg_files) == 0L) {
        stop("No .gpkg files found after filtering in: ", gpkg_dir)
    }

    # ---- Helper: dissolve messy polygon footprints ----
    dissolve_footprint <- function(g) {
        g <- sf::st_make_valid(g)
        poly_types <- c("POLYGON", "MULTIPOLYGON")
        g <- g[
            sf::st_geometry_type(g, by_geometry = TRUE) %in% poly_types,
            ,
            drop = FALSE
        ]

        if (nrow(g) == 0L) {
            return(sf::st_sfc(sf::st_polygon(), crs = sf::st_crs(g)))
        }

        g <- sf::st_collection_extract(g, "POLYGON", warn = FALSE) |>
            sf::st_make_valid()

        u <- try(.unary_union(g), silent = TRUE)
        if (inherits(u, "try-error")) {
            u <- try(.unary_union(sf::st_buffer(g, 0)), silent = TRUE)
        }
        if (inherits(u, "try-error")) {
            g2 <- sf::st_set_precision(g, prec_val) |>
                lwgeom::st_snap_to_grid(prec_val) |>
                sf::st_make_valid()
            u <- .unary_union(g2)
        }

        sf::st_cast(u, "MULTIPOLYGON", warn = FALSE)
    }

    # ---- Helper: summarise one dataset ----
    summarise_dataset <- function(gpkg_path, zones_aea) {
        lyr_names <- sf::st_layers(gpkg_path)$name
        lyr <- if (length(lyr_names)) lyr_names[1] else NA_character_
        if (is.na(lyr)) {
            return(tibble::tibble(
                !!id_col := character(0),
                dataset = character(0),
                prop_area = numeric(0)
            ))
        }

        ds <- sf::st_read(gpkg_path, layer = lyr, quiet = TRUE)
        dataset_name <- tools::file_path_sans_ext(basename(gpkg_path))

        if (nrow(ds) == 0L) {
            return(
                sf::st_drop_geometry(zones_aea) |>
                    dplyr::mutate(dataset = dataset_name, prop_area = 0) |>
                    dplyr::select(!!id_col, dataset, prop_area)
            )
        }

        ds <- ds |>
            sf::st_make_valid() |>
            sf::st_transform(sf::st_crs(zones_aea))
        geom_types <- unique(sf::st_geometry_type(ds, by_geometry = TRUE))
        has_polys <- any(geom_types %in% c("POLYGON", "MULTIPOLYGON"))

        if (has_polys) {
            ds_union <- dissolve_footprint(ds)

            inter <- try(
                sf::st_intersection(zones_aea, ds_union),
                silent = TRUE
            )

            if (inherits(inter, "try-error") || nrow(inter) == 0L) {
                if (!is.null(log_failed)) {
                    cat(dataset_name, "\n", file = log_failed, append = TRUE)
                }
                return(tibble::tibble(
                    !!id_col := sf::st_drop_geometry(zones_aea)[[id_col]],
                    dataset = dataset_name,
                    prop_area = 0
                ))
            }

            inter_area <- as.numeric(units::set_units(sf::st_area(inter), km^2))
            prop_tbl <- inter |>
                sf::st_drop_geometry() |>
                dplyr::mutate(overlap_km2 = inter_area) |>
                dplyr::group_by(.data[[id_col]]) |>
                dplyr::summarise(
                    overlap_km2 = sum(overlap_km2, na.rm = TRUE),
                    .groups = "drop"
                )
        } else {
            prop_tbl <- tibble::tibble(
                !!id_col := sf::st_drop_geometry(zones_aea)[[id_col]],
                overlap_km2 = 0
            )
        }

        prop_tbl <- prop_tbl |>
            dplyr::right_join(zones_area_tbl, by = id_col) |>
            dplyr::mutate(
                overlap_km2 = if_else(is.na(overlap_km2), 0, overlap_km2),
                prop_area = if_else(
                    zone_area_km2 > 0,
                    pmin(pmax(overlap_km2 / zone_area_km2, 0), 1),
                    NA_real_
                )
            ) |>
            dplyr::select(!!id_col, prop_area)

        tibble::tibble(
            !!id_col := sf::st_drop_geometry(zones_aea)[[id_col]],
            dataset = dataset_name,
            prop_area = prop_tbl$prop_area
        )
    }

    # ---- Run and pivot output ----
    metrics_wide <- purrr::map_dfr(
        gpkg_files,
        summarise_dataset,
        zones_aea = zones
    )
    all_datasets <- sort(unique(metrics_wide$dataset))
    all_zones <- sf::st_drop_geometry(zones)[[id_col]]

    metrics_complete <- tidyr::complete(
        metrics_wide,
        !!rlang::sym(id_col) := all_zones,
        dataset = all_datasets,
        fill = list(prop_area = 0)
    )

    long_out <- metrics_complete |>
        dplyr::mutate(
            var = paste0("prop_cover_", dataset),
            value = prop_area
        ) |>
        dplyr::select(!!id_col, var, value) |>
        dplyr::arrange(.data[[id_col]], var)

    if (!is.null(write_csv)) {
        readr::write_csv(long_out, write_csv)
    }

    long_out
}
