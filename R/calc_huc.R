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

    # Read and prepare zones
    zones <- sf::st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid() |>
        dplyr::select(!!id_col) |>
        sf::st_transform(5070) |>
        sf::st_set_precision(1) |>
        lwgeom::st_snap_to_grid(1)

    stopifnot(id_col %in% names(zones))

    zones_area_tbl <- zones |>
        dplyr::mutate(
            zone_area_km2 = as.numeric(set_units(st_area(zones), km^2))
        ) |>
        st_drop_geometry() |>
        dplyr::select(!!id_col, zone_area_km2)

    # List GPKG datasets
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
        g <- st_make_valid(g)
        poly_types <- c("POLYGON", "MULTIPOLYGON")
        g <- g[
            st_geometry_type(g, by_geometry = TRUE) %in% poly_types,
            ,
            drop = FALSE
        ]

        if (nrow(g) == 0L) {
            return(st_sfc(st_polygon(), crs = st_crs(g)))
        }

        g <- st_collection_extract(g, "POLYGON", warn = FALSE) |>
            st_set_precision(1) |>
            lwgeom::st_snap_to_grid(1) |>
            st_make_valid()

        u <- try(.unary_union(g), silent = TRUE)
        if (inherits(u, "try-error")) {
            u <- try(.unary_union(st_buffer(g, 0)), silent = TRUE)
        }
        if (inherits(u, "try-error")) {
            g2 <- st_set_precision(g, 5) |>
                lwgeom::st_snap_to_grid(5) |>
                st_make_valid()
            u <- .unary_union(g2)
        }

        st_cast(u, "MULTIPOLYGON", warn = FALSE)
    }

    # ---- Helper: summarise one dataset ----
    summarise_dataset <- function(gpkg_path, zones_aea) {
        lyr_names <- sf::st_layers(gpkg_path)$name
        lyr <- if (length(lyr_names)) lyr_names[1] else NA_character_
        if (is.na(lyr)) {
            return(tibble(
                !!id_col := character(0),
                dataset = character(0),
                prop_area = numeric(0),
                n_features = integer(0)
            ))
        }

        ds <- sf::st_read(gpkg_path, layer = lyr, quiet = TRUE)
        dataset_name <- tools::file_path_sans_ext(basename(gpkg_path))

        if (nrow(ds) == 0L) {
            return(
                st_drop_geometry(zones_aea) |>
                    mutate(
                        dataset = dataset_name,
                        prop_area = 0,
                        n_features = 0
                    ) |>
                    select(!!id_col, dataset, prop_area, n_features)
            )
        }

        ds <- ds |> st_make_valid() |> st_transform(st_crs(zones_aea))
        geom_types <- unique(st_geometry_type(ds, by_geometry = TRUE))
        has_polys <- any(geom_types %in% c("POLYGON", "MULTIPOLYGON"))

        idx <- st_intersects(zones_aea, ds, sparse = TRUE)
        n_features_vec <- lengths(idx)

        if (has_polys) {
            ds_union <- dissolve_footprint(ds)

            inter <- try(
                {
                    st_intersection(zones_aea, st_buffer(ds_union, 0))
                },
                silent = TRUE
            )

            if (inherits(inter, "try-error") || nrow(inter) == 0L) {
                if (!is.null(log_failed)) {
                    cat(dataset_name, "\n", file = log_failed, append = TRUE)
                }
                return(tibble(
                    !!id_col := st_drop_geometry(zones_aea)[[id_col]],
                    dataset = dataset_name,
                    prop_area = 0,
                    n_features = n_features_vec
                ))
            }

            inter_area <- as.numeric(set_units(st_area(inter), km^2))
            prop_tbl <- inter |>
                st_drop_geometry() |>
                mutate(overlap_km2 = inter_area) |>
                group_by(.data[[id_col]]) |>
                summarise(
                    overlap_km2 = sum(overlap_km2, na.rm = TRUE),
                    .groups = "drop"
                )
        } else {
            prop_tbl <- tibble(
                !!id_col := st_drop_geometry(zones_aea)[[id_col]],
                overlap_km2 = 0
            )
        }

        prop_tbl <- prop_tbl |>
            right_join(zones_area_tbl, by = id_col) |>
            mutate(
                overlap_km2 = if_else(is.na(overlap_km2), 0, overlap_km2),
                prop_area = if_else(
                    zone_area_km2 > 0,
                    pmin(pmax(overlap_km2 / zone_area_km2, 0), 1),
                    NA_real_
                )
            ) |>
            select(!!id_col, prop_area)

        tibble(
            !!id_col := st_drop_geometry(zones_aea)[[id_col]],
            dataset = dataset_name,
            prop_area = prop_tbl$prop_area,
            n_features = n_features_vec
        )
    }

    # ---- Run and pivot output ----
    metrics_wide <- purrr::map_dfr(
        gpkg_files,
        summarise_dataset,
        zones_aea = zones
    )
    all_datasets <- sort(unique(metrics_wide$dataset))
    all_zones <- st_drop_geometry(zones)[[id_col]]

    metrics_complete <- tidyr::complete(
        metrics_wide,
        !!rlang::sym(id_col) := all_zones,
        dataset = all_datasets,
        fill = list(prop_area = 0, n_features = 0)
    )

    long_out <- metrics_complete |>
        tidyr::pivot_longer(
            cols = c(prop_area, n_features),
            names_to = "metric",
            values_to = "value"
        ) |>
        mutate(
            var = case_when(
                metric == "prop_area" ~ paste0("prop_cover_", dataset),
                metric == "n_features" ~ paste0("num_features_", dataset),
                TRUE ~ paste0(metric, "_", dataset)
            )
        ) |>
        select(!!id_col, var, value) |>
        arrange(.data[[id_col]], var)

    if (!is.null(write_csv)) {
        readr::write_csv(long_out, write_csv)
    }

    long_out
}
