summarise_tri_air_totals <- function(
    tri_dir = "clean_data/tri_clean",
    county_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = c("county", "tract"),
    agg = c("annual", "monthly")
) {
    level <- match.arg(level)
    agg <- match.arg(agg)
    crs_m <- 5070

    # Determine layers and buffer distance
    layer <- if (level == "county") "counties_500k" else "tracts_500k"
    buffer_km <- if (level == "county") 20 else 4

    # Read and prepare geometries
    zones_ll <- sf::st_read(county_gpkg, layer = layer, quiet = TRUE) |>
        sf::st_make_valid() |>
        sf::st_transform(4326)

    zones_m <- sf::st_transform(zones_ll, crs_m)

    # Calculate area in km²
    area_vec <- as.numeric(units::set_units(st_area(zones_m), "km^2"))
    zone_area <- zones_m |>
        sf::st_drop_geometry() |>
        dplyr::mutate(area_km2 = area_vec) |>
        dplyr::select(geoid, area_km2)

    # Create +buffer zone for nearby emissions
    zones_plus_m <- sf::st_buffer(zones_m, dist = buffer_km * 1000)
    zones_plus_ll <- sf::st_transform(zones_plus_m, 4326) |>
        dplyr::select(geoid)

    # List all processed TRI files
    gpkg_files <- list.files(
        tri_dir,
        pattern = "_processed\\.gpkg$",
        full.names = TRUE
    )

    # Main loop
    df_all <- purrr::map_dfr(gpkg_files, function(gpkg_path) {
        year <- stringr::str_extract(basename(gpkg_path), "\\d{4}") |>
            as.integer()

        tri_sf <- sf::st_read(gpkg_path, quiet = TRUE) |>
            sf::st_make_valid() |>
            sf::st_transform(sf::st_crs(zones_ll))
        names(tri_sf) <- make.unique(names(tri_sf), sep = "_dup")

        fug_cols <- grep("^FUGITIVE_AIR_", names(tri_sf), value = TRUE)
        stack_cols <- grep("^STACK_AIR_", names(tri_sf), value = TRUE)
        if (length(fug_cols) + length(stack_cols) == 0L) {
            return(tibble())
        }

        tri_sf <- tri_sf |>
            dplyr::mutate(across(all_of(fug_cols), ~ ifelse(is.na(.), 0, .))) |>
            dplyr::mutate(across(all_of(stack_cols), ~ ifelse(is.na(.), 0, .)))

        # Inner county emissions
        tri_inner <- sf::st_join(tri_sf, zones_ll, join = sf::st_within) |>
            sf::st_drop_geometry() |>
            dplyr::filter(!is.na(geoid))

        inner_long <- tri_inner |>
            dplyr::select(geoid, all_of(fug_cols), all_of(stack_cols)) |>
            tidyr::pivot_longer(
                cols = -geoid,
                names_to = "pollutant_id",
                values_to = "emissions_lb"
            ) |>
            dplyr::mutate(
                pathway = dplyr::if_else(
                    stringr::str_starts(pollutant_id, "FUGITIVE"),
                    "fugitive",
                    "stack"
                )
            )

        inner_summary <- inner_long |>
            dplyr::group_by(geoid, pathway) |>
            dplyr::summarise(
                emissions_lb = sum(emissions_lb, na.rm = TRUE),
                .groups = "drop"
            ) |>
            tidyr::pivot_wider(
                names_from = pathway,
                values_from = emissions_lb,
                names_glue = "annual_total_{pathway}_air_lb",
                values_fill = 0
            ) |>
            dplyr::mutate(
                annual_total_air_lb = annual_total_fugitive_air_lb +
                    annual_total_stack_air_lb
            ) |>
            dplyr::left_join(zone_area, by = "geoid") |>
            dplyr::mutate(
                annual_total_fugitive_air_lb_per_km2 = annual_total_fugitive_air_lb /
                    area_km2,
                annual_total_stack_air_lb_per_km2 = annual_total_stack_air_lb /
                    area_km2,
                annual_total_air_lb_per_km2 = annual_total_air_lb / area_km2,
                year = year
            )

        # Outer 4km/20km buffer emissions
        tri_plus <- sf::st_join(tri_sf, zones_plus_ll, join = sf::st_within) |>
            sf::st_drop_geometry() |>
            dplyr::filter(!is.na(geoid))

        plus_summary <- if (nrow(tri_plus) == 0L) {
            inner_summary |>
                dplyr::transmute(
                    geoid,
                    annual_total_fugitive_air_lb_plusbuffer = 0,
                    annual_total_stack_air_lb_plusbuffer = 0,
                    annual_total_air_lb_plusbuffer = 0
                )
        } else {
            tri_plus |>
                dplyr::select(geoid, all_of(fug_cols), all_of(stack_cols)) |>
                tidyr::pivot_longer(
                    cols = -geoid,
                    names_to = "pollutant_id",
                    values_to = "emissions_lb"
                ) |>
                dplyr::mutate(
                    pathway = dplyr::if_else(
                        stringr::str_starts(pollutant_id, "FUGITIVE"),
                        "fugitive",
                        "stack"
                    )
                ) |>
                dplyr::group_by(geoid, pathway) |>
                dplyr::summarise(
                    emissions_lb = sum(emissions_lb, na.rm = TRUE),
                    .groups = "drop"
                ) |>
                tidyr::pivot_wider(
                    names_from = pathway,
                    values_from = emissions_lb,
                    names_glue = "annual_total_{pathway}_air_lb_plusbuffer",
                    values_fill = 0
                ) |>
                dplyr::mutate(
                    annual_total_air_lb_plusbuffer = annual_total_fugitive_air_lb_plusbuffer +
                        annual_total_stack_air_lb_plusbuffer
                )
        }

        # Combine
        dplyr::left_join(inner_summary, plus_summary, by = "geoid") |>
            dplyr::mutate(
                across(contains("_plusbuffer"), ~ tidyr::replace_na(.x, 0))
            ) |>
            tidyr::pivot_longer(
                cols = -c(geoid, year),
                names_to = "var",
                values_to = "value"
            ) |>
            dplyr::arrange(geoid, year, var)
    })

    # If monthly, replicate 12x
    if (agg == "monthly") {
        df_all <- df_all |>
            dplyr::mutate(month = list(1:12)) |>
            tidyr::unnest(month)
    }

    df_all
}
