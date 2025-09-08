road_density_zonal <- function(
    roads_gpkg = "clean_data/groads_clean/groads_clean.gpkg",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = c("county", "tract", "zip"),
    zone_layer = NULL,
    id_col = "geoid",
    write_csv = NULL
) {
    level <- match.arg(level)
    if (is.null(zone_layer)) {
        zone_layer <- switch(
            level,
            county = "counties_500k",
            tract = "tracts_500k",
            zip = "zctas_500k"
        )
    }

    # --- Dependency checks ---
    reqs <- c("sf", "dplyr", "units", "tidyr", "readr")
    ok <- vapply(reqs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
    if (!all(ok)) {
        stop(
            "Missing packages: ",
            paste(reqs[!ok], collapse = ", "),
            call. = FALSE
        )
    }

    # --- Read geometries ---
    zones <- sf::st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid() |>
        dplyr::select(dplyr::all_of(id_col))

    roads <- sf::st_read(roads_gpkg, quiet = TRUE) |>
        sf::st_make_valid()

    # --- Project to equal-area CRS (EPSG:5070, meters) ---
    crs_m <- 5070
    zones_m <- sf::st_transform(zones, crs_m)
    roads_m <- sf::st_transform(roads, crs_m)

    # --- Compute zone areas ---
    areas <- sf::st_area(zones_m)
    area_tbl <- zones_m |>
        dplyr::mutate(area_km2 = as.numeric(units::set_units(areas, km^2))) |>
        sf::st_drop_geometry() |>
        dplyr::select(dplyr::all_of(id_col), area_km2)

    # --- Clip roads to zones and calculate lengths ---
    roads_split <- sf::st_intersection(
        roads_m,
        zones_m |> dplyr::select(dplyr::all_of(id_col))
    )

    roads_split <- roads_split |>
        dplyr::mutate(road_km = as.numeric(sf::st_length(roads_split)) / 1000)

    # --- Summarise per zone ---
    summary_wide <- roads_split |>
        sf::st_drop_geometry() |>
        dplyr::group_by(.data[[id_col]]) |>
        dplyr::summarise(
            total_road_km = sum(road_km, na.rm = TRUE),
            .groups = "drop"
        ) |>
        dplyr::left_join(area_tbl, by = id_col) |>
        dplyr::mutate(road_density_km_per_km2 = total_road_km / area_km2)

    # --- Long format (+ tag as overall for consistency) ---
    summary_long <- summary_wide |>
        tidyr::pivot_longer(
            cols = c(total_road_km, area_km2, road_density_km_per_km2),
            names_to = "var",
            values_to = "value"
        ) |>
        dplyr::mutate(agg = "overall") |>
        dplyr::arrange(.data[[id_col]], var)

    if (!is.null(write_csv)) {
        readr::write_csv(summary_long, write_csv)
    }

    summary_long
}
