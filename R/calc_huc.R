huc2_from_huc12_gpkg <- function(
    huc12_gpkg,
    huc12_layer = NULL,
    huc12_id_col = NULL,
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = c("county", "tract"),
    zone_layer = NULL,
    id_col = "geoid",
    write_csv = NULL,
    precision_snap = 1e4
) {
    level <- match.arg(level)
    if (is.null(zone_layer)) {
        zone_layer <- if (level == "county") "counties_500k" else "tracts_500k"
    }

    zones <- sf::st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid() |>
        dplyr::select(dplyr::all_of(id_col)) |>
        sf::st_transform(5070)

    if (!(id_col %in% names(zones))) {
        stop("id_col not found in zones")
    }

    zones_area_tbl <- zones |>
        dplyr::mutate(
            zone_area_km2 = as.numeric(units::set_units(
                sf::st_area(zones),
                "km^2"
            ))
        ) |>
        sf::st_drop_geometry() |>
        dplyr::select(dplyr::all_of(id_col), zone_area_km2)

    if (is.null(huc12_layer)) {
        lyr_names <- sf::st_layers(huc12_gpkg)$name
        pick <- grep(
            "HU.?12|HUC.?12",
            lyr_names,
            ignore.case = TRUE,
            value = TRUE
        )
        if (!length(pick)) {
            stop("No HUC12-like layer found in ", huc12_gpkg)
        }
        huc12_layer <- pick[1]
    }

    h12 <- sf::st_read(huc12_gpkg, layer = huc12_layer, quiet = TRUE) |>
        sf::st_make_valid() |>
        sf::st_transform(sf::st_crs(zones))

    if (is.null(huc12_id_col)) {
        cand <- intersect(
            names(h12),
            c("HUC12", "HUC_12", "HU_12", "HUCID", "HUC_12D")
        )
        if (!length(cand)) {
            stop("Could not find a HUC12 code column; set huc12_id_col")
        }
        huc12_id_col <- cand[1]
    }

    h12 <- h12 |>
        dplyr::mutate(
            .huc12 = stringr::str_pad(
                gsub("[^0-9]", "", as.character(.data[[huc12_id_col]])),
                width = 12,
                side = "left",
                pad = "0"
            ),
            .huc2 = substr(.huc12, 1, 2)
        ) |>
        dplyr::select(.huc2) # <-- keep geometry implicitly

    h2 <- h12 |>
        dplyr::group_by(.huc2) |>
        dplyr::summarise(.groups = "drop") |>
        sf::st_make_valid()

    intersect_once <- function(z, h) {
        tryCatch(
            sf::st_intersection(
                dplyr::select(z, dplyr::all_of(id_col)),
                dplyr::select(h, .huc2)
            ),
            error = function(e) e
        )
    }

    inter <- intersect_once(zones, h2)
    if (inherits(inter, "error")) {
        zones2 <- sf::st_set_precision(zones, precision_snap) |>
            lwgeom::st_snap_to_grid(precision_snap)
        h2_2 <- sf::st_set_precision(h2, precision_snap) |>
            lwgeom::st_snap_to_grid(precision_snap)
        inter <- intersect_once(zones2, h2_2)
        if (inherits(inter, "error")) {
            stop("Intersection failed after snapping: ", inter$message)
        }
    }

    if (nrow(inter) == 0L) {
        all_codes <- sort(unique(h2$.huc2))
        out_zero <- tidyr::crossing(
            !!rlang::sym(id_col) := sf::st_drop_geometry(zones)[[id_col]],
            .huc2 = all_codes
        ) |>
            dplyr::mutate(var = paste0("prop_cover_huc2_", .huc2), value = 0) |>
            dplyr::select(dplyr::all_of(id_col), var, value)
        if (!is.null(write_csv)) {
            readr::write_csv(out_zero, write_csv)
        }
        return(out_zero)
    }

    inter_area <- as.numeric(units::set_units(sf::st_area(inter), "km^2"))

    prop_tbl <- inter |>
        sf::st_drop_geometry() |>
        dplyr::mutate(overlap_km2 = inter_area) |>
        dplyr::group_by(.data[[id_col]], .huc2) |>
        dplyr::summarise(
            overlap_km2 = sum(overlap_km2, na.rm = TRUE),
            .groups = "drop"
        ) |>
        dplyr::right_join(
            tidyr::expand_grid(
                !!rlang::sym(id_col) := sf::st_drop_geometry(zones)[[id_col]],
                .huc2 = sort(unique(h2$.huc2))
            ),
            by = c(id_col, ".huc2")
        ) |>
        dplyr::mutate(overlap_km2 = dplyr::coalesce(overlap_km2, 0)) |>
        dplyr::left_join(zones_area_tbl, by = id_col) |>
        dplyr::mutate(
            value = dplyr::if_else(
                zone_area_km2 > 0,
                pmin(pmax(overlap_km2 / zone_area_km2, 0), 1),
                NA_real_
            ),
            var = paste0("prop_cover_huc2_", .huc2)
        ) |>
        dplyr::select(dplyr::all_of(id_col), var, value) |>
        dplyr::arrange(.data[[id_col]], var)

    if (!is.null(write_csv)) {
        readr::write_csv(prop_tbl, write_csv)
    }
    prop_tbl
}
