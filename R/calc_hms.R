hms_fire_exposure <- function(
    hms_dir = "clean_data/hms_clean",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = c("county", "tract"),
    zone_layer = NULL,
    agg = c("annual", "monthly"),
    id_col = "geoid",
    write_csv = NULL
) {
    level <- match.arg(level)
    agg <- match.arg(agg)
    if (is.null(zone_layer)) {
        zone_layer <- if (level == "county") "counties_500k" else "tracts_500k"
    }

    # --- Dependencies check ---
    reqs <- c(
        "sf",
        "dplyr",
        "stringr",
        "purrr",
        "readr",
        "tidyr",
        "lubridate",
        "units",
        "lwgeom"
    )
    ok <- vapply(reqs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
    if (!all(ok)) {
        stop(
            "Missing packages: ",
            paste(reqs[!ok], collapse = ", "),
            call. = FALSE
        )
    }

    # --- Read zones ---
    zones_raw <- sf::st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid()

    zones_aea <- zones_raw |>
        sf::st_transform(5070) |>
        dplyr::select(!!id_col := all_of(id_col), geom)

    zone_area <- tibble::tibble(
        !!id_col := zones_aea[[id_col]],
        zone_area_m2 = as.numeric(sf::st_area(zones_aea))
    )

    # --- File list ---
    hms_files <- list.files(
        hms_dir,
        pattern = "^hms_\\d{4}_\\d{2}\\.gpkg$",
        full.names = TRUE
    ) |>
        sort()

    # --- Helper functions ---
    dens_levels <- c("Light", "Medium", "Heavy")

    clean_polys <- function(x, snap = 10) {
        x |>
            sf::st_set_precision(snap) |>
            lwgeom::st_snap_to_grid(size = snap) |>
            sf::st_make_valid() |>
            sf::st_collection_extract("POLYGON", warn = FALSE) |>
            sf::st_buffer(0)
    }

    safe_intersection <- function(a, b) {
        out <- try(suppressWarnings(sf::st_intersection(a, b)), silent = TRUE)
        if (!inherits(out, "try-error")) {
            return(out)
        }

        b2 <- clean_polys(b)
        out <- try(suppressWarnings(sf::st_intersection(a, b2)), silent = TRUE)
        if (!inherits(out, "try-error")) {
            return(out)
        }

        a2 <- clean_polys(a)
        out <- try(suppressWarnings(sf::st_intersection(a2, b2)), silent = TRUE)
        if (!inherits(out, "try-error")) {
            return(out)
        }

        suppressWarnings(sf::st_intersection(
            clean_polys(a, 50),
            clean_polys(b, 50)
        ))
    }

    dissolve_by_date_density <- function(g5070) {
        g5070 |>
            dplyr::group_by(Date, Density) |>
            dplyr::summarise(.groups = "drop") |>
            clean_polys()
    }

    hms_daily_props_for_month <- function(gpkg_file, zones_aea, zone_area_tbl) {
        layer <- tools::file_path_sans_ext(basename(gpkg_file))

        g <- sf::st_read(gpkg_file, layer = layer, quiet = TRUE) |>
            sf::st_make_valid() |>
            dplyr::filter(!is.na(Density), Density %in% dens_levels) |>
            dplyr::mutate(
                Density = factor(Density, levels = dens_levels),
                Date = as.Date(Date)
            ) |>
            sf::st_transform(5070) |>
            sf::st_collection_extract("POLYGON", warn = FALSE)

        if (nrow(g) == 0) {
            return(tibble::tibble(
                !!id_col := character(),
                Date = as.Date(character()),
                Density = factor(character(), levels = dens_levels),
                prop = numeric()
            ))
        }

        g_diss <- dissolve_by_date_density(g)
        int <- safe_intersection(zones_aea, g_diss)
        if (nrow(int) == 0) {
            return(tibble::tibble(
                !!id_col := character(),
                Date = as.Date(character()),
                Density = factor(character(), levels = dens_levels),
                prop = numeric()
            ))
        }

        int$iarea_m2 <- as.numeric(sf::st_area(int))

        int |>
            sf::st_drop_geometry() |>
            dplyr::group_by(.data[[id_col]], Date, Density) |>
            dplyr::summarise(area_m2 = sum(iarea_m2), .groups = "drop") |>
            dplyr::left_join(zone_area_tbl, by = id_col) |>
            dplyr::mutate(prop = pmin(area_m2 / zone_area_m2, 1)) |>
            dplyr::select(!!id_col, Date, Density, prop)
    }

    # --- Run daily calculations ---
    daily_props <- purrr::map_dfr(
        hms_files,
        hms_daily_props_for_month,
        zones_aea = zones_aea,
        zone_area_tbl = zone_area
    )

    if (nrow(daily_props) == 0) {
        stop(
            "No intersections found. Check CRS, study region, and file naming."
        )
    }

    # --- Aggregate to month or year ---
    daily_props <- daily_props |>
        dplyr::mutate(
            year = lubridate::year(Date),
            month = lubridate::month(Date)
        )

    if (agg == "annual") {
        days_per_year <- tibble::tibble(
            year = unique(daily_props$year),
            days_in_year = lubridate::yday(as.Date(paste0(
                unique(daily_props$year),
                "-12-31"
            )))
        )

        agg_props <- daily_props |>
            dplyr::group_by(.data[[id_col]], year, Density) |>
            dplyr::summarise(
                sum_prop = sum(prop, na.rm = TRUE),
                .groups = "drop"
            ) |>
            dplyr::left_join(days_per_year, by = "year") |>
            dplyr::mutate(value = sum_prop / days_in_year) |>
            dplyr::select(!!id_col, year, Density, value)
    } else {
        days_per_month <- daily_props |>
            dplyr::group_by(year, month) |>
            dplyr::summarise(
                days_in_month = dplyr::n_distinct(Date),
                .groups = "drop"
            )

        agg_props <- daily_props |>
            dplyr::group_by(.data[[id_col]], year, month, Density) |>
            dplyr::summarise(
                sum_prop = sum(prop, na.rm = TRUE),
                .groups = "drop"
            ) |>
            dplyr::left_join(days_per_month, by = c("year", "month")) |>
            dplyr::mutate(value = sum_prop / days_in_month) |>
            dplyr::select(!!id_col, year, month, Density, value)
    }

    # --- Fill missing combinations ---
    dens_labels <- c(
        Light = "prop_light_coverage",
        Medium = "prop_med_coverage",
        Heavy = "prop_heavy_coverage"
    )

    if (agg == "annual") {
        filled <- tidyr::complete(
            agg_props,
            !!id_col := zones_aea[[id_col]],
            year = unique(daily_props$year),
            Density = factor(dens_levels, levels = dens_levels),
            fill = list(value = 0)
        ) |>
            dplyr::mutate(var = dplyr::recode(Density, !!!dens_labels)) |>
            dplyr::select(!!id_col, year, var, value)
    } else {
        filled <- tidyr::complete(
            agg_props,
            !!id_col := zones_aea[[id_col]],
            year = unique(daily_props$year),
            month = unique(daily_props$month),
            Density = factor(dens_levels, levels = dens_levels),
            fill = list(value = 0)
        ) |>
            dplyr::mutate(var = dplyr::recode(Density, !!!dens_labels)) |>
            dplyr::select(!!id_col, year, month, var, value)
    }

    # --- Save if requested ---
    if (!is.null(write_csv)) {
        readr::write_csv(filled, write_csv)
    }

    filled
}
