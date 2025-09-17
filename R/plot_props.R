# Requires: sf, dplyr, tidyr, ggplot2, cowplot, ragg
plot_proportion_panel <- function(
    vars, # e.g., c("prop_a","prop_b","prop_c")
    pretty_names, # named chr: c(prop_a="Name A", ...)
    high_colors = NULL, # kept for backward compat (not used here)
    target_year, # e.g., 2021 or "normal"
    outfile = NULL,
    dataset, # arrow/tibble with geoid, year, variable, value
    level = c("county", "tract", "zip"),
    geoms_gpkg = "clean_data/county_census_zip/canonical_2024.gpkg",
    geoms_layers = list(
        county = "counties_500k",
        tract = "tracts_500k",
        zip = "zctas_500k" # ZCTA layer name
    ),
    include_alaska = FALSE,
    include_hawaii = FALSE,
    bbox = NULL, # lon/lat bbox; if NULL auto by flags
    plot_crs = 4326 # draw/output in lon/lat consistently
) {
    stopifnot(length(vars) == 3L)
    level <- match.arg(level)

    pad_zip5 <- function(x) {
        x <- gsub("\\D", "", as.character(x))
        ifelse(nchar(x) < 5, sprintf("%05s", x), x)
    }

    # ---- Geometry ----
    layer <- geoms_layers[[level]]
    if (is.null(layer)) {
        stop("No geometry layer configured for level='", level, "'.")
    }

    geom <- sf::st_read(geoms_gpkg, layer = layer, quiet = TRUE) |>
        sf::st_make_valid() |>
        sf::st_zm(drop = TRUE)

    if (!"geoid" %in% names(geom)) {
        stop("Geometry must have a 'geoid' column.")
    }
    geom$geoid <- as.character(geom$geoid)
    if (level == "zip") {
        geom$geoid <- pad_zip5(geom$geoid)
    }

    # Drop AK/HI only for county/tract via FIPS
    if (level %in% c("county", "tract")) {
        if (!include_alaska) {
            geom <- dplyr::filter(geom, substr(geoid, 1, 2) != "02")
        }
        if (!include_hawaii) {
            geom <- dplyr::filter(geom, substr(geoid, 1, 2) != "15")
        }
    }

    # ---- Data (wide) ----
    wide_df <- dataset |>
        dplyr::filter(.data$variable %in% vars, .data$year == target_year) |>
        dplyr::select(geoid, variable, value) |>
        dplyr::collect() |>
        dplyr::mutate(geoid = as.character(geoid))

    if (nrow(wide_df) == 0) {
        stop("No rows found for requested vars/target_year in dataset.")
    }

    if (level == "zip") {
        wide_df$geoid <- pad_zip5(wide_df$geoid)
    }

    # Guardrail: basic length check
    len_ok <- switch(
        level,
        county = all(nchar(unique(wide_df$geoid)) %in% c(5, 4, 3, 2)), # counties are 5 (state+county), but allow oddities
        tract = all(nchar(unique(wide_df$geoid)) >= 11),
        zip = all(nchar(unique(wide_df$geoid)) == 5)
    )
    if (!len_ok) {
        stop(
            "Dataset GEOIDs don't look like '",
            level,
            "' IDs. Did you pass the wrong dataset for this level?"
        )
    }

    wide_df <- tidyr::pivot_wider(
        wide_df,
        names_from = variable,
        values_from = value
    )

    # ---- Join ----
    plot_df <- dplyr::left_join(geom, wide_df, by = "geoid")
    # Count matches to catch empty join early
    matched <- sum(Reduce(`|`, lapply(vars, function(v) !is.na(plot_df[[v]]))))
    if (matched == 0) {
        stop(
            "No data matched the geometry GEOIDs. ",
            "Check that `dataset` corresponds to `level` (e.g., zip data for level='zip')."
        )
    }

    # ---- Bbox defaults (lon/lat) ----
    if (is.null(bbox)) {
        bbox <- if (include_alaska && !include_hawaii) {
            c(-170, -60, 18, 72)
        } else if (!include_alaska && !include_hawaii) {
            c(-125, -66, 24, 50)
        } else if (include_alaska && include_hawaii) {
            c(-170, -60, 18, 72)
        } else {
            c(-160, -60, 18, 72)
        }
    }

    # ---- Transform to plotting CRS for consistent axes ----
    plot_df <- sf::st_transform(plot_df, plot_crs)

    # ---- Arg checks for labels/colors ----
    missing_names <- setdiff(vars, names(pretty_names))
    if (length(missing_names)) {
        stop(
            "Missing pretty_names for: ",
            paste(missing_names, collapse = ", ")
        )
    }

    # ---- Single-map builder ----
    map_one <- function(col_name) {
        ggplot2::ggplot(plot_df) +
            ggplot2::geom_sf(
                ggplot2::aes(fill = .data[[col_name]]),
                color = NA
            ) +
            ggplot2::scale_fill_viridis_c(
                option = "viridis",
                direction = 1,
                limits = c(0, 1),
                oob = scales::squish,
                name = "Proportion",
                na.value = "grey90",
                breaks = c(0, 0.25, 0.5, 0.75, 1)
            ) +
            ggplot2::coord_sf(
                xlim = bbox[1:2],
                ylim = bbox[3:4],
                crs = sf::st_crs(plot_crs),
                default_crs = sf::st_crs(plot_crs),
                expand = FALSE
            ) +
            ggplot2::labs(
                title = paste0(pretty_names[[col_name]], " - ", target_year)
            ) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
                plot.title = ggplot2::element_text(
                    hjust = 0.5,
                    size = 12,
                    face = "bold"
                ),
                legend.position = "bottom",
                legend.title = ggplot2::element_text(size = 9),
                legend.text = ggplot2::element_text(size = 8)
            )
    }

    p_list <- lapply(vars, map_one)
    panel <- cowplot::plot_grid(plotlist = p_list, ncol = 3)

    if (!is.null(outfile)) {
        dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
        ggplot2::ggsave(
            filename = outfile,
            plot = panel,
            device = ragg::agg_png,
            width = 15,
            height = 6,
            units = "in",
            dpi = 150
        )
        message("Saved: ", normalizePath(outfile))
    }

    panel
}
