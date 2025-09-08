# ---- Consolidated 3-map proportion panel ----
plot_proportion_panel <- function(
    vars, # c("var_a","var_b","var_c")
    pretty_names, # named chr: c(var_a="Name A", ...)
    high_colors, # named chr: c(var_a="#hex", ...)
    target_year, # e.g., 2021 or "static"
    outfile = NULL, # path to save; if NULL, don't save
    dataset = county_annual, # arrow/tibble with geoid, year, variable, value
    level = c("county", "tract"),
    geoms_gpkg = ds("clean_data/county_census/canonical_2024.gpkg"),
    layer_counties = "counties_500k",
    include_alaska = FALSE,
    include_hawaii = FALSE,
    bbox = NULL # c(xmin, xmax, ymin, ymax); if NULL, auto by flags
) {
    stopifnot(length(vars) == 3L)

    level <- match.arg(level)

    # --- Geometry ---
    counties <- sf::st_read(geoms_gpkg, layer = layer_counties, quiet = TRUE) |>
        sf::st_make_valid()

    # FIPS: AK=02, HI=15  (apply filters conditionally)
    if (!include_alaska) {
        counties <- dplyr::filter(counties, substr(geoid, 1, 2) != "02")
    }
    if (!include_hawaii) {
        counties <- dplyr::filter(counties, substr(geoid, 1, 2) != "15")
    }

    # Default bbox if not provided
    if (is.null(bbox)) {
        if (include_alaska && !include_hawaii) {
            bbox <- c(-170, -60, 18, 72) # AK + CONUS
        } else if (!include_alaska && !include_hawaii) {
            bbox <- c(-125, -66, 24, 50) # CONUS
        } else if (include_alaska && include_hawaii) {
            bbox <- c(-170, -60, 18, 72) # simple wide view (HI may be off-screen)
        } else {
            bbox <- c(-160, -60, 18, 72) # catch-all
        }
    }

    # --- Data ---
    wide_df <- dataset |>
        dplyr::filter(.data$variable %in% vars, .data$year == target_year) |>
        dplyr::select(geoid, variable, value) |>
        dplyr::collect() |>
        tidyr::pivot_wider(names_from = variable, values_from = value)

    plot_df <- dplyr::left_join(counties, wide_df, by = "geoid")

    # --- Guard: ensure names are present in color/name vectors ---
    missing_names <- setdiff(vars, names(pretty_names))
    if (length(missing_names)) {
        stop(
            "Missing pretty_names for: ",
            paste(missing_names, collapse = ", ")
        )
    }
    missing_colors <- setdiff(vars, names(high_colors))
    if (length(missing_colors)) {
        stop(
            "Missing high_colors for: ",
            paste(missing_colors, collapse = ", ")
        )
    }

    # --- Single map builder ---
    map_one <- function(col_name) {
        ggplot2::ggplot(plot_df) +
            ggplot2::geom_sf(
                ggplot2::aes(fill = .data[[col_name]]),
                color = NA
            ) +
            ggplot2::scale_fill_viridis_c(
                option = "viridis", # or "mako","plasma","cividis" if you prefer
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

    return(panel)
}
