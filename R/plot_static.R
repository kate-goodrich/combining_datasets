# ---- Static map (county/tract/zip) with viridis; minimal changes for ZIPs ----
plot_static_map <- function(
    var, # string or character vector
    level = c("county", "tract", "zip"),
    include_alaska = TRUE,
    include_hawaii = FALSE,
    legend_title = if (length(var) == 1) var else "Value",
    palette = "viridis",
    direction = 1,
    bbox = NULL, # c(xmin, xmax, ymin, ymax); auto if NULL
    out_file = NULL, # auto filename if NULL
    title = NULL, # auto title if NULL
    dataset, # REQUIRED: arrow/tibble with geoid, year, variable, value
    geoms_gpkg = "clean_data/county_census_zip/canonical_2024.gpkg",
    trans = "identity", # e.g. "log10"
    na_fill = "grey90"
) {
    level <- match.arg(level)
    geom_layer <- switch(
        level,
        county = "counties_500k",
        tract = "tracts_500k",
        zip = "zctas_500k"
    )

    # helper for ZIP padding
    pad_zip5 <- function(x) {
        x <- gsub("\\D", "", as.character(x))
        ifelse(nchar(x) < 5, sprintf("%05s", x), x)
    }

    # --- Read zones ---
    geom <- sf::st_read(geoms_gpkg, layer = geom_layer, quiet = TRUE)
    geom <- sf::st_make_valid(geom)
    geom <- sf::st_zm(geom, drop = TRUE)

    # Normalize geometry IDs
    if (!"geoid" %in% names(geom)) {
        stop("Geometry must have a 'geoid' column.")
    }
    geom$geoid <- as.character(geom$geoid)
    if (level == "zip") {
        geom$geoid <- pad_zip5(geom$geoid)
    }

    # Drop states if requested (AK=02, HI=15) — only meaningful for county/tract
    if (level %in% c("county", "tract")) {
        drop_states <- c(
            if (!include_alaska) "02" else NULL,
            if (!include_hawaii) "15" else NULL
        )
        if (length(drop_states)) {
            geom <- dplyr::filter(geom, !substr(geoid, 1, 2) %in% drop_states)
        }
    }

    # --- Data (collect first; then filter) ---
    df <- dataset %>%
        dplyr::select(geoid, year, variable, value) %>%
        dplyr::collect() %>%
        dplyr::filter(year == "static", variable %in% var) %>%
        dplyr::select(geoid, variable, value)

    if (nrow(df) == 0L) {
        stop(sprintf(
            "No rows found for variable(s): %s",
            paste(var, collapse = ", ")
        ))
    }

    # Ensure join keys are character (and pad for ZIPs)
    df$geoid <- as.character(df$geoid)
    if (level == "zip") {
        df$geoid <- pad_zip5(df$geoid)
    }

    # --- Join and clean ---
    plot_df <- dplyr::left_join(geom, df, by = "geoid")
    keep <- !sf::st_is_empty(sf::st_geometry(plot_df))
    if (!all(keep)) {
        plot_df <- plot_df[keep, , drop = FALSE]
    }
    plot_df <- sf::st_make_valid(plot_df)

    # --- Defaults ---
    if (is.null(bbox)) {
        bbox <- if (include_alaska) {
            c(-170, -60, 18, 72)
        } else {
            c(-125, -66, 24, 50)
        }
    }
    if (is.null(title)) {
        title <- if (length(var) == 1) {
            sprintf("%s (%s)", var, level)
        } else {
            sprintf("Static variables (%s)", level)
        }
    }
    if (is.null(out_file)) {
        out_file <- if (length(var) == 1) {
            file.path("figures", sprintf("%s_static_%s.png", level, var))
        } else {
            file.path("figures", sprintf("%s_static_panel.png", level))
        }
    }

    # --- Plot ---
    p <- ggplot2::ggplot(plot_df) +
        ggplot2::geom_sf(ggplot2::aes(fill = value), color = NA) +
        ggplot2::scale_fill_viridis_c(
            option = palette,
            direction = direction,
            name = legend_title,
            na.value = na_fill,
            trans = trans
        ) +
        ggplot2::coord_sf(
            xlim = c(bbox[1], bbox[2]),
            ylim = c(bbox[3], bbox[4]),
            expand = FALSE
        ) +
        ggplot2::labs(title = title) +
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

    if (length(var) > 1) {
        p <- p + ggplot2::facet_wrap(~variable, ncol = min(length(var), 3))
    }

    dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(
        filename = out_file,
        plot = p,
        device = ragg::agg_png,
        width = 12,
        height = 6,
        units = "in",
        dpi = 150
    )
    invisible(out_file)
}
