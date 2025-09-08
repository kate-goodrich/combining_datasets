plot_normal_map <- function(
    var,
    level = c("county", "tract"),
    include_alaska = TRUE,
    include_hawaii = FALSE,
    legend_title = var,
    palette = "viridis",
    direction = 1,
    bbox = c(-140, -60, 18, 72),
    limits = NULL,
    out_file = NULL,
    title = NULL,
    dataset = county_annual, # Arrow/tibble with geoid, year, variable, value
    geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    trans = "log10", # log-transform outcome variable by default
    boundary = c("none", "white"), # boundary toggle
    boundary_size = 0.0001
) {
    level <- match.arg(level)
    boundary <- match.arg(boundary)
    stopifnot(is.numeric(boundary_size), boundary_size >= 0)
    geom_layer <- if (level == "county") "counties_500k" else "tracts_500k"

    # --- Read zones ---
    geom <- sf::st_read(geoms_gpkg, layer = geom_layer, quiet = TRUE)
    geom <- sf::st_make_valid(geom)
    geom <- sf::st_zm(geom, drop = TRUE)

    # Drop states if requested (AK=02, HI=15)
    drop_states <- c(
        if (!include_alaska) "02" else NULL,
        if (!include_hawaii) "15" else NULL
    )
    if (length(drop_states)) {
        geom <- dplyr::filter(geom, !substr(geoid, 1, 2) %in% drop_states)
    }

    # --- Arrow-safe: collect then filter on var ---
    df <- dataset %>%
        dplyr::select(geoid, year, variable, value) %>%
        dplyr::filter(year == "normal") %>%
        dplyr::collect() %>%
        dplyr::filter(variable == var) %>%
        dplyr::select(geoid, value)

    if (nrow(df) == 0) {
        stop(sprintf("No rows found for normal variable: %s", var))
    }

    # --- Join and filter empties ---
    plot_df <- dplyr::left_join(geom, df, by = "geoid")
    keep <- !sf::st_is_empty(sf::st_geometry(plot_df))
    if (!all(keep)) {
        plot_df <- plot_df[keep, , drop = FALSE]
    }
    plot_df <- sf::st_make_valid(plot_df)

    # ---- Stable color limits (ensure valid for log scales) ----
    vals <- plot_df$value
    if (!is.null(trans) && grepl("^log", trans, ignore.case = TRUE)) {
        vals_pos <- vals[is.finite(vals) & !is.na(vals) & vals > 0]
        if (length(vals_pos) == 0) {
            stop("All values are non-positive; cannot use log scale.")
        }
        if (is.null(limits)) {
            if (identical(var, "soltotal")) {
                limits <- c(max(1e-6, 0.001), 20)
            } else {
                q <- stats::quantile(vals_pos, c(0.02, 0.98), na.rm = TRUE)
                limits <- c(max(q[1], min(vals_pos) * 0.5), q[2])
            }
        } else {
            if (limits[1] <= 0) limits[1] <- max(1e-6, min(vals_pos) * 0.5)
        }
    } else {
        if (is.null(limits)) {
            if (identical(var, "soltotal")) {
                limits <- c(0, 20)
            } else {
                q <- stats::quantile(vals, c(0.02, 0.98), na.rm = TRUE)
                limits <- as.numeric(q)
            }
        }
    }

    if (is.null(title)) {
        title <- sprintf("%s normal (%s)", var, level)
    }

    # ---- Plot ----
    border_col <- if (boundary == "white") "white" else NA
    border_size <- if (boundary == "white") boundary_size else 0

    p <- ggplot2::ggplot(plot_df) +
        ggplot2::geom_sf(
            ggplot2::aes(fill = value),
            color = border_col,
            linewidth = border_size
        ) +
        ggplot2::scale_fill_viridis_c(
            option = palette,
            direction = direction,
            limits = limits,
            oob = scales::squish,
            name = legend_title,
            na.value = "grey90",
            trans = trans
        ) +
        ggplot2::coord_sf(xlim = bbox[1:2], ylim = bbox[3:4], expand = FALSE) +
        ggplot2::labs(title = title) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(
                hjust = 0.5,
                size = 12,
                face = "bold"
            ),
            legend.position = "bottom",
            legend.title = ggplot2::element_text(size = 9),
            legend.text = ggplot2::element_text(size = 8)
        )

    if (is.null(out_file)) {
        out_file <- file.path(
            "figures",
            sprintf("%s_normal_%s.png", level, var)
        )
    }

    dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(
        out_file,
        p,
        device = ragg::agg_png,
        width = 12,
        height = 6,
        units = "in",
        dpi = 600
    )
    message("Saved: ", normalizePath(out_file))
    invisible(out_file)
}
