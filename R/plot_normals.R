# Requires: dplyr, sf, ggplot2, viridis, ragg, scales

plot_normal_map <- function(
    var,
    level = c("county", "tract", "zip"),
    include_alaska = TRUE,
    include_hawaii = FALSE,
    legend_title = var,
    palette = "viridis",
    direction = 1,
    bbox = c(-140, -60, 18, 72), # interpreted in bbox_crs
    bbox_crs = 4326, # CRS of bbox (default lon/lat)
    limits = NULL,
    out_file = NULL,
    title = NULL,
    dataset = county_annual, # Arrow/tibble with geoid, year, variable, value
    geoms_gpkg = "clean_data/county_census_zip/canonical_2024.gpkg",
    geoms_layers = list(
        county = "counties_500k",
        tract = "tracts_500k",
        zip = "zctas_500k"
    ),
    trans = "log10", # log-transform outcome variable by default
    boundary = c("none", "white"), # boundary toggle
    boundary_size = 0.0001
) {
    level <- match.arg(level)
    boundary <- match.arg(boundary)
    stopifnot(is.numeric(boundary_size), boundary_size >= 0)

    # --- helpers ---
    pad_zip5 <- function(x) {
        x <- gsub("\\D", "", as.character(x))
        n <- nchar(x)
        if (length(n) && any(n < 5)) {
            x[n < 5] <- paste0(strrep("0", 5 - n[n < 5]), x[n < 5])
        }
        x
    }

    # --- Read geometries (stay in native CRS, e.g., EPSG:5070 for ZCTAs) ---
    geom_layer <- geoms_layers[[level]]
    if (is.null(geom_layer)) {
        stop("No geometry layer configured for level='", level, "'.")
    }
    geom <- sf::st_read(geoms_gpkg, layer = geom_layer, quiet = TRUE)
    geom <- sf::st_make_valid(geom)
    geom <- sf::st_zm(geom, drop = TRUE)

    crs_geom <- sf::st_crs(geom)
    if (is.na(crs_geom)) {
        stop("Geometry layer '", geom_layer, "' has no CRS assigned.")
    }

    if (!"geoid" %in% names(geom)) {
        stop("Geometry must have a 'geoid' column.")
    }
    geom$geoid <- as.character(geom$geoid)
    if (level == "zip") {
        geom$geoid <- pad_zip5(geom$geoid)
    }

    # Drop states via FIPS only for county/tract
    if (level %in% c("county", "tract")) {
        drop_states <- c(
            if (!include_alaska) "02" else NULL,
            if (!include_hawaii) "15" else NULL
        )
        if (length(drop_states)) {
            geom <- dplyr::filter(geom, !substr(geoid, 1, 2) %in% drop_states)
        }
    }

    # --- Arrow-safe: collect then filter on var + "normal" ---
    df <- dataset %>%
        dplyr::select(geoid, year, variable, value) %>%
        dplyr::filter(year == "normal") %>%
        dplyr::collect() %>%
        dplyr::filter(variable == var) %>%
        dplyr::select(geoid, value)

    if (nrow(df) == 0) {
        stop(sprintf("No rows found for normal variable: %s", var))
    }

    df$geoid <- as.character(df$geoid)
    if (level == "zip") {
        df$geoid <- pad_zip5(df$geoid)
    }

    # --- Join and filter empties ---
    plot_df <- dplyr::left_join(geom, df, by = "geoid")
    keep <- !sf::st_is_empty(sf::st_geometry(plot_df))
    if (!all(keep)) {
        plot_df <- plot_df[keep, , drop = FALSE]
    }
    plot_df <- sf::st_make_valid(plot_df)

    # --- For ZIPs (and as a safety), crop using bbox in bbox_crs reprojected to geometry CRS ---
    if (is.null(bbox)) {
        # sensible defaults in lon/lat; reproject below
        bbox <- if (isTRUE(include_alaska)) {
            c(-170, -60, 18, 72)
        } else {
            c(-125, -66, 24, 50)
        }
        bbox_crs <- 4326
    }
    bbox_sfc_input <- sf::st_as_sfc(
        sf::st_bbox(
            c(xmin = bbox[1], xmax = bbox[2], ymin = bbox[3], ymax = bbox[4]),
            crs = bbox_crs
        )
    )
    bbox_sfc <- if (sf::st_crs(bbox_sfc_input) != crs_geom) {
        sf::st_transform(bbox_sfc_input, crs_geom)
    } else {
        bbox_sfc_input
    }

    if (level == "zip" || !include_alaska || !include_hawaii) {
        plot_df <- suppressWarnings(sf::st_crop(plot_df, bbox_sfc))
        if (nrow(plot_df) == 0) {
            stop("All geometries were cropped out by the provided bbox.")
        }
    }

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
    border_sz <- if (boundary == "white") boundary_size else 0

    p <- ggplot2::ggplot(plot_df) +
        ggplot2::geom_sf(
            ggplot2::aes(fill = value),
            color = border_col,
            linewidth = border_sz
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
        ggplot2::coord_sf(expand = FALSE) + # axes in geometry CRS
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
        dir.create("figures", showWarnings = FALSE, recursive = TRUE)
        out_file <- file.path(
            "figures",
            sprintf("%s_normal_%s.png", level, var)
        )
    }

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
