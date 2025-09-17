# Requires: dplyr, sf, ggplot2, gganimate, scales, viridis, tidyr, magrittr (%>%)
animate_hms_smoke <- function(
    data,
    level = c("county", "tract", "zip"),
    agg = c("annual", "monthly"),
    geoms_gpkg = "clean_data/county_census_zip/canonical_2024.gpkg",
    geoms_layers = list(
        county = "counties_500k",
        tract = "tracts_500k",
        zip = "zctas_500k"
    ),
    out_gif = NULL,
    include_alaska = TRUE,
    include_hawaii = FALSE,
    bbox = NULL, # numeric c(xmin, xmax, ymin, ymax) in EPSG:4326
    fps = 2,
    width = 1000,
    height = 600,
    verbose = FALSE
) {
    stopifnot(!missing(data))
    level <- match.arg(level)
    agg <- match.arg(agg)

    .msg <- function(...) if (isTRUE(verbose)) message(...)

    pad_zip5 <- function(x) {
        x <- gsub("\\D", "", as.character(x))
        n <- nchar(x)
        if (length(n) && any(n < 5)) {
            x[n < 5] <- paste0(strrep("0", 5 - n[n < 5]), x[n < 5])
        }
        x
    }

    # ---------- Geometry (assumed EPSG:4326) ----------
    layer_name <- geoms_layers[[level]]
    if (is.null(layer_name)) {
        stop("No geometry layer configured for level='", level, "'.")
    }

    geoms <- sf::st_read(
        geoms_gpkg,
        layer = layer_name,
        quiet = !isTRUE(verbose)
    ) |>
        sf::st_make_valid() |>
        sf::st_zm(drop = TRUE)

    crs_geom <- sf::st_crs(geoms)
    if (
        is.na(crs_geom) ||
            (isTRUE(!is.na(crs_geom$epsg)) && crs_geom$epsg != 4326)
    ) {
        stop(
            "This function expects geometry in EPSG:4326; got: ",
            ifelse(is.na(crs_geom$epsg), "NA", crs_geom$epsg)
        )
    }
    .msg("Geometry CRS EPSG: ", crs_geom$epsg %||% "unknown")

    if (!"geoid" %in% names(geoms)) {
        stop("Geometry must have a 'geoid' column.")
    }
    geoms$geoid <- as.character(geoms$geoid)
    if (level == "zip") {
        geoms$geoid <- pad_zip5(geoms$geoid)
    }

    # Optional state filters via FIPS for county/tract
    if (level %in% c("county", "tract")) {
        if (!include_alaska) {
            geoms <- dplyr::filter(geoms, substr(.data$geoid, 1, 2) != "02")
        }
        if (!include_hawaii) {
            geoms <- dplyr::filter(geoms, substr(.data$geoid, 1, 2) != "15")
        }
    }

    # ---------- Data: smoke vars ----------
    smoke_df <- data |>
        dplyr::filter(
            .data$variable %in%
                c(
                    "prop_light_coverage",
                    "prop_med_coverage",
                    "prop_heavy_coverage"
                )
        ) |>
        dplyr::select(
            "geoid",
            dplyr::any_of(c("year", "month")),
            "variable",
            "value"
        ) |>
        dplyr::collect()

    if (nrow(smoke_df) == 0) {
        stop("No HMS smoke rows found in 'data'.")
    }

    smoke_df$geoid <- as.character(smoke_df$geoid)
    if (level == "zip") {
        smoke_df$geoid <- pad_zip5(smoke_df$geoid)
    }

    smoke_wide <- smoke_df |>
        tidyr::pivot_wider(
            names_from = "variable",
            values_from = "value",
            values_fill = 0
        ) |>
        dplyr::mutate(
            category = dplyr::case_when(
                .data$prop_heavy_coverage > 0 ~ "Heavy",
                .data$prop_med_coverage > 0 ~ "Medium",
                .data$prop_light_coverage > 0 ~ "Light",
                TRUE ~ "None"
            ),
            final_value = dplyr::case_when(
                .data$category == "Heavy" ~ .data$prop_heavy_coverage,
                .data$category == "Medium" ~ .data$prop_med_coverage,
                .data$category == "Light" ~ .data$prop_light_coverage,
                TRUE ~ 0
            )
        )

    # ---------- Join & clean ----------
    plot_df <- geoms |>
        dplyr::left_join(smoke_wide, by = "geoid") |>
        sf::st_as_sf()

    # DIAGNOSTICS
    n_geo <- nrow(geoms)
    n_data <- dplyr::n_distinct(smoke_wide$geoid)
    n_match <- sum(!is.na(plot_df$category) | !is.na(plot_df$final_value))
    .msg(sprintf(
        "GEOIDs in geometry: %s; in data: %s; matched: %s",
        format(n_geo, big.mark = ","),
        format(n_data, big.mark = ","),
        format(n_match, big.mark = ",")
    ))
    if (n_match == 0) {
        .msg(
            "WARNING: No GEOIDs matched between geometry and data. Check ID formats/padding."
        )
    }

    plot_df <- plot_df[!sf::st_is_empty(plot_df), ]

    # NEW: coalesce post-join so we always render a map
    plot_df <- plot_df |>
        dplyr::mutate(
            category = dplyr::coalesce(.data$category, "None"),
            final_value = dplyr::coalesce(.data$final_value, 0)
        )

    # ---------- BBOX handling (EPSG:4326 only) ----------
    if (is.null(bbox)) {
        bbox <- if (isTRUE(include_alaska)) {
            c(-170, -60, 18, 72)
        } else {
            c(-125, -66, 24, 50)
        }
    }
    bbox_sfc <- sf::st_as_sfc(sf::st_bbox(
        c(xmin = bbox[1], xmax = bbox[2], ymin = bbox[3], ymax = bbox[4]),
        crs = 4326
    ))
    geom_extent <- sf::st_as_sfc(sf::st_bbox(geoms))
    crop_area <- suppressWarnings(sf::st_intersection(bbox_sfc, geom_extent))
    if (length(crop_area) == 0 || isTRUE(sf::st_is_empty(crop_area))) {
        .msg(
            "Provided bbox does not overlap layer extent; falling back to geometry extent."
        )
        crop_area <- geom_extent
    }

    if (level == "zip" || !include_alaska || !include_hawaii) {
        plot_df <- suppressWarnings(sf::st_crop(plot_df, crop_area))
        if (nrow(plot_df) == 0) {
            stop(
                "All geometries were cropped out after bbox/extent intersection."
            )
        }
    }

    # ---------- Legend order & palette ----------
    plot_df <- dplyr::mutate(
        plot_df,
        category = factor(
            .data$category,
            levels = c("None", "Light", "Medium", "Heavy")
        )
    )
    smoke_colors <- c(
        "None" = "#F0F0F0",
        "Light" = viridis::viridis(3)[1],
        "Medium" = viridis::viridis(3)[2],
        "Heavy" = viridis::viridis(3)[3]
    )

    # ---------- Frame setup ----------
    if (agg == "annual") {
        if (!"year" %in% names(plot_df)) {
            stop("Annual data must have a 'year' column.")
        }
        plot_df <- dplyr::mutate(plot_df, frame_key = as.character(.data$year))
        title_txt <- paste0(
            tools::toTitleCase(level),
            " Annual HMS Smoke Coverage — {current_frame}"
        )
    } else {
        if (!all(c("year", "month") %in% names(plot_df))) {
            stop("Monthly data must have 'year' and 'month' columns.")
        }
        plot_df <- dplyr::mutate(
            plot_df,
            year_chr = as.character(.data$year),
            month_int = suppressWarnings(as.integer(.data$month)),
            frame_key = dplyr::if_else(
                is.na(.data$month_int),
                .data$year_chr,
                paste0(.data$year_chr, "-", sprintf("%02d", .data$month_int))
            )
        )
        title_txt <- paste0(
            tools::toTitleCase(level),
            " Monthly HMS Smoke Coverage — {current_frame}"
        )
    }

    levs <- plot_df |>
        dplyr::distinct(frame_key) |>
        dplyr::arrange(frame_key) |>
        dplyr::pull(frame_key)
    levs <- levs[!is.na(levs)]
    if (length(levs) == 0) {
        stop("No valid frames to animate.")
    }
    plot_df$frame_key <- factor(plot_df$frame_key, levels = levs)
    nframes <- length(levs)
    .msg("Frames: ", nframes)

    # ---------- Plot ----------
    # Basemap layer to guarantee something draws even when data are all "None"
    basemap <- ggplot2::geom_sf(
        data = sf::st_as_sf(sf::st_geometry(plot_df)),
        fill = "#F7F7F7",
        color = NA,
        inherit.aes = FALSE
    )

    p <- ggplot2::ggplot(plot_df) +
        basemap +
        ggplot2::geom_sf(
            ggplot2::aes(fill = .data$category, alpha = .data$final_value),
            color = NA
        ) +
        ggplot2::scale_fill_manual(
            values = smoke_colors,
            breaks = c("None", "Light", "Medium", "Heavy"),
            drop = FALSE,
            na.translate = FALSE,
            na.value = "grey80"
        ) +
        ggplot2::scale_alpha(range = c(0.25, 1), guide = "none") +
        ggplot2::coord_sf(
            xlim = c(bbox[1], bbox[2]),
            ylim = c(bbox[3], bbox[4]),
            expand = FALSE
        ) +
        ggplot2::theme_void() +
        ggplot2::theme(
            legend.position = "bottom",
            legend.title = ggplot2::element_text(size = 10),
            legend.text = ggplot2::element_text(size = 8),
            plot.title = ggplot2::element_text(
                hjust = 0.5,
                size = 14,
                face = "bold"
            ),
            plot.margin = ggplot2::margin(8, 12, 8, 12)
        ) +
        ggplot2::labs(title = title_txt, fill = "Smoke Category") +
        gganimate::transition_manual(.data$frame_key)

    # ---------- Animate ----------
    anim <- gganimate::animate(
        p,
        nframes = nframes,
        fps = fps,
        width = width,
        height = height,
        units = "px",
        renderer = gganimate::gifski_renderer(),
        device = "ragg_png"
    )

    # ---------- Save ----------
    if (is.null(out_gif)) {
        dir.create("figures", showWarnings = FALSE, recursive = TRUE)
        out_gif <- file.path(
            "figures",
            paste0(level, "_", agg, "_hms_smoke.gif")
        )
    }
    gganimate::anim_save(out_gif, animation = anim)
    invisible(out_gif)
}
