animate_hms_smoke <- function(
    data,
    level = c("county", "tract", "zip"), # added "zip"
    agg = c("annual", "monthly"),
    geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    out_gif = NULL,
    include_alaska = TRUE,
    include_hawaii = FALSE
) {
    stopifnot(!missing(data))
    level <- match.arg(level)
    agg <- match.arg(agg)

    # Geometry layer
    geom_layer <- if (level == "county") {
        "counties_500k"
    } else if (level == "tract") {
        "tracts_500k"
    } else {
        # level == "zip"
        "zctas_500k"
    }

    # Read geometries
    geoms <- sf::st_read(geoms_gpkg, layer = geom_layer, quiet = TRUE) |>
        sf::st_make_valid() |>
        sf::st_zm(drop = TRUE)

    # Smoke vars
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

    # Wide + category/final_value
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

    # Join geometries (drop empties without using '.' pronoun)
    plot_df <- geoms |>
        dplyr::left_join(smoke_wide, by = "geoid") |>
        sf::st_as_sf()
    plot_df <- plot_df[!sf::st_is_empty(plot_df), ]

    # Optional state filters (AK=02, HI=15)
    # For ZCTAs, geoid is not state-FIPS-prefixed; skip these filters.
    if (level != "zip") {
        if (!include_alaska) {
            plot_df <- dplyr::filter(plot_df, substr(.data$geoid, 1, 2) != "02")
        }
        if (!include_hawaii) {
            plot_df <- dplyr::filter(plot_df, substr(.data$geoid, 1, 2) != "15")
        }
    }

    # Legend order
    plot_df <- dplyr::mutate(
        plot_df,
        category = factor(
            .data$category,
            levels = c("None", "Light", "Medium", "Heavy")
        )
    )

    # Viridis discrete colors, lightest for "None", darkest for "Heavy"
    smoke_colors <- setNames(
        rev(viridis::viridis(4, option = "D")),
        c("None", "Light", "Medium", "Heavy")
    )

    # ---- Robust frame setup (handles year == "normal" and character/NA month) ----
    if (agg == "annual") {
        if (!"year" %in% names(plot_df)) {
            stop("Annual data must have a 'year' column.")
        }
        plot_df <- dplyr::mutate(plot_df, frame_key = as.character(.data$year))
        frame_aes <- gganimate::transition_manual(.data$frame_key)
        title_txt <- paste0(
            toupper(substr(level, 1, 1)),
            substr(level, 2, nchar(level)),
            " Annual HMS Smoke Coverage — {current_frame}"
        )
        frame_vals <- plot_df$frame_key
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
                .data$year_chr, # e.g., "normal" with month NA
                paste0(.data$year_chr, "-", sprintf("%02d", .data$month_int))
            )
        )
        frame_aes <- gganimate::transition_manual(.data$frame_key)
        title_txt <- paste0(
            toupper(substr(level, 1, 1)),
            substr(level, 2, nchar(level)),
            " Monthly HMS Smoke Coverage — {current_frame}"
        )
        frame_vals <- plot_df$frame_key
    }

    # Plot
    p <- ggplot2::ggplot(plot_df) +
        ggplot2::geom_sf(
            ggplot2::aes(fill = .data$category, alpha = .data$final_value),
            color = NA
        ) +
        ggplot2::scale_fill_manual(
            values = smoke_colors,
            breaks = c("None", "Light", "Medium", "Heavy"),
            na.value = "grey80"
        ) +
        ggplot2::scale_alpha(range = c(0.2, 1), guide = "none") +
        ggplot2::coord_sf(
            xlim = c(-170, -67),
            ylim = c(20, 72),
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
            )
        ) +
        ggplot2::labs(title = title_txt, fill = "Smoke Category") +
        frame_aes

    # Frames
    nframes <- max(1L, length(unique(stats::na.omit(frame_vals))))

    # Animate
    anim <- gganimate::animate(
        p,
        nframes = nframes,
        fps = 2,
        width = 1000,
        height = 600,
        units = "px",
        renderer = gganimate::gifski_renderer(),
        device = "ragg_png"
    )

    # Save
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
