# Requires: dplyr, sf, ggplot2, gganimate, scales, viridis (for palettes), magrittr (for %>%)
animate_geo_gif <- function(
    var,
    level = c("county", "tract"),
    agg = c("annual", "monthly"),
    data = NULL, # optional Arrow Dataset/tibble; otherwise falls back to get("<level>_<agg>")
    geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    include_alaska = TRUE,
    include_hawaii = FALSE,
    bbox = NULL, # c(xmin, xmax, ymin, ymax); auto if NULL
    legend_title = var,
    title = NULL, # defaults to "... — {current_frame}"
    palette = "viridis",
    direction = 1,
    trans = "identity",
    labels = scales::label_number(accuracy = 0.1),
    na_fill = "grey80",
    width = 1000,
    height = 600,
    fps = 3,
    out_path = NULL, # default: figures/<var>_<level>_<agg>.gif
    value_fun = NULL, # e.g., function(x) x * 1e9
    drop_na_time = TRUE,
    tween_shapes = FALSE
) {
    level <- match.arg(level)
    agg <- match.arg(agg)

    # --- Geometry ---
    geom_layer <- if (level == "county") "counties_500k" else "tracts_500k"
    geom <- sf::st_read(geoms_gpkg, layer = geom_layer, quiet = TRUE)
    geom <- sf::st_make_valid(geom)
    geom <- sf::st_zm(geom, drop = TRUE)

    # Optionally drop AK/HI (FIPS: AK=02, HI=15)
    drop_states <- c(
        if (!include_alaska) "02" else NULL,
        if (!include_hawaii) "15" else NULL
    )
    if (length(drop_states)) {
        geom <- dplyr::filter(geom, !substr(geoid, 1, 2) %in% drop_states)
    }

    # --- Data source ---
    if (is.null(data)) {
        nm <- sprintf("%s_%s", level, agg)
        if (!exists(nm, inherits = TRUE)) {
            stop(
                "Dataset '",
                nm,
                "' not found. Pass via `data=` or create object ",
                nm,
                "."
            )
        }
        ds_obj <- get(nm, inherits = TRUE)
    } else {
        ds_obj <- data
    }

    # --- Pull data for the requested variable (Arrow-safe) ---
    # Select columns depending on aggregation
    if (agg == "annual") {
        sel_cols <- c("geoid", "year", "variable", "value")
    } else {
        sel_cols <- c("geoid", "year", "month", "variable", "value")
    }

    df <- tryCatch(
        ds_obj %>%
            dplyr::select(dplyr::all_of(sel_cols)) %>%
            dplyr::filter(variable %in% var) %>%
            dplyr::collect() %>%
            dplyr::select(-variable),
        error = function(e) {
            # Fallback if Arrow chokes on the filter pushdown
            tmp <- ds_obj %>%
                dplyr::select(dplyr::all_of(sel_cols)) %>%
                dplyr::collect()
            tmp <- dplyr::filter(tmp, variable %in% var)
            dplyr::select(tmp, -variable)
        }
    )

    # Optional numeric transform
    if (!is.null(value_fun)) {
        df <- dplyr::mutate(df, value = value_fun(value))
    }

    # --- Frame key (robust to "normal" + character/NA) ---
    df <- dplyr::mutate(
        df,
        year_chr = as.character(year),
        year_int = suppressWarnings(as.integer(year))
    )
    if ("month" %in% names(df)) {
        df$month_int <- suppressWarnings(as.integer(df$month))
    } else {
        df$month_int <- NA_integer_
    }

    if (drop_na_time) {
        if (agg == "annual") {
            df <- dplyr::filter(df, !is.na(year_chr))
        } else {
            df <- dplyr::filter(df, !is.na(year_chr), !is.na(month_int))
        }
    }

    if (agg == "monthly") {
        df$frame_label <- ifelse(
            is.na(df$month_int),
            df$year_chr,
            paste0(df$year_chr, "-", sprintf("%02d", df$month_int))
        )
        df <- dplyr::arrange(
            df,
            ifelse(is.na(df$year_int), Inf, df$year_int),
            ifelse(is.na(df$month_int), 1L, df$month_int)
        )
    } else {
        df$frame_label <- df$year_chr
        df <- dplyr::arrange(df, ifelse(is.na(df$year_int), Inf, df$year_int))
    }

    levs <- unique(df$frame_label)
    df$time_state <- factor(df$frame_label, levels = levs)

    # --- Join to geometry; drop empties safely ---
    joined <- dplyr::left_join(geom, df, by = "geoid")
    if (!inherits(joined, "sf")) {
        joined <- sf::st_as_sf(joined)
    }
    joined <- joined[!sf::st_is_empty(joined), , drop = FALSE]
    plot_df <- sf::st_make_valid(joined)

    # --- View defaults ---
    if (is.null(bbox)) {
        bbox <- if (include_alaska) {
            c(-170, -60, 18, 72)
        } else {
            c(-125, -66, 24, 50)
        }
    }
    if (is.null(title)) {
        title <- sprintf(
            "%s %s %s — {current_frame}",
            utils::tail(strsplit(level, "")[[1]], 1L),
            agg,
            var
        )
        # Better title:
        title <- sprintf(
            "%s %s %s — {current_frame}",
            tools::toTitleCase(level),
            agg,
            var
        )
    }

    # --- Plot ---
    p <- ggplot2::ggplot(plot_df) +
        ggplot2::geom_sf(ggplot2::aes(fill = value), color = NA) +
        ggplot2::scale_fill_viridis_c(
            option = palette,
            direction = direction,
            trans = trans,
            labels = labels,
            na.value = na_fill,
            name = legend_title
        ) +
        ggplot2::coord_sf(
            xlim = c(bbox[1], bbox[2]),
            ylim = c(bbox[3], bbox[4]),
            expand = FALSE
        ) +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = title)

    if (tween_shapes) {
        p <- p +
            gganimate::transition_states(
                time_state,
                transition_length = 1,
                state_length = 1
            ) +
            gganimate::ease_aes("linear")
    } else {
        p <- p + gganimate::transition_manual(time_state)
    }

    nframes <- length(levels(plot_df$time_state))
    if (is.na(nframes) || nframes < 1) {
        nframes <- 1L
    }

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

    if (is.null(out_path)) {
        dir.create("figures", showWarnings = FALSE, recursive = TRUE)
        out_path <- file.path(
            "figures",
            sprintf("%s_%s_%s.gif", var, level, agg)
        )
    }
    gganimate::anim_save(out_path, anim)
    invisible(out_path)
}
