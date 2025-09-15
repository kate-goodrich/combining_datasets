# Requires: dplyr, sf, ggplot2, gganimate, scales, viridis, magrittr (%>%)

animate_geo_gif <- function(
    var,
    level = c("county", "tract", "zip"),
    agg = c("annual", "monthly"),
    data = NULL, # optional Arrow Dataset/tibble; otherwise falls back to get("<level>_<agg>")
    geoms_gpkg = "clean_data/county_census_zip/canonical_2024.gpkg",
    geoms_layers = list(
        county = "counties_500k",
        tract = "tracts_500k",
        zip = "zctas_500k" # ZCTAs for ZIP
    ),
    include_alaska = TRUE,
    include_hawaii = FALSE,
    bbox = NULL, # numeric vector c(xmin, xmax, ymin, ymax) in bbox_crs
    bbox_crs = 4326, # CRS of bbox; default lon/lat
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
    tween_shapes = FALSE,
    scale_limits = NULL, # c(min,max) for consistent color across runs
    verbose = FALSE
) {
    level <- match.arg(level)
    agg <- match.arg(agg)

    # ---- helpers ----
    pad_zip5 <- function(x) {
        x <- gsub("\\D", "", as.character(x))
        n <- nchar(x)
        if (length(n) && any(n < 5)) {
            x[n < 5] <- paste0(strrep("0", 5 - n[n < 5]), x[n < 5])
        }
        x
    }
    msg <- function(...) if (isTRUE(verbose)) message(...)

    # ---- Geometry (stay in native CRS, e.g., EPSG:5070 for ZIPs) ----
    geom_layer <- geoms_layers[[level]]
    if (is.null(geom_layer)) {
        stop(
            "No geometry layer configured for level='",
            level,
            "'. Provide via `geoms_layers`."
        )
    }

    geom <- sf::st_read(
        geoms_gpkg,
        layer = geom_layer,
        quiet = !isTRUE(verbose)
    )
    geom <- sf::st_make_valid(geom)
    geom <- sf::st_zm(geom, drop = TRUE)

    crs_geom <- sf::st_crs(geom)
    if (is.na(crs_geom)) {
        stop(
            "Geometry layer '",
            geom_layer,
            "' has no CRS. Please assign one in the GPKG."
        )
    }
    msg(
        "Geometry CRS: ",
        if (!is.null(crs_geom$input)) crs_geom$input else crs_geom$epsg
    )

    if (!"geoid" %in% names(geom)) {
        stop("Geometry must have a 'geoid' column.")
    }
    geom$geoid <- as.character(geom$geoid)
    if (level == "zip") {
        geom$geoid <- pad_zip5(geom$geoid)
    }

    # Optionally drop AK/HI for county/tract via FIPS prefix; for ZIP we generally use bbox instead
    if (level %in% c("county", "tract")) {
        drop_states <- c(
            if (!include_alaska) "02" else NULL,
            if (!include_hawaii) "15" else NULL
        )
        if (length(drop_states)) {
            geom <- dplyr::filter(geom, !substr(geoid, 1, 2) %in% drop_states)
        }
    }

    # ---- Data source ----
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

    # ---- Pull data for the requested variable (Arrow-safe) ----
    sel_cols <- if (agg == "annual") {
        c("geoid", "year", "variable", "value")
    } else {
        c("geoid", "year", "month", "variable", "value")
    }

    df <- tryCatch(
        ds_obj %>%
            dplyr::select(dplyr::all_of(sel_cols)) %>%
            dplyr::filter(variable %in% var) %>%
            dplyr::collect() %>%
            dplyr::select(-variable),
        error = function(e) {
            tmp <- ds_obj %>%
                dplyr::select(dplyr::all_of(sel_cols)) %>%
                dplyr::collect()
            tmp <- dplyr::filter(tmp, variable %in% var)
            dplyr::select(tmp, -variable)
        }
    )

    if (nrow(df) == 0) {
        stop(
            "No rows found for variable '",
            var,
            "' at level=",
            level,
            " agg=",
            agg,
            "."
        )
    }

    # Normalize IDs
    if (!"geoid" %in% names(df)) {
        stop("Data must have a 'geoid' column.")
    }
    df$geoid <- as.character(df$geoid)
    if (level == "zip") {
        df$geoid <- pad_zip5(df$geoid)
    }

    # Optional numeric transform
    if (!is.null(value_fun)) {
        df <- dplyr::mutate(df, value = value_fun(value))
    }

    # ---- Time fields + frame labels (robust to NA) ----
    df <- dplyr::mutate(
        df,
        year_chr = as.character(year),
        year_int = suppressWarnings(as.integer(year)),
        month_int = if ("month" %in% names(df)) {
            suppressWarnings(as.integer(month))
        } else {
            NA_integer_
        }
    )

    if (agg == "monthly") {
        df <- dplyr::mutate(
            df,
            frame_label = ifelse(
                is.na(year_chr) | is.na(month_int),
                NA_character_,
                paste0(year_chr, "-", sprintf("%02d", month_int))
            )
        )
        df <- dplyr::arrange(
            df,
            ifelse(is.na(year_int), Inf, year_int),
            ifelse(is.na(month_int), 1L, month_int)
        )
    } else {
        df <- dplyr::mutate(
            df,
            frame_label = ifelse(is.na(year_chr), NA_character_, year_chr)
        )
        df <- dplyr::arrange(df, ifelse(is.na(year_int), Inf, year_int))
    }

    if (isTRUE(drop_na_time)) {
        df <- dplyr::filter(df, !is.na(frame_label))
    }
    if (nrow(df) == 0) {
        stop(
            "No valid frames (all missing time fields) for variable '",
            var,
            "'."
        )
    }

    if (all(is.na(df$value))) {
        stop("All 'value' are NA for variable '", var, "'.")
    }

    levs <- unique(df$frame_label)
    if (length(levs) == 0) {
        stop("No valid frames for variable '", var, "'.")
    }
    df$time_state <- factor(df$frame_label, levels = levs)

    # ---- Join to geometry; drop empties + rows without frames ----
    joined <- dplyr::left_join(geom, df, by = "geoid")
    if (!inherits(joined, "sf")) {
        joined <- sf::st_as_sf(joined, crs = crs_geom)
    }
    joined <- joined[!sf::st_is_empty(joined), , drop = FALSE]
    plot_df <- sf::st_make_valid(joined)
    plot_df <- dplyr::filter(plot_df, !is.na(time_state))

    if (nrow(plot_df) == 0) {
        stop(
            "No geometries matched data for '",
            var,
            "'. Check GEOID formats and filters."
        )
    }

    # ---- View defaults / spatial cropping in GEOMETRY CRS (e.g., EPSG:5070) ----
    # If bbox is NULL, define a sensible default in lon/lat (4326), then transform to geom CRS
    if (is.null(bbox)) {
        # Default CONUS vs. AK-inclusive boxes provided in lon/lat
        bbox <- if (isTRUE(include_alaska)) {
            c(-170, -60, 18, 72)
        } else {
            c(-125, -66, 24, 50)
        }
        bbox_crs <- 4326
    }

    # Build bbox in its declared CRS, then transform to the geometry CRS for cropping
    bbox_sfc_input <- sf::st_as_sfc(
        sf::st_bbox(
            c(xmin = bbox[1], xmax = bbox[2], ymin = bbox[3], ymax = bbox[4]),
            crs = bbox_crs
        )
    )
    if (sf::st_crs(bbox_sfc_input) != crs_geom) {
        bbox_sfc <- sf::st_transform(bbox_sfc_input, crs_geom)
    } else {
        bbox_sfc <- bbox_sfc_input
    }

    # For ZIPs (and as a safety), apply bbox cropping
    if (level == "zip" || !include_alaska || !include_hawaii) {
        plot_df <- suppressWarnings(sf::st_crop(plot_df, bbox_sfc))
        if (nrow(plot_df) == 0) {
            stop(
                "All geometries were cropped out by the provided bbox (reprojected to geometry CRS)."
            )
        }
    }

    # ---- Titles ----
    if (is.null(title)) {
        title <- sprintf(
            "%s %s %s — {current_frame}",
            tools::toTitleCase(level),
            agg,
            var
        )
    }

    # ---- Plot ----
    sc <- ggplot2::scale_fill_viridis_c(
        option = palette,
        direction = direction,
        trans = trans,
        labels = labels,
        na.value = na_fill,
        name = legend_title,
        limits = scale_limits
    )

    p <- ggplot2::ggplot(plot_df) +
        ggplot2::geom_sf(ggplot2::aes(fill = value), color = NA) +
        sc +
        ggplot2::coord_sf(expand = FALSE) + # in geometry CRS
        ggplot2::theme_minimal() +
        ggplot2::labs(title = title)

    # ---- Animation ----
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

    nframes <- length(levs)
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
