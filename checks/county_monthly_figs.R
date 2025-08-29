# oppten apptainer in bash first
# apptainer shell /ddn/gs1/group/set/chords/combining_datasets/container_combining_datasets.sif

.libPaths("/usr/local/lib/R/site-library")

get_project_dir <- function() {
    from_env <- Sys.getenv("PROJECT_DIR", unset = NA)
    if (!is.na(from_env) && nzchar(from_env)) {
        return(normalizePath(from_env, mustWork = FALSE))
    }
    in_file <- tryCatch(knitr::current_input(), error = function(e) NULL)
    if (!is.null(in_file) && nzchar(in_file)) {
        return(normalizePath(dirname(in_file), mustWork = FALSE))
    }
    normalizePath(getwd(), mustWork = FALSE)
}

project_dir <- get_project_dir()
knitr::opts_knit$set(root.dir = project_dir)

# Headless-safe plotting
knitr::opts_chunk$set(dev = "ragg_png", dpi = 150)

# Ensure figures dir exists
dir.create(
    file.path(project_dir, "figures"),
    showWarnings = FALSE,
    recursive = TRUE
)

# Keep ragg from interpreting huge inch sizes
options(ragg.max_dim = 10000)

# packages
library(arrow)
library(dplyr)
library(tidyverse)
library(sf)
library(gganimate)
library(cowplot)
library(ragg)
library(gifski)

# Resolve paths relative to root.dir set above
ds <- function(...) file.path(project_dir, ...)

county_monthly <- open_dataset(ds(
    "handoffs/county_monthly_long/county_monthly.parquet"
))

# ###########################################################################################
# ############## HMS smoke county monthly animated map ##############
#
# # --- Load county geometries (includes Alaska + Hawaii) ---
# counties_all <- sf::st_read(
#     ds("clean_data/county_census/canonical_2024.gpkg"),
#     layer = "counties_500k",
#     quiet = TRUE
# ) %>%
#     sf::st_make_valid()
#
# # --- Extract HMS smoke proportion variables (monthly) ---
# smoke_df <- county_monthly %>%
#     filter(
#         variable %in%
#             c(
#                 "prop_light_coverage",
#                 "prop_med_coverage",
#                 "prop_heavy_coverage"
#             )
#     ) %>%
#     select(geoid, year, month, variable, value) %>%
#     collect() %>%
#     mutate(
#         year = suppressWarnings(as.integer(year)),
#         month = suppressWarnings(as.integer(month))
#     )
#
# # --- Pivot to wide format for category + final_value ---
# smoke_wide <- smoke_df %>%
#     tidyr::pivot_wider(
#         names_from = variable,
#         values_from = value,
#         values_fill = 0
#     ) %>%
#     mutate(
#         category = case_when(
#             prop_heavy_coverage > 0 ~ "Heavy",
#             prop_med_coverage > 0 ~ "Medium",
#             prop_light_coverage > 0 ~ "Light",
#             TRUE ~ "None"
#         ),
#         final_value = case_when(
#             category == "Heavy" ~ prop_heavy_coverage,
#             category == "Medium" ~ prop_med_coverage,
#             category == "Light" ~ prop_light_coverage,
#             TRUE ~ 0
#         ),
#         year = as.integer(year),
#         month = as.integer(month),
#         ym = ifelse(
#             is.na(year) | is.na(month),
#             NA_character_,
#             sprintf("%04d-%02d", year, month)
#         )
#     )
#
# # --- Join geometries ---
# plot_df <- counties_all %>%
#     left_join(smoke_wide, by = "geoid") %>%
#     sf::st_as_sf() %>%
#     filter(!sf::st_is_empty(sf::st_geometry(.))) %>%
#     filter(substr(geoid, 1, 2) != "15") # Keep Alaska, drop Hawaii
#
# # --- Custom smoke colors ---
# smoke_colors <- c(
#     "Light" = "#eccc7c",
#     "Medium" = "#dc8b30",
#     "Heavy" = "#d96527",
#     "None" = "#dfdac4"
# )
#
# # --- Set category factor levels in desired order ---
# plot_df <- plot_df %>%
#     mutate(
#         category = factor(
#             category,
#             levels = c("None", "Light", "Medium", "Heavy")
#         )
#     )
#
# # --- Build animated plot (monthly) ---
# p_main <- ggplot(plot_df) +
#     geom_sf(aes(fill = category, alpha = final_value), color = NA) +
#     scale_fill_manual(
#         values = smoke_colors,
#         breaks = c("None", "Light", "Medium", "Heavy"),
#         na.value = "grey80"
#     ) +
#     scale_alpha(range = c(0.2, 1), guide = "none") +
#     coord_sf(
#         xlim = c(-170, -67),
#         ylim = c(20, 72),
#         expand = FALSE
#     ) +
#     theme_void() +
#     theme(
#         legend.position = "bottom",
#         legend.title = element_text(size = 10),
#         legend.text = element_text(size = 8),
#         plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
#     ) +
#     labs(
#         title = "County-Monthly HMS Smoke Coverage — {current_frame}",
#         fill = "Smoke Category"
#     ) +
#     gganimate::transition_manual(ym)
#
# # --- Count frames safely ---
# nframes <- plot_df$ym %>%
#     unique() %>%
#     sort(na.last = NA) %>%
#     length()
#
# # --- Animate using ragg ---
# anim <- gganimate::animate(
#     p_main,
#     nframes = max(1L, nframes),
#     fps = 4,
#     width = 1000,
#     height = 600,
#     units = "px",
#     renderer = gifski_renderer(),
#     device = "ragg_png"
# )
#
# # --- Save animation ---
# gganimate::anim_save(ds("figures/county_monthly_hms_smoke.gif"), anim)
#

###########################################################################################
# ---- Generic GIF animator for county × monthly ----

animate_geo_gif <- function(
    var,
    level = c("county", "tract"),
    agg = c("annual", "monthly"),
    include_alaska = TRUE,
    include_hawaii = FALSE,
    bbox = NULL, # c(xmin, xmax, ymin, ymax)
    legend_title = var,
    title = NULL, # defaults to "... — {current_frame}"
    palette = "mako",
    direction = -1,
    trans = "identity", # e.g., "log10"
    labels = scales::label_number(accuracy = 0.1),
    na_fill = "grey80",
    width = 1000,
    height = 600,
    fps = 4,
    out_path = NULL, # default: figures/<var>_<level>_<agg>.gif
    value_fun = NULL, # e.g., function(x) x * 1e9
    drop_na_time = TRUE,
    tween_shapes = FALSE
) {
    level <- match.arg(level)
    agg <- match.arg(agg)

    # --- Geometry ---
    geom_layer <- if (level == "county") "counties_500k" else "tracts_500k"
    geom <- sf::st_read(
        ds("clean_data/county_census/canonical_2024.gpkg"),
        layer = geom_layer,
        quiet = TRUE
    ) |>
        sf::st_make_valid() |>
        sf::st_zm(drop = TRUE)

    drop_states <- c(
        if (!include_alaska) "02" else NULL,
        if (!include_hawaii) "15" else NULL
    )
    if (length(drop_states)) {
        geom <- dplyr::filter(geom, !substr(geoid, 1, 2) %in% drop_states)
    }

    # --- Data source (Arrow/tibble) ---
    ds_obj <- get(sprintf("%s_%s", level, agg), inherits = TRUE)

    df <- tryCatch(
        ds_obj |>
            dplyr::filter(variable %in% !!var) |>
            dplyr::select(geoid, year, month, value) |>
            dplyr::collect(),
        error = function(e) {
            ds_obj |>
                dplyr::select(geoid, year, month, variable, value) |>
                dplyr::collect() |>
                dplyr::filter(variable == var) |>
                dplyr::select(-variable)
        }
    )

    if (!is.null(value_fun)) {
        df <- dplyr::mutate(df, value = value_fun(value))
    }

    # --- Frame label ---
    df <- df |>
        dplyr::mutate(
            year = suppressWarnings(as.integer(year)),
            month = suppressWarnings(as.integer(month))
        )

    if (drop_na_time) {
        if (agg == "monthly") {
            df <- dplyr::filter(df, !is.na(year), !is.na(month))
        }
        if (agg == "annual") df <- dplyr::filter(df, !is.na(year))
    }

    df <- df |>
        dplyr::mutate(
            frame_label = dplyr::case_when(
                agg == "monthly" ~ sprintf("%04d-%02d", year, month),
                TRUE ~ sprintf("%04d", year)
            )
        )

    levs <- df |>
        dplyr::arrange(year, dplyr::if_else(is.na(month), 1L, month)) |>
        dplyr::distinct(frame_label) |>
        dplyr::pull(frame_label)

    df <- dplyr::mutate(df, time_state = factor(frame_label, levels = levs))

    # --- Join back to geometry; ensure sf, then drop empties via base subsetting ---
    tmp <- dplyr::left_join(geom, df, by = "geoid")
    if (!inherits(tmp, "sf")) {
        tmp <- sf::st_as_sf(tmp)
    } # keep whatever the sf column name is
    nonempty <- !sf::st_is_empty(sf::st_geometry(tmp))
    plot_df <- tmp[nonempty, , drop = FALSE]
    plot_df <- sf::st_make_valid(plot_df)

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
            tools::toTitleCase(level),
            agg,
            var
        )
    }

    # --- Plot ---
    p <- ggplot(plot_df) +
        geom_sf(aes(fill = value), color = NA) +
        scale_fill_viridis_c(
            option = palette,
            direction = direction,
            trans = trans,
            labels = labels,
            na.value = na_fill,
            name = legend_title
        ) +
        coord_sf(
            xlim = c(bbox[1], bbox[2]),
            ylim = c(bbox[3], bbox[4]),
            expand = FALSE
        ) +
        theme_minimal() +
        labs(title = title)

    if (tween_shapes) {
        p <- p +
            gganimate::transition_states(
                time_state,
                transition_length = 1,
                state_length = 1
            ) +
            gganimate::ease_aes("linear")
    } else {
        p <- p + gganimate::transition_manual(time_state) # {current_frame}
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
        renderer = gifski_renderer(),
        device = "ragg_png"
    )

    if (is.null(out_path)) {
        out_path <- ds(sprintf("figures/%s_%s_%s.gif", var, level, agg))
    }
    gganimate::anim_save(out_path, anim)
    invisible(out_path)
}


###########################################################################################
# Examples — county/monthly

# # merra dusmass25 (kg/m^3 -> µg/m^3)
# # merra2 dusmass25 (kg/m^3 -> µg/m^3)
# animate_geo_gif(
#     var = "dusmass25",
#     level = "county",
#     agg = "monthly",
#     include_alaska = TRUE,
#     include_hawaii = FALSE,
#     bbox = c(-170, -60, 18, 72),
#     legend_title = expression("Dust (µg·m"^-3 * ")"),
#     palette = "magma",
#     direction = 1,
#     trans = "log10",
#     labels = scales::label_number(accuracy = 0.1),
#     value_fun = function(x) x * 1e9,
#     fps = 4,
#     out_path = ds("figures/county_monthly_dusmass.gif"),
#     title = "County monthly merra2 dusmass25 — {current_frame}"
# )
#
#
# # gridmet rmax
# animate_geo_gif(
#     var = "rmax",
#     level = "county",
#     agg = "monthly",
#     include_alaska = FALSE,
#     include_hawaii = FALSE,
#     bbox = c(-125, -66, 24, 50),
#     legend_title = "Rmax",
#     palette = "plasma",
#     direction = 1,
#     out_path = ds("figures/county_monthly_rmax.gif"),
#     title = "County monthly gridmet rmax — {current_frame}"
# )

# just remove bbox entirely
animate_geo_gif(
    var = "tmin",
    level = "county",
    agg = "monthly",
    include_alaska = TRUE,
    include_hawaii = FALSE,
    legend_title = expression(T[min] * " (°C)"),
    palette = "turbo",
    direction = 1,
    out_path = ds("figures/county_monthly_tmin.gif"),
    title = "County monthly terraclimate tmin — {current_frame}"
)

animate_geo_gif(
    var = "annual_total_air_lb_per_km2",
    level = "county",
    agg = "monthly",
    include_alaska = TRUE,
    include_hawaii = FALSE,
    legend_title = expression("TRI air emissions per area (lb·km"^-2 * ")"),
    palette = "rocket",
    direction = -1,
    out_path = ds("figures/county_monthly_total_air_lb_per_km2.gif"),
    title = "County monthly TRI air emissions — {current_frame}"
)

#
# # MODIS EVI (already scaled)
# animate_geo_gif(
#     var = "evi",
#     level = "county",
#     agg = "monthly",
#     include_alaska = FALSE,
#     include_hawaii = FALSE,
#     bbox = c(-125, -66, 24, 50),
#     legend_title = "EVI",
#     palette = "viridis",
#     direction = 1,
#     out_path = ds("figures/county_monthly_evi.gif"),
#     title = "County monthly MODIS EVI — {current_frame}"
# )
#
