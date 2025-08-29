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
options(ragg.max_dim = 10000) # optional; with units="px" you shouldn't hit the cap


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

county_annual <- open_dataset(ds(
    "handoffs/county_annual_long/county_annual.parquet"
))

###########################################################################################

# ############## HMS smoke county annual animated map ##############
#
# # --- Load county geometries (includes Alaska + Hawaii) ---
# counties_all <- sf::st_read(
#     ds("clean_data/county_census/canonical_2024.gpkg"),
#     layer = "counties_500k",
#     quiet = TRUE
# ) %>%
#     sf::st_make_valid()
#
# # --- Extract HMS smoke proportion variables ---
# smoke_df <- county_annual %>%
#     filter(
#         variable %in%
#             c(
#                 "prop_light_coverage",
#                 "prop_med_coverage",
#                 "prop_heavy_coverage"
#             )
#     ) %>%
#     select(geoid, year, variable, value) %>%
#     collect()
#
# # --- Pivot to wide format for priority calculation ---
# smoke_wide <- smoke_df %>%
#     tidyr::pivot_wider(
#         names_from = variable,
#         values_from = value,
#         values_fill = 0
#     ) %>%
#     mutate(
#         category = dplyr::case_when(
#             prop_heavy_coverage > 0 ~ "Heavy",
#             prop_med_coverage > 0 ~ "Medium",
#             prop_light_coverage > 0 ~ "Light",
#             TRUE ~ "None"
#         ),
#         final_value = dplyr::case_when(
#             category == "Heavy" ~ prop_heavy_coverage,
#             category == "Medium" ~ prop_med_coverage,
#             category == "Light" ~ prop_light_coverage,
#             TRUE ~ 0
#         )
#     )
#
# # --- Join geometries ---
# plot_df <- counties_all %>%
#     left_join(smoke_wide, by = "geoid") %>%
#     sf::st_as_sf() %>%
#     filter(!sf::st_is_empty(sf::st_geometry(.)))
#
# # --- Remove Hawaii ONLY ---
# plot_df <- plot_df %>%
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
# # --- Build animated plot ---
# p_main <- ggplot(plot_df) +
#     geom_sf(aes(fill = category, alpha = final_value), color = NA) +
#     scale_fill_manual(
#         values = smoke_colors,
#         breaks = c("None", "Light", "Medium", "Heavy"), # <- Ensures correct legend order
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
#         title = "County-Annual HMS Smoke Coverage — Year: {current_frame}",
#         fill = "Smoke Category"
#     ) +
#     gganimate::transition_manual(year)
#
#
# # --- Count frames safely ---
# nframes <- plot_df$year %>%
#     unique() %>%
#     sort(na.last = NA) %>%
#     length()
#
# # --- Animate using ragg ---
# anim <- gganimate::animate(
#     p_main,
#     nframes = max(1L, nframes),
#     fps = 2,
#     width = 1000,
#     height = 600,
#     units = "px",
#     renderer = gifski_renderer(),
#     device = "ragg_png"
# )
#
# # --- Save animation ---
# gganimate::anim_save(ds("figures/county_annual_hms_smoke.gif"), anim)
#

# ---- Generic GIF animator for county/tract × annual/monthly ----
animate_geo_gif <- function(
    var,
    level = c("county", "tract"),
    agg = c("annual", "monthly"),
    include_alaska = TRUE,
    include_hawaii = FALSE,
    bbox = NULL, # c(xmin, xmax, ymin, ymax)
    legend_title = var,
    title = NULL,
    palette = "mako",
    direction = -1,
    trans = "identity", # e.g., "log10"
    labels = scales::label_number(accuracy = 0.1),
    na_fill = "grey80",
    width = 1000,
    height = 600,
    fps = 2,
    out_path = NULL, # default: figures/<var>_<level>_<agg>.gif
    value_fun = NULL, # e.g., function(x) x * 1e9
    drop_na_time = TRUE,
    tween_shapes = FALSE # <- NEW: avoid polygon morphing by default
) {
    level <- match.arg(level)
    agg <- match.arg(agg)

    geom_layer <- if (level == "county") "counties_500k" else "tracts_500k"
    geom <- sf::st_read(
        ds("clean_data/county_census/canonical_2024.gpkg"),
        layer = geom_layer,
        quiet = TRUE
    ) %>%
        sf::st_make_valid() %>%
        sf::st_zm(drop = TRUE)

    drop_states <- c(
        if (!include_alaska) "02" else NULL,
        if (!include_hawaii) "15" else NULL
    )
    if (length(drop_states)) {
        geom <- dplyr::filter(geom, !substr(geoid, 1, 2) %in% drop_states)
    }

    ds_obj <- get(sprintf("%s_%s", level, agg), inherits = TRUE)

    df <- tryCatch(
        ds_obj %>%
            dplyr::filter(variable %in% !!var) %>%
            {
                if (agg == "annual") {
                    dplyr::select(., geoid, year, value)
                } else {
                    dplyr::select(., geoid, year, month, value)
                }
            } %>%
            dplyr::collect(),
        error = function(e) {
            ds_obj %>%
                dplyr::select(
                    geoid,
                    year,
                    dplyr::all_of(
                        if (agg == "monthly") c("month") else character(0)
                    ),
                    variable,
                    value
                ) %>%
                dplyr::collect() %>%
                dplyr::filter(variable == var) %>%
                dplyr::select(-variable)
        }
    )

    if (!is.null(value_fun)) {
        df <- dplyr::mutate(df, value = value_fun(value))
    }

    if (agg == "annual") {
        if (drop_na_time) {
            df <- dplyr::filter(df, !is.na(year))
        }
        levs <- df %>%
            dplyr::arrange(year) %>%
            dplyr::distinct(year) %>%
            dplyr::pull(year)
        df <- df %>%
            dplyr::mutate(
                time_state = factor(
                    as.character(year),
                    levels = as.character(levs)
                )
            )
    } else {
        if (drop_na_time) {
            df <- dplyr::filter(df, !is.na(year), !is.na(month))
        }
        df <- df %>% dplyr::mutate(ym = sprintf("%04d-%02d", year, month))
        levs <- df %>%
            dplyr::arrange(year, month) %>%
            dplyr::distinct(ym) %>%
            dplyr::pull(ym)
        df <- df %>% dplyr::mutate(time_state = factor(ym, levels = levs))
    }

    plot_df <- dplyr::left_join(geom, df, by = "geoid") %>%
        dplyr::filter(!is.na(time_state)) %>%
        dplyr::filter(!sf::st_is_empty(sf::st_geometry(.))) %>%
        sf::st_make_valid()

    if (is.null(bbox)) {
        bbox <- c(-125, -66, 24, 50)
    }

    if (is.null(title)) {
        title <- sprintf(
            "%s-%s %s — {closest_state}",
            tools::toTitleCase(level),
            agg,
            var
        )
    }

    nframes <- plot_df %>% dplyr::distinct(time_state) %>% nrow()
    if (is.na(nframes) || nframes < 1) {
        nframes <- 1L
    }

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

    # Use manual transition by default (no polygon morphing)
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

#
# # merra dusmass25 county annual animated map (with AK)
#
# animate_geo_gif(
#     var = "dusmass25",
#     level = "county",
#     agg = "annual",
#     include_alaska = TRUE,
#     include_hawaii = FALSE,
#     bbox = c(-170, -60, 18, 72),
#     legend_title = expression("Dust (µg·m"^-3 * ")"),
#     palette = "magma",
#     direction = 1,
#     trans = "log10",
#     labels = scales::label_number(accuracy = 0.1),
#     value_fun = function(x) x * 1e9, # kg/m^3 -> µg/m^3
#     out_path = ds("figures/county_annual_dusmass.gif"),
#     title = "County annual merra2 dusmass25 — Year: {current_frame}"
# )
#
#
# # gridmet rmax county annual
#
# animate_geo_gif(
#     var = "rmax",
#     level = "county",
#     agg = "annual",
#     include_alaska = FALSE,
#     include_hawaii = FALSE,
#     bbox = c(-125, -66, 24, 50),
#     legend_title = "Rmax",
#     palette = "plasma",
#     direction = 1,
#     out_path = ds("figures/county_annual_rmax.gif"),
#     title = "County annual gridmet rmax — Year: {current_frame}"
# )
#

# terra tmin

animate_geo_gif(
    var = "tmin",
    level = "county",
    agg = "annual",
    include_alaska = TRUE,
    include_hawaii = FALSE,
    bbox = c(-170, -60, 18, 72),
    legend_title = expression(T[min] * " (°C)"),
    palette = "turbo",
    direction = 1,
    out_path = ds("figures/county_annual_tmin.gif"),
    title = "County annual terraclimate tmin — Year: {current_frame}"
)

# tri

animate_geo_gif(
    var = "annual_total_air_lb_per_km2",
    level = "county",
    agg = "annual",
    include_alaska = TRUE,
    include_hawaii = FALSE,
    bbox = c(-170, -60, 18, 72),
    legend_title = expression("TRI air emissions per area (lb·km"^-2 * ")"),
    palette = "rocket",
    direction = -1,
    out_path = ds("figures/county_annual_total_air_lb_per_km2.gif"),
    title = "County annual tri total_air_lb_per_km2 — Year: {current_frame}"
)

#
# # MODIS EVI — county/annual, CONUS only (values already scaled to [-1, 1], so no conversion)
# animate_geo_gif(
#     var = "evi",
#     level = "county",
#     agg = "annual",
#     include_alaska = FALSE,
#     include_hawaii = FALSE,
#     bbox = c(-125, -66, 24, 50),
#     legend_title = "EVI",
#     palette = "viridis",
#     direction = 1,
#     out_path = ds("figures/county_annual_evi.gif"),
#     title = "County annual MODIS EVI — Year: {current_frame}"
# )
#
#
# # nlcd plot
#
# # --- Pick year and 3 NLCD categories ---
# target_year <- 2021
# vars <- c(
#     "land_cover_24", # Developed, High Intensity
#     "land_cover_42", # Evergreen Forest
#     "land_cover_82"
# ) # Cultivated Crops
#
# pretty_names <- c(
#     land_cover_24 = "Developed, High Intensity",
#     land_cover_42 = "Evergreen Forest",
#     land_cover_82 = "Cultivated Crops"
# )
#
# # One distinct high color per category (0→white, 1→color)
# high_colors <- c(
#     land_cover_24 = "#5e503f", # red
#     land_cover_42 = "#606c38", # green
#     land_cover_82 = "#dda15e" # orange
# )
#
# # --- Load CONUS counties (drop AK + HI) ---
# counties_conus <- sf::st_read(
#     ds("clean_data/county_census/canonical_2024.gpkg"),
#     layer = "counties_500k",
#     quiet = TRUE
# ) |>
#     sf::st_make_valid() |>
#     dplyr::filter(!substr(geoid, 1, 2) %in% c("02", "15"))
#
# # --- Pull NLCD proportions for the chosen year ---
# nlcd_df <- county_annual |>
#     dplyr::filter(variable %in% vars, year == target_year) |>
#     dplyr::select(geoid, variable, value) |>
#     dplyr::collect() |>
#     tidyr::pivot_wider(names_from = variable, values_from = value)
#
# plot_df <- dplyr::left_join(counties_conus, nlcd_df, by = "geoid")
#
# # --- Helper to build one map for a column ---
# map_one <- function(col_name) {
#     ggplot(plot_df) +
#         geom_sf(aes(fill = .data[[col_name]]), color = NA) +
#         scale_fill_gradient(
#             low = "white",
#             high = high_colors[[col_name]],
#             limits = c(0, 1),
#             oob = scales::squish,
#             name = "Proportion",
#             na.value = "grey90",
#             breaks = c(0, 0.25, 0.5, 0.75, 1)
#         ) +
#         coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
#         labs(
#             title = paste0(pretty_names[[col_name]], " — ", target_year)
#         ) +
#         theme_minimal() +
#         theme(
#             plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
#             legend.position = "bottom",
#             legend.title = element_text(size = 9),
#             legend.text = element_text(size = 8)
#         )
# }
#
# p1 <- map_one(vars[1])
# p2 <- map_one(vars[2])
# p3 <- map_one(vars[3])
#
# panel <- cowplot::plot_grid(p1, p2, p3, ncol = 3)
#
# # --- Save the panel PNG (headless-safe via ragg) ---
# outfile <- ds("figures/county_annual_nlcd.png")
# ggsave(
#     outfile,
#     panel,
#     device = ragg::agg_png,
#     width = 15,
#     height = 6,
#     units = "in",
#     dpi = 150
# )
# message("Saved: ", normalizePath(outfile))
#
# #Koppen
#
# # --- Pick year and 3 Köppen categories ---
# target_year <- "static"
# vars <- c(
#     "koppen_14", # Cfa, Temperate, no dry season, hot summer
#     "koppen_7", # BSk, Arid, steppe, cold
#     "koppen_26" # Dfb, Cold, no dry season, warm summer
# )
#
# pretty_names <- c(
#     koppen_14 = "Cfa — Temperate, no dry season, hot summer",
#     koppen_7 = "BSk — Arid, steppe, cold",
#     koppen_26 = "Dfb — Cold, no dry season, warm summer"
# )
#
# high_colors <- c(
#     koppen_14 = "#254e2c",
#     koppen_7 = "#8da4ac",
#     koppen_26 = "#edcc6f"
# )
#
# # --- Load counties (include AK, drop HI only) ---
# counties_ak_conus <- sf::st_read(
#     ds("clean_data/county_census/canonical_2024.gpkg"),
#     layer = "counties_500k",
#     quiet = TRUE
# ) |>
#     sf::st_make_valid() |>
#     dplyr::filter(substr(geoid, 1, 2) != "15") # keep AK ('02'), drop HI ('15')
#
# # --- Wider bbox to show AK + CONUS ---
# bbox <- c(-170, -60, 18, 72) # xmin, xmax, ymin, ymax
#
# # --- Pull Köppen proportions for the chosen year ---
# koppen_df <- county_annual |>
#     dplyr::filter(variable %in% vars, year == target_year) |>
#     dplyr::select(geoid, variable, value) |>
#     dplyr::collect() |>
#     tidyr::pivot_wider(names_from = variable, values_from = value)
#
# plot_df <- dplyr::left_join(counties_ak_conus, koppen_df, by = "geoid")
#
# # --- Helper to build one map for a column ---
# map_one <- function(col_name) {
#     ggplot(plot_df) +
#         geom_sf(aes(fill = .data[[col_name]]), color = NA) +
#         scale_fill_gradient(
#             low = "white",
#             high = high_colors[[col_name]],
#             limits = c(0, 1),
#             oob = scales::squish,
#             name = "Proportion",
#             na.value = "grey90",
#             breaks = c(0, 0.25, 0.5, 0.75, 1)
#         ) +
#         coord_sf(xlim = bbox[1:2], ylim = bbox[3:4], expand = FALSE) +
#         labs(title = paste0(pretty_names[[col_name]], " — ", target_year)) +
#         theme_minimal() +
#         theme(
#             plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
#             legend.position = "bottom",
#             legend.title = element_text(size = 9),
#             legend.text = element_text(size = 8)
#         )
# }
#
# p1 <- map_one(vars[1])
# p2 <- map_one(vars[2])
# p3 <- map_one(vars[3])
# panel <- cowplot::plot_grid(p1, p2, p3, ncol = 3)
#
# # --- Save PNG ---
# outfile <- ds("figures/county_annual_koppen.png")
# ggsave(
#     outfile,
#     panel,
#     device = ragg::agg_png,
#     width = 15,
#     height = 6,
#     units = "in",
#     dpi = 150
# )
# message("Saved: ", normalizePath(outfile))
#
#
# # ---- Generic static map function (Arrow-safe, standardized filenames) ----
# plot_static_map <- function(
#     var, # string or character vector
#     level = c("county", "tract"),
#     include_alaska = TRUE,
#     include_hawaii = FALSE,
#     legend_title = if (length(var) == 1) var else "Value",
#     palette = "viridis",
#     direction = 1,
#     bbox = c(-170, -60, 18, 72), # AK + CONUS by default
#     out_file = NULL,
#     title = NULL
# ) {
#     level <- match.arg(level)
#     geom_layer <- if (level == "county") "counties_500k" else "tracts_500k"
#
#     # --- Read zones ---
#     geom <- sf::st_read(
#         ds("clean_data/county_census/canonical_2024.gpkg"),
#         layer = geom_layer,
#         quiet = TRUE
#     ) |>
#         sf::st_make_valid() |>
#         sf::st_zm(drop = TRUE)
#
#     # Drop states if requested
#     drop_states <- c(
#         if (!include_alaska) "02" else NULL,
#         if (!include_hawaii) "15" else NULL
#     )
#     if (length(drop_states)) {
#         geom <- dplyr::filter(geom, !substr(geoid, 1, 2) %in% drop_states)
#     }
#
#     # --- Arrow-safe: collect before filtering on var ---
#     df <- county_annual |>
#         dplyr::select(geoid, year, variable, value) |>
#         dplyr::filter(year == "static") |>
#         dplyr::collect() |>
#         dplyr::filter(variable %in% var) |>
#         dplyr::select(geoid, variable, value)
#
#     if (nrow(df) == 0) {
#         stop(sprintf(
#             "No rows found for variable(s): %s",
#             paste(var, collapse = ", ")
#         ))
#     }
#
#     # --- Join and filter empties ---
#     plot_df <- dplyr::left_join(geom, df, by = "geoid")
#     keep <- !sf::st_is_empty(sf::st_geometry(plot_df))
#     if (!all(keep)) {
#         plot_df <- plot_df[keep, , drop = FALSE]
#     }
#     plot_df <- sf::st_make_valid(plot_df)
#
#     if (is.null(title)) {
#         title <- if (length(var) == 1) {
#             sprintf("%s (%s)", var, level)
#         } else {
#             sprintf("Static variables (%s)", level)
#         }
#     }
#
#     p <- ggplot(plot_df) +
#         geom_sf(aes(fill = value), color = NA) +
#         scale_fill_viridis_c(
#             option = palette,
#             direction = direction,
#             name = legend_title,
#             na.value = "grey90"
#         ) +
#         coord_sf(xlim = bbox[1:2], ylim = bbox[3:4], expand = FALSE) +
#         labs(title = title) +
#         theme_minimal() +
#         theme(
#             plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
#             legend.position = "bottom",
#             legend.title = element_text(size = 9),
#             legend.text = element_text(size = 8)
#         )
#
#     if (length(var) > 1) {
#         p <- p + facet_wrap(~variable, ncol = min(length(var), 3))
#     }
#
#     # --- Standardized file naming ---
#     if (is.null(out_file)) {
#         if (length(var) == 1) {
#             out_file <- ds(sprintf("figures/%s_static_%s.png", level, var))
#         } else {
#             out_file <- ds(sprintf("figures/%s_static_panel.png", level))
#         }
#     }
#
#     ggsave(
#         out_file,
#         p,
#         device = ragg::agg_png,
#         width = 12,
#         height = 6,
#         units = "in",
#         dpi = 150
#     )
#     message("Saved: ", normalizePath(out_file))
#     invisible(out_file)
# }
#
#
# # GMTED mean elevation
# plot_static_map(
#     var = "mn30_grd",
#     level = "county",
#     include_alaska = TRUE,
#     include_hawaii = FALSE,
#     legend_title = "Elevation (m)",
#     palette = "turbo",
#     direction = -1,
#     title = "County static GMTED Mean Elevation"
# )
#
# # Road density
# plot_static_map(
#     var = "road_density_km_per_km2",
#     level = "county",
#     include_alaska = TRUE,
#     include_hawaii = FALSE,
#     legend_title = "Road Density (km/km²)",
#     palette = "mako",
#     direction = -1,
#     title = "County static Road Density"
# )
#
#
# plot_static_map(
#     var = "prop_cover_nhdarea",
#     level = "county",
#     include_alaska = TRUE,
#     include_hawaii = FALSE,
#     legend_title = "Proportion NHD Area",
#     palette = "cividis",
#     title = "County static NHD Area"
# )
#
#
# # ---- Generic normal map function ----
# plot_normal_map <- function(
#     var,
#     level = c("county", "tract"),
#     include_alaska = TRUE,
#     include_hawaii = FALSE,
#     legend_title = var,
#     palette = "viridis",
#     direction = 1,
#     bbox = c(-140, -60, 18, 72),
#     out_file = NULL,
#     title = NULL
# ) {
#     level <- match.arg(level)
#     geom_layer <- if (level == "county") "counties_500k" else "tracts_500k"
#
#     geom <- sf::st_read(
#         ds("clean_data/county_census/canonical_2024.gpkg"),
#         layer = geom_layer,
#         quiet = TRUE
#     ) |>
#         sf::st_make_valid() |>
#         sf::st_zm(drop = TRUE)
#
#     drop_states <- c(
#         if (!include_alaska) "02" else NULL,
#         if (!include_hawaii) "15" else NULL
#     )
#     if (length(drop_states)) {
#         geom <- dplyr::filter(geom, !substr(geoid, 1, 2) %in% drop_states)
#     }
#
#     df <- county_annual |>
#         dplyr::select(geoid, year, variable, value) |>
#         dplyr::filter(year == "normal") |>
#         dplyr::collect() |>
#         dplyr::filter(variable == var) |>
#         dplyr::select(geoid, value)
#
#     if (nrow(df) == 0) {
#         stop(sprintf("No rows found for normal variable: %s", var))
#     }
#
#     plot_df <- dplyr::left_join(geom, df, by = "geoid")
#     keep <- !sf::st_is_empty(sf::st_geometry(plot_df))
#     if (!all(keep)) {
#         plot_df <- plot_df[keep, , drop = FALSE]
#     }
#     plot_df <- sf::st_make_valid(plot_df)
#
#     if (is.null(title)) {
#         title <- sprintf("%s normal (%s)", var, level)
#     }
#
#     p <- ggplot(plot_df) +
#         geom_sf(aes(fill = value), color = NA) +
#         scale_fill_viridis_c(
#             option = palette,
#             direction = direction,
#             name = legend_title,
#             na.value = "grey90"
#         ) +
#         coord_sf(xlim = bbox[1:2], ylim = bbox[3:4], expand = FALSE) +
#         labs(title = title) +
#         theme_minimal() +
#         theme(
#             plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
#             legend.position = "bottom",
#             legend.title = element_text(size = 9),
#             legend.text = element_text(size = 8)
#         )
#
#     if (is.null(out_file)) {
#         out_file <- ds(sprintf("figures/%s_normal_%s.png", level, var))
#     }
#
#     ggsave(
#         out_file,
#         p,
#         device = ragg::agg_png,
#         width = 12,
#         height = 6,
#         units = "in",
#         dpi = 150
#     )
#     message("Saved: ", normalizePath(out_file))
#     invisible(out_file)
# }
#
#
# # PRISM normal solar radiation
# plot_normal_map(
#     var = "soltotal",
#     level = "county",
#     include_alaska = FALSE,
#     include_hawaii = FALSE,
#     legend_title = "Solar Radiation (MJ/m²/day)",
#     palette = "inferno",
#     title = "County normal PRISM SolTotal"
# )
#
