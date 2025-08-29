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
knitr::opts_chunk$set(dev = "ragg_png", dpi = 150)

dir.create(
    file.path(project_dir, "figures"),
    showWarnings = FALSE,
    recursive = TRUE
)
options(ragg.max_dim = 10000)

library(arrow)
library(dplyr)
library(tidyverse)
library(sf)
library(gganimate)
library(cowplot)
library(ragg)
library(gifski)
library(scales)

ds <- function(...) file.path(project_dir, ...)

# ==== DATASET: county monthly ====
county_monthly <- open_dataset(ds(
    "handoffs/county_monthly_long/county_monthly.parquet"
))

###########################################################################################
# HMS smoke — county monthly animated map

counties_all <- sf::st_read(
    ds("clean_data/county_census/canonical_2024.gpkg"),
    layer = "counties_500k",
    quiet = TRUE
) %>%
    sf::st_make_valid()

smoke_df <- county_monthly %>%
    filter(
        variable %in%
            c("prop_light_coverage", "prop_med_coverage", "prop_heavy_coverage")
    ) %>%
    select(geoid, year, month, variable, value) %>%
    collect()

smoke_wide <- smoke_df %>%
    tidyr::pivot_wider(
        names_from = variable,
        values_from = value,
        values_fill = 0
    ) %>%
    mutate(
        category = case_when(
            prop_heavy_coverage > 0 ~ "Heavy",
            prop_med_coverage > 0 ~ "Medium",
            prop_light_coverage > 0 ~ "Light",
            TRUE ~ "None"
        ),
        final_value = case_when(
            category == "Heavy" ~ prop_heavy_coverage,
            category == "Medium" ~ prop_med_coverage,
            category == "Light" ~ prop_light_coverage,
            TRUE ~ 0
        ),
        ym = sprintf("%04d-%02d", year, month)
    )

plot_df <- counties_all %>%
    left_join(smoke_wide, by = "geoid") %>%
    sf::st_as_sf() %>%
    filter(substr(geoid, 1, 2) != "15") %>% # drop HI, keep AK
    filter(!sf::st_is_empty(sf::st_geometry(.))) %>%
    mutate(time_state = factor(ym, levels = ym %>% unique() %>% sort()))

smoke_colors <- c(
    "Light" = "#eccc7c",
    "Medium" = "#dc8b30",
    "Heavy" = "#d96527",
    "None" = "#dfdac4"
)

p_main <- ggplot(plot_df) +
    geom_sf(aes(fill = category, alpha = final_value), color = NA) +
    scale_fill_manual(
        values = smoke_colors,
        breaks = c("None", "Light", "Medium", "Heavy"),
        na.value = "grey80"
    ) +
    scale_alpha(range = c(0.2, 1), guide = "none") +
    coord_sf(xlim = c(-165, -67), ylim = c(20, 72), expand = FALSE) +
    theme_void() +
    theme(
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    ) +
    labs(
        title = "County-Monthly HMS Smoke Coverage — {current_frame}",
        fill = "Smoke Category"
    ) +
    gganimate::transition_manual(time_state)

nframes <- plot_df$time_state %>% unique() %>% length()
anim <- gganimate::animate(
    p_main,
    nframes = max(1L, nframes),
    fps = 4,
    width = 1000,
    height = 600,
    units = "px",
    renderer = gifski_renderer(),
    device = "ragg_png"
)
gganimate::anim_save(ds("figures/county_monthly_hms_smoke.gif"), anim)

###########################################################################################
# Generic GIF animator (defaults: county + monthly)

animate_geo_gif <- function(
    var,
    level = c("county", "tract"),
    agg = c("monthly", "annual"),
    include_alaska = TRUE,
    include_hawaii = FALSE,
    bbox = NULL,
    legend_title = var,
    title = NULL,
    palette = "mako",
    direction = -1,
    trans = "identity",
    labels = scales::label_number(accuracy = 0.1),
    na_fill = "grey80",
    width = 1000,
    height = 600,
    fps = 4,
    out_path = NULL,
    value_fun = NULL,
    drop_na_time = TRUE,
    tween_shapes = FALSE
) {
    level <- match.arg(level)
    agg <- match.arg(agg)

    geom_layer <- if (level == "tract") "tracts_500k" else "counties_500k"
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
                if (agg == "monthly") {
                    dplyr::select(., geoid, year, month, value)
                } else {
                    dplyr::select(., geoid, year, value)
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

    if (agg == "monthly") {
        if (drop_na_time) {
            df <- dplyr::filter(df, !is.na(year), !is.na(month))
        }
        df <- df %>% dplyr::mutate(ym = sprintf("%04d-%02d", year, month))
        levs <- df %>%
            dplyr::arrange(year, month) %>%
            dplyr::distinct(ym) %>%
            dplyr::pull(ym)
        df <- df %>% dplyr::mutate(time_state = factor(ym, levels = levs))
    } else {
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
            "%s-%s %s — {current_frame}",
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

# Expose dataset aliases expected by animate_geo_gif()
county_monthly <- county_monthly
county_annual <- NULL
tract_monthly <- NULL
tract_annual <- NULL

###########################################################################################
# Examples — county monthly animations

# MERRA-2 dust (if monthly available; converts kg/m^3 -> µg/m^3)
animate_geo_gif(
    var = "dusmass25",
    level = "county",
    agg = "monthly",
    include_alaska = TRUE,
    include_hawaii = FALSE,
    bbox = c(-170, -60, 18, 72),
    legend_title = expression("Dust (µg·m"^-3 * ")"),
    palette = "magma",
    direction = 1,
    trans = "log10",
    labels = scales::label_number(accuracy = 0.1),
    value_fun = function(x) x * 1e9,
    out_path = ds("figures/county_monthly_dusmass.gif"),
    title = "County monthly merra2 dusmass25 — {current_frame}"
)

# gridMET rmax
animate_geo_gif(
    var = "rmax",
    level = "county",
    agg = "monthly",
    include_alaska = FALSE,
    include_hawaii = FALSE,
    bbox = c(-125, -66, 24, 50),
    legend_title = "Rmax",
    palette = "plasma",
    direction = -1,
    out_path = ds("figures/county_monthly_rmax.gif"),
    title = "County monthly gridmet rmax — {current_frame}"
)

# TerraClimate tmin (°C)
animate_geo_gif(
    var = "tmin",
    level = "county",
    agg = "monthly",
    include_alaska = TRUE,
    include_hawaii = FALSE,
    bbox = c(-125, -66, 24, 50),
    legend_title = expression(T[min] * " (°C)"),
    palette = "turbo",
    direction = 1,
    out_path = ds("figures/county_monthly_tmin.gif"),
    title = "County monthly terraclimate tmin — {current_frame}"
)

# MODIS EVI
animate_geo_gif(
    var = "evi",
    level = "county",
    agg = "monthly",
    include_alaska = FALSE,
    include_hawaii = FALSE,
    bbox = c(-125, -66, 24, 50),
    legend_title = "EVI",
    palette = "viridis",
    direction = -1,
    out_path = ds("figures/county_monthly_evi.gif"),
    title = "County monthly MODIS EVI — {current_frame}"
)

###########################################################################################
# NLCD panel (counties) — monthly dataset averaged across months in target year

target_year <- 2021
vars <- c("land_cover_24", "land_cover_42", "land_cover_82")

pretty_names <- c(
    land_cover_24 = "Developed, High Intensity",
    land_cover_42 = "Evergreen Forest",
    land_cover_82 = "Cultivated Crops"
)

high_colors <- c(
    land_cover_24 = "#5e503f",
    land_cover_42 = "#606c38",
    land_cover_82 = "#dda15e"
)

counties_conus <- sf::st_read(
    ds("clean_data/county_census/canonical_2024.gpkg"),
    layer = "counties_500k",
    quiet = TRUE
) |>
    sf::st_make_valid() |>
    dplyr::filter(!substr(geoid, 1, 2) %in% c("02", "15"))

nlcd_df <- county_monthly |>
    dplyr::filter(variable %in% vars, year == target_year) |>
    dplyr::select(geoid, variable, value) |>
    dplyr::collect() |>
    dplyr::group_by(geoid, variable) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = variable, values_from = value)

plot_df <- dplyr::left_join(counties_conus, nlcd_df, by = "geoid")

map_one <- function(col_name) {
    ggplot(plot_df) +
        geom_sf(aes(fill = .data[[col_name]]), color = NA) +
        scale_fill_gradient(
            low = "white",
            high = high_colors[[col_name]],
            limits = c(0, 1),
            oob = scales::squish,
            name = "Proportion",
            na.value = "grey90",
            breaks = c(0, 0.25, 0.5, 0.75, 1)
        ) +
        coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
        labs(title = paste0(pretty_names[[col_name]], " — ", target_year)) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
            legend.position = "bottom",
            legend.title = element_text(size = 9),
            legend.text = element_text(size = 8)
        )
}

panel <- cowplot::plot_grid(
    map_one(vars[1]),
    map_one(vars[2]),
    map_one(vars[3]),
    ncol = 3
)
outfile <- ds("figures/county_monthly_nlcd.png")
ggsave(
    outfile,
    panel,
    device = ragg::agg_png,
    width = 15,
    height = 6,
    units = "in",
    dpi = 150
)
message("Saved: ", normalizePath(outfile))

###########################################################################################
# Köppen panel (counties) — “static” from monthly dataset (averaged if repeated)

vars <- c("koppen_14", "koppen_7", "koppen_26")
pretty_names <- c(
    koppen_14 = "Cfa — Temperate, no dry season, hot summer",
    koppen_7 = "BSk — Arid, steppe, cold",
    koppen_26 = "Dfb — Cold, no dry season, warm summer"
)
high_colors <- c(
    koppen_14 = "#254e2c",
    koppen_7 = "#8da4ac",
    koppen_26 = "#edcc6f"
)

counties_ak_conus <- sf::st_read(
    ds("clean_data/county_census/canonical_2024.gpkg"),
    layer = "counties_500k",
    quiet = TRUE
) |>
    sf::st_make_valid() |>
    dplyr::filter(substr(geoid, 1, 2) != "15")

bbox <- c(-170, -60, 18, 72)

koppen_df <- county_monthly |>
    dplyr::filter(variable %in% vars, year == "static") |>
    dplyr::select(geoid, variable, value) |>
    dplyr::collect() |>
    dplyr::group_by(geoid, variable) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = variable, values_from = value)

plot_df <- dplyr::left_join(counties_ak_conus, koppen_df, by = "geoid")

map_one <- function(col_name) {
    ggplot(plot_df) +
        geom_sf(aes(fill = .data[[col_name]]), color = NA) +
        scale_fill_gradient(
            low = "white",
            high = high_colors[[col_name]],
            limits = c(0, 1),
            oob = scales::squish,
            name = "Proportion",
            na.value = "grey90",
            breaks = c(0, 0.25, 0.5, 0.75, 1)
        ) +
        coord_sf(xlim = bbox[1:2], ylim = bbox[3:4], expand = FALSE) +
        labs(title = paste0(pretty_names[[col_name]], " — static")) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
            legend.position = "bottom",
            legend.title = element_text(size = 9),
            legend.text = element_text(size = 8)
        )
}

panel <- cowplot::plot_grid(
    map_one(vars[1]),
    map_one(vars[2]),
    map_one(vars[3]),
    ncol = 3
)
outfile <- ds("figures/county_monthly_koppen.png")
ggsave(
    outfile,
    panel,
    device = ragg::agg_png,
    width = 15,
    height = 6,
    units = "in",
    dpi = 150
)
message("Saved: ", normalizePath(outfile))
