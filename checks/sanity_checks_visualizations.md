Sanity Checks and Visualizations for Aggregated Amadeus Datasets
================

# set container library

``` r
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
dir.create(file.path(project_dir, "figures"), showWarnings = FALSE, recursive = TRUE)

# Keep ragg from interpreting huge inch sizes
options(ragg.max_dim = 10000)  # optional; with units="px" you shouldn't hit the cap
```

\# Load packages

# need to add to container: geoMSF,

# Load data

``` r
#for interactive

# Resolve paths relative to root.dir set above
ds <- function(...) file.path(project_dir, ...)

county_annual  <- open_dataset(ds("handoffs/county_annual_long/county_annual.parquet"))
county_monthly <- open_dataset(ds("handoffs/county_monthly_long/county_monthly.parquet"))
tract_annual   <- open_dataset(ds("handoffs/tract_annual_long/tract_annual.parquet"))
tract_monthly  <- open_dataset(ds("handoffs/tract_monthly_long/tract_monthly.parquet"))
```

# 

# \# County-Annual Sanity checks

# `{r} # # check for duplicated geoid-year-variable combinations # anyDuplicated(county_annual[c("geoid", "year", "variable")]) #  # # Basic shape # # should be 3144 counties, 17 years (2010-2024) normal and static, X vars # county_annual %>% #   summarise( #     n_rows     = n(), #     n_counties = n_distinct(geoid), #     n_years    = n_distinct(year), #     n_vars     = n_distinct(variable) #   ) %>% #   collect() #  # # List distinct years in the dataset # county_annual %>% #   distinct(year) %>% #   arrange(year) %>% #   collect() #  #  # var_summary <- county_annual %>% #   group_by(variable) %>% #   summarise( #     min_val   = min(value, na.rm = TRUE), #     max_val   = max(value, na.rm = TRUE), #     mean_val  = mean(value, na.rm = TRUE), #     median_val = median(value, na.rm = TRUE), #     sd_val    = sd(value, na.rm = TRUE) #   ) %>% #   arrange(variable) %>% #   collect() #  #  # # Expected ranges table # expected_ranges <- tribble( #   ~variable,                       ~min_exp,  ~max_exp, #   "aet",                           0,        250, #   "albedo",                        0,        0.9, #   "area_km2",                      5,        383000, #   "bcsmass",                       1e-11,    1e-8, #   "be30_grd",                      0,        6000, #   "cldtot",                        0,        1, #   "def",                           0,        300, #   "ds30_grd",                      0,        6000, #   "dusmass25",                     1e-11,    2e-8, #   "evi",                           -0.1,     1, #   "etr",                           0,        18, #   "evap",                          1e-8,     1e-4, #   "fractional_impervious_surface", 0,        100, #   "grn",                           0,        1, #   "gwetroot",                      0,        1, #   "koppen_1",                      0,        1, #   "koppen_14",                     0,        1, #   "koppen_15",                     0,        1, #   "koppen_16",                     0,        1, #   "koppen_17",                     0,        1, #   "koppen_18",                     0,        1, #   "koppen_19",                     0,        1, #   "koppen_2",                      0,        1, #   "koppen_21",                     0,        1, #   "koppen_22",                     0,        1, #   "koppen_23",                     0,        1, #   "koppen_25",                     0,        1, #   "koppen_26",                     0,        1, #   "koppen_27",                     0,        1, #   "koppen_29",                     0,        1, #   "koppen_3",                      0,        1, #   "koppen_30",                     0,        1, #   "koppen_4",                      0,        1, #   "koppen_5",                      0,        1, #   "koppen_6",                      0,        1, #   "koppen_7",                      0,        1, #   "koppen_8",                      0,        1, #   "koppen_9",                      0,        1, #   "koppen_confidence",             0,        100, #   "lai",                           0,        7, #  "land_cover_11",                 0,        1, #   "land_cover_12",                 0,        1, #   "land_cover_21",                 0,        1, #   "land_cover_22",                 0,        1, #   "land_cover_23",                 0,        1, #   "land_cover_24",                 0,        1, #   "land_cover_31",                 0,        1, #   "land_cover_41",                 0,        1, #   "land_cover_42",                 0,        1, #   "land_cover_43",                 0,        1, #   "land_cover_52",                 0,        1, #   "land_cover_71",                 0,        1, #   "land_cover_81",                 0,        1, #   "land_cover_82",                 0,        1, #   "land_cover_90",                 0,        1, #   "land_cover_95",                 0,        1, #   "land_cover_confidence",    0,        100, #   "lst_day_1km_k",                 230,      330, #   "lst_night_1km_k",               230,      310, #   "lwgab",                         200,      400, #   "md30_grd",                      -400,     8850, #   "mi30_grd",                      -400,     8850, #   "mn30_grd",                      -400,     8850, #   "mx30_grd",                      0,        8850, #   "ndvi",                          -0.1,     1, #   "pblh",                          50,       2500, #   "pdsi",                          -9,       20, #   "pet",                           0,        350, #   "ppt",                           0,        900, #   "pr",                            0,        45, #   "precsno",                       0,        7e-5, #   "prectotcorr",                   1e-6,     5e-4, #   "prop_cover_burnaddwaterbody",   0,        1, #   "prop_cover_catchment",          0,        1, #   "prop_cover_catchmentsp",        0,        1, #   "prop_cover_huc12",              0,        1, #   "prop_cover_landsea",            0,        1, #   "prop_cover_nhdarea",            0,        1, #   "prop_cover_nhdwaterbody",       0,        1, #   "prop_heavy_coverage",           0,        1, #   "prop_light_coverage",           0,        1, #   "prop_med_coverage",             0,        1, #   "ps",                            50000,    105000, #   "qv2m",                          0,        0.04, #   "rmax",                          0,        100, #   "rmin",                          0,        100, #   "road_density_km_per_km2",       0,        8, #   "sd30_grd",                      0,        600, #   "slp",                           99000,    102500, #   "soil",                          0,        420, #   "solclear",                      2,        35, #   "solslope",                      0,        40, #   "soltotal",                      0,        32, #   "soltrans",                      0,        1, #   "sph",                           0,        0.03, #   "srad",                          0,        400, #   "sur_refl_b01",                  0,        0.6, #   "sur_refl_b02",                  0,        0.6, #   "sur_refl_b03",                  0,        0.7, #   "sur_refl_b04",                  0,        0.6, #   "sur_refl_b05",                  0,        0.5, #   "sur_refl_b06",                  0,        0.5, #   "sur_refl_b07",                  0,        0.5, #   "swe",                           0,        1200, #   "t2mdew",                        230,      350, #   "tdmean",                        -40,      27, #   "th",                            0,        360, #   "tmax",                          -50,      50, #   "tmean",                         -10,      30, #   "tmin",                          -60,      30, #   "tmmn",                          240,      350, #   "tmmx",                          240,      350, #   "total_air_lb",                  0,        1e8, #   "total_air_lb_per_km2",          0,        1e5, #   "total_air_lb_plus20km",         0,        1e8, #   "total_fugitive_air_lb",         0,        1e8, #   "total_fugitive_air_lb_per_km2", 0,        1e5, #   "total_fugitive_air_lb_plus20km",0,        1e8, #   "total_road_km",                 0,        6e4, #   "total_stack_air_lb",            0,        1e8, #   "total_stack_air_lb_per_km2",    0,        1e5, #   "total_stack_air_lb_plus20km",   0,        1e8, #   "totexttau",                     0.02,     0.30, #   "ts",                            250,      305, #   "u10m",                          -20,      20, #   "vap",                           0,        5, #   "vpd",                           0,        6, #   "vpdmax",                        2,        60, #   "vpdmin",                        0,        30, #   "vs",                            0,        20, #   "ws",                            0,        14, #   "z0m",                           1e-4,     3 #  # ) #  # # Compare observed vs expected # comparison <- var_summary %>% #   left_join(expected_ranges, by = "variable") %>% #   mutate( #     min_ok = min_val >= min_exp, #     max_ok = max_val <= max_exp, #     passes = ifelse(min_ok & max_ok, "Y", "N") #   ) %>% #   select(variable, min_val, max_val, min_exp, max_exp, passes) #  # print(comparison, n = Inf) #  #  #  #`

# 

# 

# 

# 

# Plot: County Annual rmax over time

``` r
# Geometries
counties <- sf::st_read(
  ds("clean_data/county_census/canonical_2024.gpkg"),
  layer = "counties_500k",
  quiet = TRUE
) %>%
  filter(!substr(geoid, 1, 2) %in% c("02", "15"))  # drop AK + HI

# Data
rmax_df <- county_annual %>%
  filter(variable == "rmax") %>%
  select(geoid, year, value) %>%
  collect()

plot_df <- counties %>% left_join(rmax_df, by = "geoid")

# Frame count without NAs
nframes <- plot_df$year %>% unique() %>% sort(na.last = NA) %>% length()

p <- ggplot(plot_df) +
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  theme_minimal() +
  labs(
    title = "County-annual Rmax (GridMET max relative humidity): {closest_state}",
    fill  = "Rmax"
  ) +
  transition_states(year, transition_length = 1, state_length = 1) +
  ease_aes("linear")

anim <- gganimate::animate(
  p,                       # or `final` for the HMS plot
  nframes  = max(1L, nframes),
  fps      = 2,
  width    = 1000,
  height   = 600,
  units    = "px",      
  renderer = gifski_renderer(),
  device   = "ragg_png"
)


gganimate::anim_save(ds("figures/rmax_county_annual.gif"), anim)
```

![](figures/rmax_county_annual.gif)

# Plot: County Annual HMS smoke proportions over time (with AK + HI insets)

``` r
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)

# --- Load county geometries (includes Alaska + Hawaii) ---
counties_all <- sf::st_read(
  ds("clean_data/county_census/canonical_2024.gpkg"),
  layer = "counties_500k",
  quiet = TRUE
) %>%
  sf::st_make_valid()

# --- Extract HMS smoke proportion variables ---
smoke_df <- county_annual %>%
  filter(variable %in% c(
    "prop_light_coverage",
    "prop_med_coverage",
    "prop_heavy_coverage"
  )) %>%
  select(geoid, year, variable, value) %>%
  collect()

# --- Pivot to wide format for priority calculation ---
smoke_wide <- smoke_df %>%
  tidyr::pivot_wider(
    names_from  = variable,
    values_from = value,
    values_fill = 0
  ) %>%
  mutate(
    category = dplyr::case_when(
      prop_heavy_coverage > 0 ~ "Heavy",
      prop_med_coverage   > 0 ~ "Medium",
      prop_light_coverage > 0 ~ "Light",
      TRUE                    ~ "None"
    ),
    final_value = dplyr::case_when(
      category == "Heavy"  ~ prop_heavy_coverage,
      category == "Medium" ~ prop_med_coverage,
      category == "Light"  ~ prop_light_coverage,
      TRUE                 ~ 0
    )
  )

# --- Join geometries ---
plot_df <- counties_all %>%
  left_join(smoke_wide, by = "geoid") %>%
  sf::st_as_sf() %>%
  filter(!sf::st_is_empty(sf::st_geometry(.)))

# --- Remove Hawaii ONLY ---
plot_df <- plot_df %>%
  filter(substr(geoid, 1, 2) != "15")   # Keep Alaska, drop Hawaii

# --- Custom smoke colors ---
smoke_colors <- c(
  "Light"  = "#eccc7c",
  "Medium" = "#dc8b30",
  "Heavy"  = "#d96527",
  "None"   = "#dfdac4"
)

# --- Set category factor levels in desired order ---
plot_df <- plot_df %>%
  mutate(category = factor(category, levels = c("None", "Light", "Medium", "Heavy")))

# --- Build animated plot ---
p_main <- ggplot(plot_df) +
  geom_sf(aes(fill = category, alpha = final_value), color = NA) +
  scale_fill_manual(
    values = smoke_colors,
    breaks = c("None", "Light", "Medium", "Heavy"),  # <- Ensures correct legend order
    na.value = "grey80"
  ) +
  scale_alpha(range = c(0.2, 1), guide = "none") +
  coord_sf(
    xlim = c(-165, -67),
    ylim = c(20, 72),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 8),
    plot.title   = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  labs(
    title = "County-Annual HMS Smoke Coverage — Year: {current_frame}",
    fill  = "Smoke Category"
  ) +
  gganimate::transition_manual(year)


# --- Count frames safely ---
nframes <- plot_df$year %>%
  unique() %>%
  sort(na.last = NA) %>%
  length()

# --- Animate using ragg ---
anim <- gganimate::animate(
  p_main,
  nframes  = max(1L, nframes),
  fps      = 2,
  width    = 1000,
  height   = 600,
  units    = "px",
  renderer = gifski_renderer(),
  device   = "ragg_png"
)

# --- Save animation ---
gganimate::anim_save(ds("figures/hms_smoke_county_annual.gif"), anim)

# --- Display inline ---
knitr::include_graphics("figures/hms_smoke_county_annual.gif")
```

![](figures/hms_smoke_county_annual.gif)<!-- -->
![](figures/hms_smoke_county_annual.gif)

# 

# 

# 

# \# County-Monthly Sanity checks

# `{r} # # check for duplicated geoid-year-variable combinations # anyDuplicated(county_monthly[c("geoid", "year", "variable")]) #  # # Basic shape # # should be 3144 counties, 17 years (2010-2024) normal and static, 139 vars # county_monthly %>% #   summarise( #     n_rows     = n(), #     n_counties = n_distinct(geoid), #     n_years    = n_distinct(year), #     n_vars     = n_distinct(variable) #   ) %>% #   collect() #  # # List distinct years and months in the dataset # county_monthly %>% #   distinct(year, month) %>% #   arrange(year, month) %>% #   collect() %>% #   print(n = Inf) #  #  # county_monthly %>% #   filter(is.na(year) & is.na(month)) %>% #   distinct(variable) %>% #   arrange(variable) %>% #   collect() %>% #   print(n = Inf) #  #  # var_summary <- county_monthly %>% #   group_by(variable) %>% #   summarise( #     min_val   = min(value, na.rm = TRUE), #     max_val   = max(value, na.rm = TRUE), #     mean_val  = mean(value, na.rm = TRUE), #     median_val = median(value, na.rm = TRUE), #     sd_val    = sd(value, na.rm = TRUE) #   ) %>% #   arrange(variable) %>% #   collect() #  #  # # Compare observed vs expected # comparison <- var_summary %>% #   left_join(expected_ranges, by = "variable") %>% #   mutate( #     min_ok = min_val >= min_exp, #     max_ok = max_val <= max_exp, #     passes = ifelse(min_ok & max_ok, "Y", "N") #   ) %>% #   select(variable, min_val, max_val, min_exp, max_exp, passes) #  # print(comparison, n = Inf) #  #`

# 

``` r
# Plot: County-Monthly Rmax Over Time

# --- Load county geometries ---
counties_all <- sf::st_read(
  ds("clean_data/county_census/canonical_2024.gpkg"),
  layer = "counties_500k",
  quiet = TRUE
) %>%
  sf::st_make_valid()

# --- Extract Rmax values ---
rmax_df <- county_monthly %>%
  filter(variable == "rmax") %>%
  select(geoid, year, month, value) %>%
  collect() %>%
  mutate(frame_id = paste(year, sprintf("%02d", month), sep = "-"))

# --- Join geometries ---
plot_df <- counties_all %>%
  left_join(rmax_df, by = "geoid") %>%
  sf::st_as_sf() %>%
  filter(!sf::st_is_empty(sf::st_geometry(.))) %>%
  filter(substr(geoid, 1, 2) != "15")  # Remove Hawaii


# --- Build plot ---
p <- ggplot(plot_df) +
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  coord_sf(
    xlim = c(-165, -67),
    ylim = c(20, 72),
    expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = "County-Monthly Rmax (GridMET Max Relative Humidity): {current_frame}",
    fill  = "Rmax"
  ) +
  gganimate::transition_manual(frame_id)

# --- Animate ---
nframes <- plot_df$frame_id %>% unique() %>% sort(na.last = NA) %>% length()

anim <- gganimate::animate(
  p,
  nframes  = max(1L, nframes),
  fps      = 6,
  width    = 1000,
  height   = 600,
  units    = "px",
  renderer = gifski_renderer(),
  device   = "ragg_png"
)

gganimate::anim_save(ds("figures/rmax_county_monthly.gif"), anim)
knitr::include_graphics("figures/rmax_county_monthly.gif")
```

![](figures/rmax_county_monthly.gif)<!-- -->

``` r
# Plot: County Monthly HMS Smoke Coverage Over Time

# --- Load county geometries (includes Alaska + Hawaii) ---
counties_all <- sf::st_read(
  ds("clean_data/county_census/canonical_2024.gpkg"),
  layer = "counties_500k",
  quiet = TRUE
) %>%
  sf::st_make_valid()

# --- Extract HMS smoke proportion variables ---
smoke_df <- county_monthly %>%
  filter(variable %in% c(
    "prop_light_coverage",
    "prop_med_coverage",
    "prop_heavy_coverage"
  )) %>%
  select(geoid, year, month, variable, value) %>%
  collect()

# --- Pivot to wide format for priority calculation ---
smoke_wide <- smoke_df %>%
  tidyr::pivot_wider(
    names_from  = variable,
    values_from = value,
    values_fill = 0
  ) %>%
  mutate(
    category = dplyr::case_when(
      prop_heavy_coverage > 0 ~ "Heavy",
      prop_med_coverage   > 0 ~ "Medium",
      prop_light_coverage > 0 ~ "Light",
      TRUE                    ~ "None"
    ),
    final_value = dplyr::case_when(
      category == "Heavy"  ~ prop_heavy_coverage,
      category == "Medium" ~ prop_med_coverage,
      category == "Light"  ~ prop_light_coverage,
      TRUE                 ~ 0
    ),
    frame_id = paste(year, sprintf("%02d", month), sep = "-") # YYYY-MM format
  )

# --- Join geometries ---
plot_df <- counties_all %>%
  left_join(smoke_wide, by = "geoid") %>%
  sf::st_as_sf() %>%
  filter(!sf::st_is_empty(sf::st_geometry(.)))

# --- Remove Hawaii ONLY ---
plot_df <- plot_df %>%
  filter(substr(geoid, 1, 2) != "15")

# --- Custom smoke colors ---
smoke_colors <- c(
  "Light"  = "#eccc7c",
  "Medium" = "#dc8b30",
  "Heavy"  = "#d96527",
  "None"   = "#dfdac4"
)

# --- Set category order ---
plot_df <- plot_df %>%
  mutate(category = factor(category, levels = c("None", "Light", "Medium", "Heavy")))

# --- Build animated plot ---
p_main <- ggplot(plot_df) +
  geom_sf(aes(fill = category, alpha = final_value), color = NA) +
  scale_fill_manual(
    values = smoke_colors,
    breaks = c("None", "Light", "Medium", "Heavy"),
    na.value = "grey80"
  ) +
  scale_alpha(range = c(0.2, 1), guide = "none") +
  coord_sf(
    xlim = c(-165, -67),
    ylim = c(20, 72),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 8),
    plot.title   = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  labs(
    title = "County-Monthly HMS Smoke Coverage — {current_frame}",
    fill  = "Smoke Category"
  ) +
  gganimate::transition_manual(frame_id)

# --- Animate ---
nframes <- plot_df$frame_id %>% unique() %>% sort(na.last = NA) %>% length()

anim <- gganimate::animate(
  p_main,
  nframes  = max(1L, nframes),
  fps      = 6,  # Faster since more frames
  width    = 1000,
  height   = 600,
  units    = "px",
  renderer = gifski_renderer(),
  device   = "ragg_png"
)

gganimate::anim_save(ds("figures/hms_smoke_county_monthly.gif"), anim)
knitr::include_graphics("figures/hms_smoke_county_monthly.gif")
```

![](figures/hms_smoke_county_monthly.gif)<!-- -->

# 

# 

# 

# \# Tract-Annual Sanity checks

# `{r} # # check for duplicated geoid-year-variable combinations # anyDuplicated(tract_annual[c("geoid", "year", "variable")]) #  # # Basic shape # # should be over 84,414 counties, 17 years (2010-2024) normal and static, 132 vars # # 84119 census tracts - some may have had errors # tract_annual %>% #   summarise( #     n_rows     = n(), #     n_tracts = n_distinct(geoid), #     n_years    = n_distinct(year), #     n_vars     = n_distinct(variable) #   ) %>% #   collect() #  # # List distinct years in the dataset # tract_annual %>% #   distinct(year) %>% #   arrange(year) %>% #   collect() #  #  # var_summary <- tract_annual %>% #   group_by(variable) %>% #   summarise( #     min_val   = min(value, na.rm = TRUE), #     max_val   = max(value, na.rm = TRUE), #     mean_val  = mean(value, na.rm = TRUE), #     median_val = median(value, na.rm = TRUE), #     sd_val    = sd(value, na.rm = TRUE) #   ) %>% #   arrange(variable) %>% #   collect() #  #  # # Compare observed vs expected # comparison <- var_summary %>% #   left_join(expected_ranges, by = "variable") %>% #   mutate( #     min_ok = min_val >= min_exp, #     max_ok = max_val <= max_exp, #     passes = ifelse(min_ok & max_ok, "Y", "N") #   ) %>% #   select(variable, min_val, max_val, min_exp, max_exp, passes) #  # print(comparison, n = Inf) #  #`

# 

# 

``` r
# Plot: Tract-Annual Rmax Over Time

# --- Load tract geometries ---
tracts_all <- sf::st_read(
  ds("clean_data/county_census/canonical_2024.gpkg"),
  layer = "tracts_500k",
  quiet = TRUE
) %>%
  sf::st_make_valid()

# --- Extract Rmax values ---
rmax_df <- tract_annual %>%
  filter(variable == "rmax") %>%
  select(geoid, year, value) %>%
  collect()

# --- Join geometries ---
plot_df <- counties_all %>%
  left_join(rmax_df, by = "geoid") %>%
  sf::st_as_sf() %>%
  filter(!sf::st_is_empty(sf::st_geometry(.))) %>%
  filter(substr(geoid, 1, 2) != "15")  # Remove Hawaii


# --- Build plot ---
p <- ggplot(plot_df) +
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  coord_sf(
    xlim = c(-165, -67),
    ylim = c(20, 72),
    expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = "Tract-Annual Rmax (GridMET Max Relative Humidity): {current_frame}",
    fill  = "Rmax"
  ) +
  gganimate::transition_manual(year)

# --- Animate ---
nframes <- plot_df$year %>% unique() %>% sort(na.last = NA) %>% length()

anim <- gganimate::animate(
  p,
  nframes  = max(1L, nframes),
  fps      = 2,
  width    = 1000,
  height   = 600,
  units    = "px",
  renderer = gifski_renderer(),
  device   = "ragg_png"
)
```

    ## Warning in lapply(row_vars$frames, as.integer): NAs introduced by coercion

    ## Warning in split.data.frame(d, as.integer(split_panel[, 3])): NAs introduced by
    ## coercion

    ## Warning: Cannot get dimensions of plot table. Plot region might not be fixed
    ## Caused by error in `geom_sf()`:
    ## ! Problem while converting geom to grob.
    ## ℹ Error occurred in the 1st layer.
    ## Caused by error in `if (empty(data)) ...`:
    ## ! missing value where TRUE/FALSE needed

    ## Warning: Failed to plot frame
    ## Caused by error in `geom_sf()`:
    ## ! Problem while converting geom to grob.
    ## ℹ Error occurred in the 1st layer.
    ## Caused by error in `if (empty(data)) ...`:
    ## ! missing value where TRUE/FALSE needed

``` r
gganimate::anim_save(ds("figures/rmax_tract_annual.gif"), anim)
knitr::include_graphics("figures/rmax_tract_annual.gif")
```

![](figures/rmax_tract_annual.gif)<!-- -->

``` r
# Plot: Tract Annual HMS Smoke Coverage Over Time

# --- Load tract geometries (includes Alaska + Hawaii) ---
tracts_all <- sf::st_read(
  ds("clean_data/county_census/canonical_2024.gpkg"),
  layer = "tracts_500k",
  quiet = TRUE
) %>%
  sf::st_make_valid()

# --- Extract HMS smoke proportion variables ---
smoke_df <- tract_annual %>%
  filter(variable %in% c(
    "prop_light_coverage",
    "prop_med_coverage",
    "prop_heavy_coverage"
  )) %>%
  select(geoid, year, variable, value) %>%
  collect()

# --- Pivot and calculate priority ---
smoke_wide <- smoke_df %>%
  pivot_wider(
    names_from  = variable,
    values_from = value,
    values_fill = 0
  ) %>%
  mutate(
    category = case_when(
      prop_heavy_coverage > 0 ~ "Heavy",
      prop_med_coverage   > 0 ~ "Medium",
      prop_light_coverage > 0 ~ "Light",
      TRUE                    ~ "None"
    ),
    final_value = case_when(
      category == "Heavy"  ~ prop_heavy_coverage,
      category == "Medium" ~ prop_med_coverage,
      category == "Light"  ~ prop_light_coverage,
      TRUE                 ~ 0
    )
  )

# --- Join geometries ---
plot_df <- tracts_all %>%
  left_join(smoke_wide, by = "geoid") %>%
  sf::st_as_sf() %>%
  filter(!sf::st_is_empty(sf::st_geometry(.)))

# --- Remove Hawaii ---
plot_df <- plot_df %>%
  filter(substr(geoid, 1, 2) != "15")

# --- Set colors and order ---
plot_df <- plot_df %>%
  mutate(category = factor(category, levels = c("None", "Light", "Medium", "Heavy")))

p_main <- ggplot(plot_df) +
  geom_sf(aes(fill = category, alpha = final_value), color = NA) +
  scale_fill_manual(values = smoke_colors, breaks = c("None", "Light", "Medium", "Heavy")) +
  scale_alpha(range = c(0.2, 1), guide = "none") +
  coord_sf(
    xlim = c(-165, -67),
    ylim = c(20, 72),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 8),
    plot.title   = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  labs(
    title = "Tract-Annual HMS Smoke Coverage — Year: {current_frame}",
    fill  = "Smoke Category"
  ) +
  gganimate::transition_manual(year)

# --- Animate ---
nframes <- plot_df$year %>% unique() %>% sort(na.last = NA) %>% length()

anim <- gganimate::animate(
  p_main,
  nframes  = max(1L, nframes),
  fps      = 2,
  width    = 1000,
  height   = 600,
  units    = "px",
  renderer = gifski_renderer(),
  device   = "ragg_png"
)

gganimate::anim_save(ds("figures/hms_smoke_tract_annual.gif"), anim)
knitr::include_graphics("figures/hms_smoke_tract_annual.gif")
```

![](figures/hms_smoke_tract_annual.gif)<!-- -->

# 

# \# Tract-Monthly Sanity checks

# `{r} # # check for duplicated geoid-year-variable combinations # anyDuplicated(tract_monthly[c("geoid", "year", "variable")]) #  # # Basic shape # # should be over 84,414 counties, 17 years (2010-2024) normal and static, 140 vars # # 84119 census tracts - some may have had errors #  # tract_monthly %>% #   summarise( #     n_rows     = n(), #     n_tracts = n_distinct(geoid), #     n_years    = n_distinct(year), #     n_vars     = n_distinct(variable) #   ) %>% #   collect() #  #  # # List distinct years in the dataset # tract_monthly %>% #   distinct(year, month) %>% #   arrange(year, month) %>% #   collect() %>% #   print(n = Inf) #  # # terraclimate variables with missing year/month. go back and fix #  # tract_monthly %>% #   filter(is.na(year) & is.na(month)) %>% #   distinct(variable) %>% #   arrange(variable) %>% #   collect() %>% #   print(n = Inf) #  # # terraclimate variables with missing year/month. go back and fix #  # var_summary <- tract_monthly %>% #   group_by(variable) %>% #   summarise( #     min_val   = min(value, na.rm = TRUE), #     max_val   = max(value, na.rm = TRUE), #     mean_val  = mean(value, na.rm = TRUE), #     median_val = median(value, na.rm = TRUE), #     sd_val    = sd(value, na.rm = TRUE) #   ) %>% #   arrange(variable) %>% #   collect() #  # # Compare observed vs expected # comparison <- var_summary %>% #   left_join(expected_ranges, by = "variable") %>% #   mutate( #     min_ok = min_val >= min_exp, #     max_ok = max_val <= max_exp, #     passes = ifelse(min_ok & max_ok, "Y", "N") #   ) %>% #   select(variable, min_val, max_val, min_exp, max_exp, passes) #  # print(comparison, n = Inf) #`

# 

# 

``` r
# Plot: Tract-Monthly Rmax Over Time

# --- Load tract geometries ---
tracts_all <- sf::st_read(
  ds("clean_data/county_census/canonical_2024.gpkg"),
  layer = "tracts_500k",
  quiet = TRUE
) %>%
  sf::st_make_valid()

# --- Extract Rmax values ---
rmax_df <- tract_monthly %>%
  filter(variable == "rmax") %>%
  select(geoid, year, month, value) %>%
  collect() %>%
  mutate(frame_id = paste(year, sprintf("%02d", month), sep = "-"))

# --- Join geometries ---
plot_df <- counties_all %>%
  left_join(rmax_df, by = "geoid") %>%
  sf::st_as_sf() %>%
  filter(!sf::st_is_empty(sf::st_geometry(.))) %>%
  filter(substr(geoid, 1, 2) != "15")  # Remove Hawaii


# --- Build plot ---
p <- ggplot(plot_df) +
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  coord_sf(
    xlim = c(-165, -67),
    ylim = c(20, 72),
    expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = "Tract-Monthly Rmax (GridMET Max Relative Humidity): {current_frame}",
    fill  = "Rmax"
  ) +
  gganimate::transition_manual(frame_id)

# --- Animate ---
nframes <- plot_df$frame_id %>% unique() %>% sort(na.last = NA) %>% length()

anim <- gganimate::animate(
  p,
  nframes  = max(1L, nframes),
  fps      = 6,
  width    = 1000,
  height   = 600,
  units    = "px",
  renderer = gifski_renderer(),
  device   = "ragg_png"
)
```

    ## Warning in lapply(row_vars$frames, as.integer): NAs introduced by coercion

    ## Warning in split.data.frame(d, as.integer(split_panel[, 3])): NAs introduced by
    ## coercion

    ## Warning: Cannot get dimensions of plot table. Plot region might not be fixed
    ## Caused by error in `geom_sf()`:
    ## ! Problem while converting geom to grob.
    ## ℹ Error occurred in the 1st layer.
    ## Caused by error in `if (empty(data)) ...`:
    ## ! missing value where TRUE/FALSE needed

    ## Warning: Failed to plot frame
    ## Caused by error in `geom_sf()`:
    ## ! Problem while converting geom to grob.
    ## ℹ Error occurred in the 1st layer.
    ## Caused by error in `if (empty(data)) ...`:
    ## ! missing value where TRUE/FALSE needed

``` r
gganimate::anim_save(ds("figures/rmax_tract_monthly.gif"), anim)
knitr::include_graphics("figures/rmax_tract_monthly.gif")
```

![](figures/rmax_tract_monthly.gif)<!-- -->

``` r
# Plot: Tract Monthly HMS Smoke Coverage Over Time

# --- Load tract geometries ---
tracts_all <- sf::st_read(
  ds("clean_data/county_census/canonical_2024.gpkg"),
  layer = "tracts_500k",
  quiet = TRUE
) %>%
  sf::st_make_valid()

# --- Extract HMS smoke proportion variables ---
smoke_df <- tract_monthly %>%
  filter(variable %in% c(
    "prop_light_coverage",
    "prop_med_coverage",
    "prop_heavy_coverage"
  )) %>%
  select(geoid, year, month, variable, value) %>%
  collect()

# --- Pivot and calculate priority ---
smoke_wide <- smoke_df %>%
  pivot_wider(
    names_from  = variable,
    values_from = value,
    values_fill = 0
  ) %>%
  mutate(
    category = case_when(
      prop_heavy_coverage > 0 ~ "Heavy",
      prop_med_coverage   > 0 ~ "Medium",
      prop_light_coverage > 0 ~ "Light",
      TRUE                    ~ "None"
    ),
    final_value = case_when(
      category == "Heavy"  ~ prop_heavy_coverage,
      category == "Medium" ~ prop_med_coverage,
      category == "Light"  ~ prop_light_coverage,
      TRUE                 ~ 0
    ),
    frame_id = paste(year, sprintf("%02d", month), sep = "-")
  )

# --- Join geometries ---
plot_df <- tracts_all %>%
  left_join(smoke_wide, by = "geoid") %>%
  sf::st_as_sf() %>%
  filter(!sf::st_is_empty(sf::st_geometry(.)))


# --- Remove Hawaii ---
plot_df <- plot_df %>%
  filter(substr(geoid, 1, 2) != "15")

# --- Set colors and order ---
plot_df <- plot_df %>%
  mutate(category = factor(category, levels = c("None", "Light", "Medium", "Heavy")))

p_main <- ggplot(plot_df) +
  geom_sf(aes(fill = category, alpha = final_value), color = NA) +
  scale_fill_manual(values = smoke_colors, breaks = c("None", "Light", "Medium", "Heavy")) +
  scale_alpha(range = c(0.2, 1), guide = "none") +
  coord_sf(
    xlim = c(-165, -67),
    ylim = c(20, 72),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 8),
    plot.title   = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  labs(
    title = "Tract-Monthly HMS Smoke Coverage — {current_frame}",
    fill  = "Smoke Category"
  ) +
  gganimate::transition_manual(frame_id)

# --- Animate ---
nframes <- plot_df$frame_id %>% unique() %>% sort(na.last = NA) %>% length()

anim <- gganimate::animate(
  p_main,
  nframes  = max(1L, nframes),
  fps      = 6,
  width    = 1000,
  height   = 600,
  units    = "px",
  renderer = gifski_renderer(),
  device   = "ragg_png"
)

gganimate::anim_save(ds("figures/hms_smoke_tract_monthly.gif"), anim)
knitr::include_graphics("figures/hms_smoke_tract_monthly.gif")
```

![](figures/hms_smoke_tract_monthly.gif)<!-- -->

# 

# \# to do

# - make some visualizations of select variables over time and space using ggplot::geom_sf purr::walk

# do one per dataset (e.g. terraclimate aet, modis ndvi, nlcd land cover) per aggregation level (county annual, county monthly, tract annual, tract monthly)

# knit at the end of targets
