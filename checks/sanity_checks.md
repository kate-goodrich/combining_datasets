Sanity Checks and Visualizations for Aggregated Amadeus Datasets
================

- [set container library](#set-container-library)
- [Load data](#load-data)
- [County-Annual Sanity checks](#county-annual-sanity-checks)
- [County-Monthly Sanity checks](#county-monthly-sanity-checks)
- [Tract-Annual Sanity checks](#tract-annual-sanity-checks)
- [Tract-Monthly Sanity checks](#tract-monthly-sanity-checks)

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


# Keep ragg from interpreting huge inch sizes
options(ragg.max_dim = 10000)  # optional; with units="px" you shouldn't hit the cap
```

\# Load packages

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

# County-Annual Sanity checks

``` r
# check for duplicated geoid-year-variable combinations
anyDuplicated(county_annual[c("geoid", "year", "variable")])
```

    ## [1] 0

``` r
# Basic shape
# should be 3144 counties, 17 years (2010-2024) normal and static, 148 vars
county_annual %>%
  summarise(
    n_rows     = n(),
    n_counties = n_distinct(geoid),
    n_years    = n_distinct(year),
    n_vars     = n_distinct(variable)
  ) %>%
  collect()
```

    ## # A tibble: 1 × 4
    ##    n_rows n_counties n_years n_vars
    ##     <int>      <int>   <int>  <int>
    ## 1 4275826       3144      17    148

``` r
# List distinct years in the dataset
county_annual %>%
  distinct(year) %>%
  arrange(year) %>%
  collect()
```

    ## # A tibble: 17 × 1
    ##    year  
    ##    <chr> 
    ##  1 2010  
    ##  2 2011  
    ##  3 2012  
    ##  4 2013  
    ##  5 2014  
    ##  6 2015  
    ##  7 2016  
    ##  8 2017  
    ##  9 2018  
    ## 10 2019  
    ## 11 2020  
    ## 12 2021  
    ## 13 2022  
    ## 14 2023  
    ## 15 2024  
    ## 16 normal
    ## 17 static

``` r
var_summary <- county_annual %>%
  group_by(variable) %>%
  summarise(
    min_val   = min(value, na.rm = TRUE),
    max_val   = max(value, na.rm = TRUE),
    mean_val  = mean(value, na.rm = TRUE),
    median_val = median(value, na.rm = TRUE),
    sd_val    = sd(value, na.rm = TRUE)
  ) %>%
  arrange(variable) %>%
  collect()
```

    ## Warning: median() currently returns an approximate median in Arrow
    ## This warning is displayed once per session.

``` r
# Expected ranges table
expected_ranges <- tribble(
  ~variable,                       ~min_exp,  ~max_exp,
  "aet",                           0,        250,
  "albedo",                        0,        0.9,
  "area_km2",                      5,        383000,
  "bcsmass",                       1e-11,    1e-8,
  "be30_grd",                      0,        6000,
  "cldtot",                        0,        1,
  "def",                           0,        340,
  "ds30_grd",                      0,        6000,
  "dusmass25",                     1e-11,    3e-8,
  "evi",                           -0.1,     1,
  "etr",                           0,        18,
  "evap",                          1e-8,     1e-4,
  "fractional_impervious_surface", 0,        100,
    "fractional_impervious_surface_mean", 0,        100,
  "grn",                           0,        1,
  "gwetroot",                      0,        1,
  "koppen_1",                      0,        1,
  "koppen_14",                     0,        1,
  "koppen_15",                     0,        1,
  "koppen_16",                     0,        1,
  "koppen_17",                     0,        1,
  "koppen_18",                     0,        1,
  "koppen_19",                     0,        1,
  "koppen_2",                      0,        1,
  "koppen_21",                     0,        1,
  "koppen_22",                     0,        1,
  "koppen_23",                     0,        1,
  "koppen_25",                     0,        1,
  "koppen_26",                     0,        1,
  "koppen_27",                     0,        1,
  "koppen_29",                     0,        1,
  "koppen_3",                      0,        1,
  "koppen_30",                     0,        1,
  "koppen_4",                      0,        1,
  "koppen_5",                      0,        1,
  "koppen_6",                      0,        1,
  "koppen_7",                      0,        1,
  "koppen_8",                      0,        1,
  "koppen_9",                      0,        1,
    "prop_cover_huc2_01",               0,       1,
  "prop_cover_huc2_02",               0,         1,
  "prop_cover_huc2_03",               0,         1,
  "prop_cover_huc2_04",               0,         1,
  "prop_cover_huc2_05",               0,         1,
  "prop_cover_huc2_06",               0,         1,
  "prop_cover_huc2_07",               0,         1,
  "prop_cover_huc2_08",               0,         1,
  "prop_cover_huc2_09",               0,         1,
  "prop_cover_huc2_10",               0,         1,
  "prop_cover_huc2_11",               0,         1,
  "prop_cover_huc2_12",               0,         1,
  "prop_cover_huc2_13",               0,         1,
  "prop_cover_huc2_14",               0,         1,
  "prop_cover_huc2_15",               0,         1,
  "prop_cover_huc2_16",               0,         1,
  "prop_cover_huc2_17",               0,         1,
  "prop_cover_huc2_18",               0,         1,
  "prop_cover_huc2_20",               0,         1,
  "prop_cover_huc2_21",               0,         1,
  "prop_cover_huc2_22",               0,         1,
  "koppen_confidence",             0,        100,
  "lai",                           0,        9,
 "land_cover_11",                 0,        1,
  "land_cover_12",                 0,        1,
  "land_cover_21",                 0,        1,
  "land_cover_22",                 0,        1,
  "land_cover_23",                 0,        1,
  "land_cover_24",                 0,        1,
  "land_cover_31",                 0,        1,
  "land_cover_41",                 0,        1,
  "land_cover_42",                 0,        1,
  "land_cover_43",                 0,        1,
  "land_cover_52",                 0,        1,
  "land_cover_71",                 0,        1,
  "land_cover_81",                 0,        1,
  "land_cover_82",                 0,        1,
  "land_cover_90",                 0,        1,
  "land_cover_95",                 0,        1,
  "land_cover_confidence",    0,        100,
  "land_cover_confidence_mean",    0,        100,
  "lst_day_1km_k",                 230,      400,
  "lst_night_1km_k",               230,      310,
  "lwgab",                         200,      400,
  "md30_grd",                      -400,     8850,
  "mi30_grd",                      -400,     8850,
  "mn30_grd",                      -400,     8850,
  "mx30_grd",                      0,        8850,
  "ndvi",                          -1,     1,
  "pblh",                          50,       2500,
  "pdsi",                          -10,       20,
  "pet",                           0,        350,
  "ppt",                           0,        1400,
  "pr",                            0,        50,
  "precsno",                       0,        7e-5,
  "prectotcorr",                   1e-7,     5e-4,
  "prop_heavy_coverage",           0,        1,
  "prop_light_coverage",           0,        1,
  "prop_med_coverage",             0,        1,
  "ps",                            50000,    105000,
  "qv2m",                          0,        0.04,
  "rmax",                          0,        100,
  "rmin",                          0,        100,
  "road_density_km_per_km2",       0,        9,
  "sd30_grd",                      0,        600,
  "slp",                           99000,    102500,
  "soil",                          0,        500,
  "solclear",                      2,        35,
  "solslope",                      0,        40,
  "soltotal",                      0,        32,
  "soltrans",                      0,        1,
  "sph",                           0,        0.03,
  "srad",                          0,        400,
  "sur_refl_b01",                  -.1,        1,
  "sur_refl_b02",                  0,        1,
  "sur_refl_b03",                  0,        1,
  "sur_refl_b04",                  0,        1,
  "sur_refl_b05",                  0,        1,
  "sur_refl_b06",                  0,        1,
  "sur_refl_b07",                  0,        1,
  "swe",                           0,        1300,
  "t2mdew",                        230,      350,
  "tdmean",                        -40,      27,
  "th",                            0,        360,
  "tmax",                          -50,      50,
  "tmean",                         -16,      40,
  "tmin",                          -60,      33,
  "tmmn",                          240,      350,
  "tmmx",                          240,      350,
  "annual_total_air_lb",                  0,        1e8,
  "annual_total_air_lb_per_km2",          0,        1e6,
  "annual_total_air_lb_plusbuffer",         0,        1e8,
  "annual_total_fugitive_air_lb",         0,        1e8,
  "annual_total_fugitive_air_lb_per_km2", 0,        5e5,
  "annual_total_fugitive_air_lb_plusbuffer",0,        1e8,
  "total_road_km",                 0,        6e4,
  "annual_total_stack_air_lb",            0,        1e8,
  "annual_total_stack_air_lb_per_km2",    0,        1e6,
  "annual_total_stack_air_lb_plusbuffer",   0,        1e8,
  "totexttau",                     0.02,     0.30,
  "ts",                            250,      305,
  "u10m",                          -20,      20,
  "vap",                           0,        5,
  "vpd",                           0,        6,
  "vpdmax",                        0,        80,
  "vpdmin",                        0,        30,
  "vs",                            0,        20,
  "ws",                            0,        14,
  "z0m",                           1e-4,     3,
  "tmin_norm",  -60,      30,
  "tmax_norm" ,  -50,      50,

)

# Compare observed vs expected
comparison <- var_summary %>%
  left_join(expected_ranges, by = "variable") %>%
  mutate(
    min_ok = min_val >= min_exp,
    max_ok = max_val <= max_exp,
    passes = ifelse(min_ok & max_ok, "Y", "N")
  ) %>%
  select(variable, min_val, max_val, min_exp, max_exp, passes)

print(comparison, n = Inf)
```

    ## # A tibble: 148 × 6
    ##     variable                             min_val max_val  min_exp max_exp passes
    ##     <chr>                                  <dbl>   <dbl>    <dbl>   <dbl> <chr> 
    ##   1 aet                                 2.73e+ 0 1.14e+2  0       2.5 e+2 Y     
    ##   2 albedo                              8.44e- 2 6.06e-1  0       9   e-1 Y     
    ##   3 annual_total_air_lb                 0        8.91e+6  0       1   e+8 Y     
    ##   4 annual_total_air_lb_per_km2         0        9.16e+4  0       1   e+6 Y     
    ##   5 annual_total_air_lb_plusbuffer      0        1.14e+7  0       1   e+8 Y     
    ##   6 annual_total_fugitive_air_lb        0        8.48e+6  0       1   e+8 Y     
    ##   7 annual_total_fugitive_air_lb_per_…  0        1.54e+4  0       5   e+5 Y     
    ##   8 annual_total_fugitive_air_lb_plus…  0        8.52e+6  0       1   e+8 Y     
    ##   9 annual_total_stack_air_lb           0        5.73e+6  0       1   e+8 Y     
    ##  10 annual_total_stack_air_lb_per_km2   0        9.16e+4  0       1   e+6 Y     
    ##  11 annual_total_stack_air_lb_plusbuf…  0        9.68e+6  0       1   e+8 Y     
    ##  12 area_km2                            5.29e+ 0 3.83e+5  5  e+ 0 3.83e+5 Y     
    ##  13 bcsmass                             1.77e-11 2.74e-9  1  e-11 1   e-8 Y     
    ##  14 be30_grd                            8.68e- 1 3.45e+3  0       6   e+3 Y     
    ##  15 cldtot                              2.05e- 1 7.68e-1  0       1   e+0 Y     
    ##  16 def                                 0        1.78e+2  0       3.4 e+2 Y     
    ##  17 ds30_grd                            8.16e- 1 3.48e+3  0       6   e+3 Y     
    ##  18 dusmass25                           2.41e-10 1.39e-8  1  e-11 3   e-8 Y     
    ##  19 etr                                 2.07e+ 0 9.44e+0  0       1.8 e+1 Y     
    ##  20 evap                                2.54e- 6 5.90e-5  1  e- 8 1   e-4 Y     
    ##  21 evi                                 5.94e- 2 4.71e-1 -1  e- 1 1   e+0 Y     
    ##  22 fractional_impervious_surface       7.67e- 2 6.27e+1  0       1   e+2 Y     
    ##  23 grn                                 0        9.81e-1  0       1   e+0 Y     
    ##  24 gwetroot                            1.69e- 1 9.47e-1  0       1   e+0 Y     
    ##  25 koppen_1                            2.36e- 2 6.09e-1  0       1   e+0 Y     
    ##  26 koppen_14                           9.20e- 5 1   e+0  0       1   e+0 Y     
    ##  27 koppen_15                           5.20e- 5 5.97e-1  0       1   e+0 Y     
    ##  28 koppen_16                           4.38e- 4 3.76e-2  0       1   e+0 Y     
    ##  29 koppen_17                           5.31e- 3 2.83e-1  0       1   e+0 Y     
    ##  30 koppen_18                           1.62e- 4 1   e+0  0       1   e+0 Y     
    ##  31 koppen_19                           4.90e- 8 4.81e-1  0       1   e+0 Y     
    ##  32 koppen_2                            1.55e- 3 6.40e-1  0       1   e+0 Y     
    ##  33 koppen_21                           1.57e- 4 9.98e-1  0       1   e+0 Y     
    ##  34 koppen_22                           2.11e- 3 9.19e-1  0       1   e+0 Y     
    ##  35 koppen_23                           2.62e- 4 2.04e-1  0       1   e+0 Y     
    ##  36 koppen_25                           1.70e- 4 1   e+0  0       1   e+0 Y     
    ##  37 koppen_26                           6.87e- 8 1   e+0  0       1   e+0 Y     
    ##  38 koppen_27                           5.51e- 5 1   e+0  0       1   e+0 Y     
    ##  39 koppen_29                           2.41e- 5 9.57e-1  0       1   e+0 Y     
    ##  40 koppen_3                            2.97e- 2 1   e+0  0       1   e+0 Y     
    ##  41 koppen_30                           7.85e- 5 4.52e-3  0       1   e+0 Y     
    ##  42 koppen_4                            1.86e- 6 1   e+0  0       1   e+0 Y     
    ##  43 koppen_5                            3.98e- 5 9.70e-1  0       1   e+0 Y     
    ##  44 koppen_6                            1.61e- 3 1   e+0  0       1   e+0 Y     
    ##  45 koppen_7                            9.93e- 5 1   e+0  0       1   e+0 Y     
    ##  46 koppen_8                            5.65e- 5 1   e+0  0       1   e+0 Y     
    ##  47 koppen_9                            2.23e- 3 1   e+0  0       1   e+0 Y     
    ##  48 koppen_confidence                   4.46e+ 1 1   e+2  0       1   e+2 Y     
    ##  49 lai                                 1.00e-20 6.99e+0  0       9   e+0 Y     
    ##  50 land_cover_11                       0        5.75e-1  0       1   e+0 Y     
    ##  51 land_cover_12                       0        2.53e-2  0       1   e+0 Y     
    ##  52 land_cover_21                       0        3.62e-1  0       1   e+0 Y     
    ##  53 land_cover_22                       0        3.71e-1  0       1   e+0 Y     
    ##  54 land_cover_23                       0        4.85e-1  0       1   e+0 Y     
    ##  55 land_cover_24                       0        4.20e-1  0       1   e+0 Y     
    ##  56 land_cover_31                       0        3.94e-1  0       1   e+0 Y     
    ##  57 land_cover_41                       0        9.14e-1  0       1   e+0 Y     
    ##  58 land_cover_42                       0        8.82e-1  0       1   e+0 Y     
    ##  59 land_cover_43                       0        4.25e-1  0       1   e+0 Y     
    ##  60 land_cover_52                       0        9.91e-1  0       1   e+0 Y     
    ##  61 land_cover_71                       0        9.72e-1  0       1   e+0 Y     
    ##  62 land_cover_81                       0        8.39e-1  0       1   e+0 Y     
    ##  63 land_cover_82                       0        9.15e-1  0       1   e+0 Y     
    ##  64 land_cover_90                       0        7.37e-1  0       1   e+0 Y     
    ##  65 land_cover_95                       0        6.22e-1  0       1   e+0 Y     
    ##  66 land_cover_confidence               0        8.70e+1  0       1   e+2 Y     
    ##  67 lst_day_1km_k                       2.68e+ 2 3.14e+2  2.3e+ 2 4   e+2 Y     
    ##  68 lst_night_1km_k                     2.64e+ 2 2.95e+2  2.3e+ 2 3.1 e+2 Y     
    ##  69 lwgab                               2.02e+ 2 3.93e+2  2  e+ 2 4   e+2 Y     
    ##  70 md30_grd                            6.34e- 1 3.47e+3 -4  e+ 2 8.85e+3 Y     
    ##  71 mi30_grd                           -1.71e+ 0 3.31e+3 -4  e+ 2 8.85e+3 Y     
    ##  72 mn30_grd                            7.04e- 1 3.48e+3 -4  e+ 2 8.85e+3 Y     
    ##  73 mx30_grd                            4.81e+ 0 3.67e+3  0       8.85e+3 Y     
    ##  74 ndvi                                1.12e- 1 8.12e-1 -1  e+ 0 1   e+0 Y     
    ##  75 pblh                                3.56e+ 2 1.38e+3  5  e+ 1 2.5 e+3 Y     
    ##  76 pdsi                               -7.26e+ 0 9.49e+0 -1  e+ 1 2   e+1 Y     
    ##  77 pet                                 1.72e+ 0 1.86e+2  0       3.5 e+2 Y     
    ##  78 ppt                                 0        5.18e+2  0       1.4 e+3 Y     
    ##  79 pr                                  1.12e- 1 1.08e+1  0       5   e+1 Y     
    ##  80 precsno                             0        6.33e-5  0       7   e-5 Y     
    ##  81 prectotcorr                         2.17e- 6 1.32e-4  1  e- 7 5   e-4 Y     
    ##  82 prop_cover_huc2_01                  0        1   e+0  0       1   e+0 Y     
    ##  83 prop_cover_huc2_02                  0        1   e+0  0       1   e+0 Y     
    ##  84 prop_cover_huc2_03                  0        1   e+0  0       1   e+0 Y     
    ##  85 prop_cover_huc2_04                  0        1   e+0  0       1   e+0 Y     
    ##  86 prop_cover_huc2_05                  0        1   e+0  0       1   e+0 Y     
    ##  87 prop_cover_huc2_06                  0        1   e+0  0       1   e+0 Y     
    ##  88 prop_cover_huc2_07                  0        1   e+0  0       1   e+0 Y     
    ##  89 prop_cover_huc2_08                  0        1   e+0  0       1   e+0 Y     
    ##  90 prop_cover_huc2_09                  0        1   e+0  0       1   e+0 Y     
    ##  91 prop_cover_huc2_10                  0        1   e+0  0       1   e+0 Y     
    ##  92 prop_cover_huc2_11                  0        1   e+0  0       1   e+0 Y     
    ##  93 prop_cover_huc2_12                  0        1   e+0  0       1   e+0 Y     
    ##  94 prop_cover_huc2_13                  0        1   e+0  0       1   e+0 Y     
    ##  95 prop_cover_huc2_14                  0        1   e+0  0       1   e+0 Y     
    ##  96 prop_cover_huc2_15                  0        1   e+0  0       1   e+0 Y     
    ##  97 prop_cover_huc2_16                  0        1   e+0  0       1   e+0 Y     
    ##  98 prop_cover_huc2_17                  0        1   e+0  0       1   e+0 Y     
    ##  99 prop_cover_huc2_18                  0        1   e+0  0       1   e+0 Y     
    ## 100 prop_cover_huc2_20                  0        9.98e-1  0       1   e+0 Y     
    ## 101 prop_cover_huc2_21                  0        0        0       1   e+0 Y     
    ## 102 prop_cover_huc2_22                  0        0        0       1   e+0 Y     
    ## 103 prop_heavy_coverage                 0        1.43e-1  0       1   e+0 Y     
    ## 104 prop_light_coverage                 0        5.70e-1  0       1   e+0 Y     
    ## 105 prop_med_coverage                   0        1.97e-1  0       1   e+0 Y     
    ## 106 ps                                  6.85e+ 4 1.02e+5  5  e+ 4 1.05e+5 Y     
    ## 107 qv2m                                1.18e- 3 1.66e-2  0       4   e-2 Y     
    ## 108 rmax                                3.88e+ 1 9.96e+1  0       1   e+2 Y     
    ## 109 rmin                                1.16e+ 1 6.55e+1  0       1   e+2 Y     
    ## 110 road_density_km_per_km2             3.87e- 4 1.14e+0  0       9   e+0 Y     
    ## 111 sd30_grd                            8.36e- 1 9.36e+1  0       6   e+2 Y     
    ## 112 slp                                 1.00e+ 5 1.02e+5  9.9e+ 4 1.02e+5 Y     
    ## 113 soil                                1.72e- 2 3.06e+2  0       5   e+2 Y     
    ## 114 solclear                            4.86e+ 0 3.37e+1  2  e+ 0 3.5 e+1 Y     
    ## 115 solslope                            1.93e+ 0 2.92e+1  0       4   e+1 Y     
    ## 116 soltotal                            1.95e+ 0 2.92e+1  0       3.2 e+1 Y     
    ## 117 soltrans                            3.47e- 1 9.25e-1  0       1   e+0 Y     
    ## 118 sph                                 2.79e- 3 1.59e-2  0       3   e-2 Y     
    ## 119 srad                                7.31e+ 1 2.58e+2  0       4   e+2 Y     
    ## 120 sur_refl_b01                        3.42e- 2 5.55e-1 -1  e- 1 1   e+0 Y     
    ## 121 sur_refl_b02                        1.22e- 1 5.43e-1  0       1   e+0 Y     
    ## 122 sur_refl_b03                        2.00e- 2 5.73e-1  0       1   e+0 Y     
    ## 123 sur_refl_b04                        4.20e- 2 5.74e-1  0       1   e+0 Y     
    ## 124 sur_refl_b05                        1.24e- 1 4.15e-1  0       1   e+0 Y     
    ## 125 sur_refl_b06                        9.03e- 2 4.48e-1  0       1   e+0 Y     
    ## 126 sur_refl_b07                        4.08e- 2 3.77e-1  0       1   e+0 Y     
    ## 127 swe                                 0        3.05e+2  0       1.3 e+3 Y     
    ## 128 t2mdew                              2.54e+ 2 2.95e+2  2.3e+ 2 3.5 e+2 Y     
    ## 129 tdmean                             -1.77e+ 1 2.44e+1 -4  e+ 1 2.7 e+1 Y     
    ## 130 th                                  7.50e+ 1 2.80e+2  0       3.6 e+2 Y     
    ## 131 tmax                               -5.16e+ 0 3.26e+1 -5  e+ 1 5   e+1 Y     
    ## 132 tmax_norm                          -1.06e+ 1 4.15e+1 -5  e+ 1 5   e+1 Y     
    ## 133 tmean                              -1.55e+ 1 3.37e+1 -1.6e+ 1 4   e+1 Y     
    ## 134 tmin                               -1.32e+ 1 2.13e+1 -6  e+ 1 3.3 e+1 Y     
    ## 135 tmin_norm                          -2.07e+ 1 2.61e+1 -6  e+ 1 3   e+1 Y     
    ## 136 tmmn                                2.65e+ 2 2.94e+2  2.4e+ 2 3.5 e+2 Y     
    ## 137 tmmx                                2.79e+ 2 3.05e+2  2.4e+ 2 3.5 e+2 Y     
    ## 138 total_road_km                       1.40e+ 0 2.59e+3  0       6   e+4 Y     
    ## 139 totexttau                           5.04e- 2 2.57e-1  2  e- 2 3   e-1 Y     
    ## 140 ts                                  2.53e+ 2 3.00e+2  2.5e+ 2 3.05e+2 Y     
    ## 141 u10m                               -5.98e+ 0 4.41e+0 -2  e+ 1 2   e+1 Y     
    ## 142 vap                                 3.11e- 1 2.50e+0  0       5   e+0 Y     
    ## 143 vpd                                 1.56e- 1 2.70e+0  0       6   e+0 Y     
    ## 144 vpdmax                              7.57e- 1 6.52e+1  0       8   e+1 Y     
    ## 145 vpdmin                              1.41e- 1 1.87e+1  0       3   e+1 Y     
    ## 146 vs                                  2.33e+ 0 7.24e+0  0       2   e+1 Y     
    ## 147 ws                                  1.74e+ 0 7.76e+0  0       1.4 e+1 Y     
    ## 148 z0m                                 3.81e- 4 2.26e+0  1  e- 4 3   e+0 Y

# County-Monthly Sanity checks

``` r
# check for duplicated geoid-year-variable combinations
anyDuplicated(county_monthly[c("geoid", "year", "variable")])
```

    ## [1] 0

``` r
# Basic shape
# should be 3144 counties, 17 years (2010-2024) normal and static, 148 vars
county_monthly %>%
  summarise(
    n_rows     = n(),
    n_counties = n_distinct(geoid),
    n_years    = n_distinct(year),
    n_vars     = n_distinct(variable)
  ) %>%
  collect()
```

    ## # A tibble: 1 × 4
    ##     n_rows n_counties n_years n_vars
    ##      <int>      <int>   <int>  <int>
    ## 1 44900604       3144      17    148

``` r
# List distinct years and months in the dataset
county_monthly %>%
  distinct(year, month) %>%
  arrange(year, month) %>%
  collect() %>%
  print(n = Inf)
```

    ## # A tibble: 193 × 2
    ##     year   month
    ##     <chr>  <int>
    ##   1 2010       1
    ##   2 2010       2
    ##   3 2010       3
    ##   4 2010       4
    ##   5 2010       5
    ##   6 2010       6
    ##   7 2010       7
    ##   8 2010       8
    ##   9 2010       9
    ##  10 2010      10
    ##  11 2010      11
    ##  12 2010      12
    ##  13 2011       1
    ##  14 2011       2
    ##  15 2011       3
    ##  16 2011       4
    ##  17 2011       5
    ##  18 2011       6
    ##  19 2011       7
    ##  20 2011       8
    ##  21 2011       9
    ##  22 2011      10
    ##  23 2011      11
    ##  24 2011      12
    ##  25 2012       1
    ##  26 2012       2
    ##  27 2012       3
    ##  28 2012       4
    ##  29 2012       5
    ##  30 2012       6
    ##  31 2012       7
    ##  32 2012       8
    ##  33 2012       9
    ##  34 2012      10
    ##  35 2012      11
    ##  36 2012      12
    ##  37 2013       1
    ##  38 2013       2
    ##  39 2013       3
    ##  40 2013       4
    ##  41 2013       5
    ##  42 2013       6
    ##  43 2013       7
    ##  44 2013       8
    ##  45 2013       9
    ##  46 2013      10
    ##  47 2013      11
    ##  48 2013      12
    ##  49 2014       1
    ##  50 2014       2
    ##  51 2014       3
    ##  52 2014       4
    ##  53 2014       5
    ##  54 2014       6
    ##  55 2014       7
    ##  56 2014       8
    ##  57 2014       9
    ##  58 2014      10
    ##  59 2014      11
    ##  60 2014      12
    ##  61 2015       1
    ##  62 2015       2
    ##  63 2015       3
    ##  64 2015       4
    ##  65 2015       5
    ##  66 2015       6
    ##  67 2015       7
    ##  68 2015       8
    ##  69 2015       9
    ##  70 2015      10
    ##  71 2015      11
    ##  72 2015      12
    ##  73 2016       1
    ##  74 2016       2
    ##  75 2016       3
    ##  76 2016       4
    ##  77 2016       5
    ##  78 2016       6
    ##  79 2016       7
    ##  80 2016       8
    ##  81 2016       9
    ##  82 2016      10
    ##  83 2016      11
    ##  84 2016      12
    ##  85 2017       1
    ##  86 2017       2
    ##  87 2017       3
    ##  88 2017       4
    ##  89 2017       5
    ##  90 2017       6
    ##  91 2017       7
    ##  92 2017       8
    ##  93 2017       9
    ##  94 2017      10
    ##  95 2017      11
    ##  96 2017      12
    ##  97 2018       1
    ##  98 2018       2
    ##  99 2018       3
    ## 100 2018       4
    ## 101 2018       5
    ## 102 2018       6
    ## 103 2018       7
    ## 104 2018       8
    ## 105 2018       9
    ## 106 2018      10
    ## 107 2018      11
    ## 108 2018      12
    ## 109 2019       1
    ## 110 2019       2
    ## 111 2019       3
    ## 112 2019       4
    ## 113 2019       5
    ## 114 2019       6
    ## 115 2019       7
    ## 116 2019       8
    ## 117 2019       9
    ## 118 2019      10
    ## 119 2019      11
    ## 120 2019      12
    ## 121 2020       1
    ## 122 2020       2
    ## 123 2020       3
    ## 124 2020       4
    ## 125 2020       5
    ## 126 2020       6
    ## 127 2020       7
    ## 128 2020       8
    ## 129 2020       9
    ## 130 2020      10
    ## 131 2020      11
    ## 132 2020      12
    ## 133 2021       1
    ## 134 2021       2
    ## 135 2021       3
    ## 136 2021       4
    ## 137 2021       5
    ## 138 2021       6
    ## 139 2021       7
    ## 140 2021       8
    ## 141 2021       9
    ## 142 2021      10
    ## 143 2021      11
    ## 144 2021      12
    ## 145 2022       1
    ## 146 2022       2
    ## 147 2022       3
    ## 148 2022       4
    ## 149 2022       5
    ## 150 2022       6
    ## 151 2022       7
    ## 152 2022       8
    ## 153 2022       9
    ## 154 2022      10
    ## 155 2022      11
    ## 156 2022      12
    ## 157 2023       1
    ## 158 2023       2
    ## 159 2023       3
    ## 160 2023       4
    ## 161 2023       5
    ## 162 2023       6
    ## 163 2023       7
    ## 164 2023       8
    ## 165 2023       9
    ## 166 2023      10
    ## 167 2023      11
    ## 168 2023      12
    ## 169 2024       1
    ## 170 2024       2
    ## 171 2024       3
    ## 172 2024       4
    ## 173 2024       5
    ## 174 2024       6
    ## 175 2024       7
    ## 176 2024       8
    ## 177 2024       9
    ## 178 2024      10
    ## 179 2024      11
    ## 180 2024      12
    ## 181 normal     1
    ## 182 normal     2
    ## 183 normal     3
    ## 184 normal     4
    ## 185 normal     5
    ## 186 normal     6
    ## 187 normal     7
    ## 188 normal     8
    ## 189 normal     9
    ## 190 normal    10
    ## 191 normal    11
    ## 192 normal    12
    ## 193 static    NA

``` r
var_summary <- county_monthly %>%
  group_by(variable) %>%
  summarise(
    min_val   = min(value, na.rm = TRUE),
    max_val   = max(value, na.rm = TRUE),
    mean_val  = mean(value, na.rm = TRUE),
    median_val = median(value, na.rm = TRUE),
    sd_val    = sd(value, na.rm = TRUE)
  ) %>%
  arrange(variable) %>%
  collect()

print(var_summary, n = Inf)
```

    ## # A tibble: 148 × 6
    ##     variable                        min_val max_val mean_val median_val   sd_val
    ##     <chr>                             <dbl>   <dbl>    <dbl>      <dbl>    <dbl>
    ##   1 aet                            0        2.03e+2 6.46e+ 1   6.15e+ 1 4.37e+ 1
    ##   2 albedo                         5.83e- 2 6.10e-1 1.77e- 1   1.76e- 1 4.05e- 2
    ##   3 annual_total_air_lb            0        8.91e+6 1.24e+ 5   1.43e+ 4 3.73e+ 5
    ##   4 annual_total_air_lb_per_km2    0        9.16e+4 1.34e+ 2   8.03e+ 0 1.79e+ 3
    ##   5 annual_total_air_lb_plusbuff…  0        1.14e+7 4.36e+ 5   1.53e+ 5 7.99e+ 5
    ##   6 annual_total_fugitive_air_lb   0        8.48e+6 2.87e+ 4   1.33e+ 3 1.69e+ 5
    ##   7 annual_total_fugitive_air_lb…  0        1.54e+4 2.22e+ 1   7.51e- 1 1.95e+ 2
    ##   8 annual_total_fugitive_air_lb…  0        8.52e+6 9.80e+ 4   2.33e+ 4 3.09e+ 5
    ##   9 annual_total_stack_air_lb      0        5.73e+6 9.53e+ 4   8.92e+ 3 2.86e+ 5
    ##  10 annual_total_stack_air_lb_pe…  0        9.16e+4 1.12e+ 2   4.89e+ 0 1.69e+ 3
    ##  11 annual_total_stack_air_lb_pl…  0        9.68e+6 3.38e+ 5   1.13e+ 5 6.29e+ 5
    ##  12 area_km2                       5.29e+ 0 3.83e+5 2.69e+ 3   1.58e+ 3 6.10e+ 3
    ##  13 bcsmass                        1.77e-11 3.27e-9 4.56e-10   4.25e-10 2.16e-10
    ##  14 be30_grd                       8.68e- 1 3.45e+3 4.40e+ 2   2.78e+ 2 5.03e+ 2
    ##  15 cldtot                         2.00e- 1 7.83e-1 4.78e- 1   4.85e- 1 7.25e- 2
    ##  16 def                            0        3.00e+2 2.91e+ 1   3.21e+ 0 4.76e+ 1
    ##  17 ds30_grd                       8.16e- 1 3.48e+3 4.43e+ 2   2.81e+ 2 5.06e+ 2
    ##  18 dusmass25                      2.32e-10 1.49e-8 1.66e- 9   1.52e- 9 7.43e-10
    ##  19 etr                            1.11e- 1 1.71e+1 4.73e+ 0   4.78e+ 0 2.32e+ 0
    ##  20 evap                           8.79e- 7 7.15e-5 2.60e- 5   2.47e- 5 1.31e- 5
    ##  21 evi                           -5.24e- 2 7.95e-1 3.14e- 1   2.77e- 1 1.53e- 1
    ##  22 fractional_impervious_surface  7.67e- 2 6.27e+1 3.28e+ 0   1.46e+ 0 6.19e+ 0
    ##  23 grn                            0        9.81e-1 8.81e- 1   8.97e- 1 8.48e- 2
    ##  24 gwetroot                       1.69e- 1 9.47e-1 5.91e- 1   6.14e- 1 1.30e- 1
    ##  25 koppen_1                       2.36e- 2 6.09e-1 2.45e- 1   1.74e- 1 1.91e- 1
    ##  26 koppen_14                      9.20e- 5 1   e+0 9.19e- 1   1   e+ 0 2.28e- 1
    ##  27 koppen_15                      5.20e- 5 5.97e-1 1.64e- 1   9.97e- 2 1.75e- 1
    ##  28 koppen_16                      4.38e- 4 3.76e-2 1.47e- 2   7.50e- 3 1.62e- 2
    ##  29 koppen_17                      5.31e- 3 2.83e-1 6.36e- 2   2.95e- 2 8.14e- 2
    ##  30 koppen_18                      1.62e- 4 1   e+0 2.44e- 1   1.33e- 1 2.58e- 1
    ##  31 koppen_19                      4.90e- 8 4.81e-1 9.98e- 2   6.36e- 2 1.10e- 1
    ##  32 koppen_2                       1.55e- 3 6.40e-1 1.83e- 1   1.48e- 1 1.99e- 1
    ##  33 koppen_21                      1.57e- 4 9.98e-1 1.92e- 1   9.69e- 2 2.30e- 1
    ##  34 koppen_22                      2.11e- 3 9.19e-1 2.97e- 1   1.05e- 1 3.25e- 1
    ##  35 koppen_23                      2.62e- 4 2.04e-1 6.94e- 2   3.82e- 3 1.17e- 1
    ##  36 koppen_25                      1.70e- 4 1   e+0 8.27e- 1   1   e+ 0 3.13e- 1
    ##  37 koppen_26                      6.87e- 8 1   e+0 5.26e- 1   4.84e- 1 4.04e- 1
    ##  38 koppen_27                      5.51e- 5 1   e+0 2.74e- 1   1.82e- 1 2.71e- 1
    ##  39 koppen_29                      2.41e- 5 9.57e-1 1.56e- 1   5.16e- 2 1.97e- 1
    ##  40 koppen_3                       2.97e- 2 1   e+0 5.84e- 1   6.91e- 1 2.93e- 1
    ##  41 koppen_30                      7.85e- 5 4.52e-3 2.16e- 3   1.88e- 3 2.24e- 3
    ##  42 koppen_4                       1.86e- 6 1   e+0 3.45e- 1   2.08e- 1 3.51e- 1
    ##  43 koppen_5                       3.98e- 5 9.70e-1 2.98e- 1   1.91e- 1 2.81e- 1
    ##  44 koppen_6                       1.61e- 3 1   e+0 4.72e- 1   3.35e- 1 4.08e- 1
    ##  45 koppen_7                       9.93e- 5 1   e+0 6.18e- 1   6.72e- 1 3.47e- 1
    ##  46 koppen_8                       5.65e- 5 1   e+0 3.32e- 1   2.20e- 1 3.26e- 1
    ##  47 koppen_9                       2.23e- 3 1   e+0 4.70e- 1   3.48e- 1 3.94e- 1
    ##  48 koppen_confidence              4.46e+ 1 1   e+2 9.32e+ 1   9.99e+ 1 1.05e+ 1
    ##  49 lai                            1.00e-20 6.99e+0 1.94e+ 0   1.83e+ 0 8.92e- 1
    ##  50 land_cover_11                  0        5.75e-1 2.20e- 2   9.91e- 3 3.83e- 2
    ##  51 land_cover_12                  0        2.53e-2 3.93e- 5   0        6.98e- 4
    ##  52 land_cover_21                  0        3.62e-1 4.73e- 2   3.89e- 2 3.95e- 2
    ##  53 land_cover_22                  0        3.71e-1 3.67e- 2   2.05e- 2 4.90e- 2
    ##  54 land_cover_23                  0        4.85e-1 1.75e- 2   4.69e- 3 4.38e- 2
    ##  55 land_cover_24                  0        4.20e-1 5.56e- 3   8.67e- 4 2.23e- 2
    ##  56 land_cover_31                  0        3.94e-1 4.81e- 3   1.47e- 3 1.60e- 2
    ##  57 land_cover_41                  0        9.14e-1 1.57e- 1   5.85e- 2 2.01e- 1
    ##  58 land_cover_42                  0        8.82e-1 8.72e- 2   9.83e- 3 1.43e- 1
    ##  59 land_cover_43                  0        4.25e-1 4.09e- 2   5.95e- 3 6.69e- 2
    ##  60 land_cover_52                  0        9.91e-1 8.42e- 2   3.92e- 3 1.97e- 1
    ##  61 land_cover_71                  0        9.72e-1 8.60e- 2   1.13e- 2 1.73e- 1
    ##  62 land_cover_81                  0        8.39e-1 1.08e- 1   5.82e- 2 1.30e- 1
    ##  63 land_cover_82                  0        9.15e-1 2.16e- 1   7.89e- 2 2.62e- 1
    ##  64 land_cover_90                  0        7.37e-1 6.15e- 2   1.13e- 2 1.02e- 1
    ##  65 land_cover_95                  0        6.22e-1 1.43e- 2   2.92e- 3 4.14e- 2
    ##  66 land_cover_confidence          0        8.70e+1 6.15e+ 1   6.11e+ 1 1.12e+ 1
    ##  67 lst_day_1km_k                  2.47e+ 2 3.28e+2 2.92e+ 2   2.95e+ 2 1.18e+ 1
    ##  68 lst_night_1km_k                2.40e+ 2 3.06e+2 2.81e+ 2   2.82e+ 2 1.01e+ 1
    ##  69 lwgab                          2.02e+ 2 3.94e+2 3.09e+ 2   3.08e+ 2 2.84e+ 1
    ##  70 md30_grd                       6.34e- 1 3.47e+3 4.43e+ 2   2.80e+ 2 5.06e+ 2
    ##  71 mi30_grd                      -1.71e+ 0 3.31e+3 4.19e+ 2   2.59e+ 2 4.93e+ 2
    ##  72 mn30_grd                       7.04e- 1 3.48e+3 4.43e+ 2   2.80e+ 2 5.06e+ 2
    ##  73 mx30_grd                       4.81e+ 0 3.67e+3 4.70e+ 2   3.04e+ 2 5.20e+ 2
    ##  74 ndvi                          -4.93e- 2 9.15e-1 5.25e- 1   5.27e- 1 2.06e- 1
    ##  75 pblh                           2.11e+ 2 1.60e+3 7.44e+ 2   7.30e+ 2 1.93e+ 2
    ##  76 pdsi                          -8.74e+ 0 1.63e+1 7.41e- 1   1.21e+ 0 2.99e+ 0
    ##  77 pet                            0        3.08e+2 4.88e+ 1   5.87e+ 0 6.08e+ 1
    ##  78 ppt                            0        8.50e+2 8.52e+ 1   7.77e+ 1 5.90e+ 1
    ##  79 pr                             0        4.05e+1 2.89e+ 0   2.51e+ 0 2.17e+ 0
    ##  80 precsno                        0        6.89e-5 3.52e- 6   2.66e- 6 3.92e- 6
    ##  81 prectotcorr                    1.01e- 6 1.36e-4 3.19e- 5   3.19e- 5 1.33e- 5
    ##  82 prop_cover_huc2_01             0        1   e+0 1.87e- 2   0        1.33e- 1
    ##  83 prop_cover_huc2_02             0        1   e+0 7.11e- 2   0        2.52e- 1
    ##  84 prop_cover_huc2_03             0        1   e+0 1.52e- 1   0        3.55e- 1
    ##  85 prop_cover_huc2_04             0        1   e+0 5.70e- 2   0        2.23e- 1
    ##  86 prop_cover_huc2_05             0        1   e+0 1.23e- 1   0        3.22e- 1
    ##  87 prop_cover_huc2_06             0        1   e+0 2.92e- 2   0        1.60e- 1
    ##  88 prop_cover_huc2_07             0        1   e+0 9.22e- 2   0        2.80e- 1
    ##  89 prop_cover_huc2_08             0        1   e+0 5.02e- 2   0        2.11e- 1
    ##  90 prop_cover_huc2_09             0        1   e+0 1.40e- 2   0        1.11e- 1
    ##  91 prop_cover_huc2_10             0        1   e+0 1.33e- 1   0        3.33e- 1
    ##  92 prop_cover_huc2_11             0        1   e+0 8.46e- 2   0        2.70e- 1
    ##  93 prop_cover_huc2_12             0        1   e+0 5.95e- 2   0        2.31e- 1
    ##  94 prop_cover_huc2_13             0        1   e+0 1.51e- 2   0        1.14e- 1
    ##  95 prop_cover_huc2_14             0        1   e+0 1.17e- 2   0        1.02e- 1
    ##  96 prop_cover_huc2_15             0        1   e+0 6.40e- 3   0        7.39e- 2
    ##  97 prop_cover_huc2_16             0        1   e+0 1.19e- 2   0        1.02e- 1
    ##  98 prop_cover_huc2_17             0        1   e+0 4.03e- 2   0        1.95e- 1
    ##  99 prop_cover_huc2_18             0        1   e+0 1.82e- 2   0        1.31e- 1
    ## 100 prop_cover_huc2_20             0        9.98e-1 1.57e- 3   0        3.93e- 2
    ## 101 prop_cover_huc2_21             0        0       0          0        0       
    ## 102 prop_cover_huc2_22             0        0       0          0        0       
    ## 103 prop_heavy_coverage            0        8.76e-1 8.51e- 3   0        3.74e- 2
    ## 104 prop_light_coverage            0        9.67e-1 1.26e- 1   3.13e- 2 1.89e- 1
    ## 105 prop_med_coverage              0        7.52e-1 2.81e- 2   0        7.26e- 2
    ## 106 ps                             6.85e+ 4 1.02e+5 9.66e+ 4   9.83e+ 4 5.42e+ 3
    ## 107 qv2m                           1.18e- 3 1.67e-2 7.89e- 3   7.72e- 3 2.28e- 3
    ## 108 rmax                           1.87e+ 1 1.00e+2 8.65e+ 1   8.80e+ 1 9.17e+ 0
    ## 109 rmin                           2.92e+ 0 8.77e+1 4.21e+ 1   4.31e+ 1 1.07e+ 1
    ## 110 road_density_km_per_km2        3.87e- 4 1.14e+0 1.51e- 1   1.25e- 1 1.16e- 1
    ## 111 sd30_grd                       8.36e- 1 9.36e+1 1.14e+ 1   6.26e+ 0 1.25e+ 1
    ## 112 slp                            1.00e+ 5 1.02e+5 1.02e+ 5   1.02e+ 5 1.81e+ 2
    ## 113 soil                           1.72e- 2 4.04e+2 7.33e+ 1   6.81e+ 1 5.70e+ 1
    ## 114 solclear                       4.86e+ 0 3.37e+1 2.11e+ 1   2.18e+ 1 7.30e+ 0
    ## 115 solslope                       1.93e+ 0 2.92e+1 1.48e+ 1   1.52e+ 1 5.93e+ 0
    ## 116 soltotal                       1.95e+ 0 2.92e+1 1.48e+ 1   1.52e+ 1 5.93e+ 0
    ## 117 soltrans                       3.47e- 1 9.25e-1 6.88e- 1   6.98e- 1 7.55e- 2
    ## 118 sph                            6.78e- 4 2.09e-2 7.39e- 3   6.30e- 3 4.30e- 3
    ## 119 srad                           8.87e- 2 3.66e+2 1.83e+ 2   1.87e+ 2 7.46e+ 1
    ## 120 sur_refl_b01                   1.69e- 2 9.35e-1 1.19e- 1   8.24e- 2 1.16e- 1
    ## 121 sur_refl_b02                   7.65e- 2 8.77e-1 2.88e- 1   2.70e- 1 9.13e- 2
    ## 122 sur_refl_b03                   1.01e- 2 9.45e-1 8.53e- 2   4.72e- 2 1.18e- 1
    ## 123 sur_refl_b04                   2.51e- 2 9.49e-1 1.14e- 1   7.85e- 2 1.13e- 1
    ## 124 sur_refl_b05                   8.66e- 2 5.52e-1 3.01e- 1   3.02e- 1 4.99e- 2
    ## 125 sur_refl_b06                   5.17e- 2 4.95e-1 2.28e- 1   2.18e- 1 5.96e- 2
    ## 126 sur_refl_b07                   2.59e- 2 4.21e-1 1.30e- 1   1.16e- 1 5.56e- 2
    ## 127 swe                            0        1.01e+3 1.27e+ 1   0        4.14e+ 1
    ## 128 t2mdew                         2.54e+ 2 2.95e+2 2.80e+ 2   2.80e+ 2 5.23e+ 0
    ## 129 tdmean                        -1.77e+ 1 2.44e+1 6.35e+ 0   6.18e+ 0 9.29e+ 0
    ## 130 th                             1.84e+ 1 3.32e+2 1.88e+ 2   1.90e+ 2 3.58e+ 1
    ## 131 tmax                          -2.70e+ 1 4.45e+1 1.95e+ 1   2.10e+ 1 1.03e+ 1
    ## 132 tmax_norm                     -1.06e+ 1 4.15e+1 1.88e+ 1   2.01e+ 1 1.00e+ 1
    ## 133 tmean                         -1.55e+ 1 3.37e+1 1.26e+ 1   1.34e+ 1 9.68e+ 0
    ## 134 tmin                          -3.66e+ 1 2.93e+1 6.90e+ 0   7.26e+ 0 9.69e+ 0
    ## 135 tmin_norm                     -2.07e+ 1 2.61e+1 6.51e+ 0   6.69e+ 0 9.45e+ 0
    ## 136 tmmn                           2.46e+ 2 3.03e+2 2.80e+ 2   2.80e+ 2 9.62e+ 0
    ## 137 tmmx                           2.57e+ 2 3.17e+2 2.93e+ 2   2.94e+ 2 1.03e+ 1
    ## 138 total_road_km                  1.40e+ 0 2.59e+3 2.50e+ 2   2.06e+ 2 1.79e+ 2
    ## 139 totexttau                      5.01e- 2 2.87e-1 1.48e- 1   1.54e- 1 3.10e- 2
    ## 140 ts                             2.53e+ 2 3.02e+2 2.86e+ 2   2.86e+ 2 5.33e+ 0
    ## 141 u10m                          -6.35e+ 0 4.77e+0 5.28e- 1   5.42e- 1 7.15e- 1
    ## 142 vap                            5.68e- 3 3.25e+0 1.16e+ 0   9.73e- 1 6.87e- 1
    ## 143 vpd                            0        5.52e+0 7.68e- 1   7.03e- 1 5.00e- 1
    ## 144 vpdmax                         7.57e- 1 6.52e+1 1.44e+ 1   1.39e+ 1 8.37e+ 0
    ## 145 vpdmin                         1.41e- 1 1.87e+1 1.08e+ 0   7.98e- 1 1.08e+ 0
    ## 146 vs                             1.03e+ 0 1.01e+1 4.11e+ 0   4.09e+ 0 8.06e- 1
    ## 147 ws                             1.02e+ 0 1.24e+1 3.41e+ 0   3.31e+ 0 9.96e- 1
    ## 148 z0m                            3.73e- 4 2.26e+0 7.29e- 1   3.18e- 1 7.79e- 1

``` r
# Compare observed vs expected
comparison <- var_summary %>%
  left_join(expected_ranges, by = "variable") %>%
  mutate(
    min_ok = min_val >= min_exp,
    max_ok = max_val <= max_exp,
    passes = ifelse(min_ok & max_ok, "Y", "N")
  ) %>%
  select(variable, min_val, max_val, min_exp, max_exp, passes)

print(comparison, n = Inf)
```

    ## # A tibble: 148 × 6
    ##     variable                             min_val max_val  min_exp max_exp passes
    ##     <chr>                                  <dbl>   <dbl>    <dbl>   <dbl> <chr> 
    ##   1 aet                                 0        2.03e+2  0       2.5 e+2 Y     
    ##   2 albedo                              5.83e- 2 6.10e-1  0       9   e-1 Y     
    ##   3 annual_total_air_lb                 0        8.91e+6  0       1   e+8 Y     
    ##   4 annual_total_air_lb_per_km2         0        9.16e+4  0       1   e+6 Y     
    ##   5 annual_total_air_lb_plusbuffer      0        1.14e+7  0       1   e+8 Y     
    ##   6 annual_total_fugitive_air_lb        0        8.48e+6  0       1   e+8 Y     
    ##   7 annual_total_fugitive_air_lb_per_…  0        1.54e+4  0       5   e+5 Y     
    ##   8 annual_total_fugitive_air_lb_plus…  0        8.52e+6  0       1   e+8 Y     
    ##   9 annual_total_stack_air_lb           0        5.73e+6  0       1   e+8 Y     
    ##  10 annual_total_stack_air_lb_per_km2   0        9.16e+4  0       1   e+6 Y     
    ##  11 annual_total_stack_air_lb_plusbuf…  0        9.68e+6  0       1   e+8 Y     
    ##  12 area_km2                            5.29e+ 0 3.83e+5  5  e+ 0 3.83e+5 Y     
    ##  13 bcsmass                             1.77e-11 3.27e-9  1  e-11 1   e-8 Y     
    ##  14 be30_grd                            8.68e- 1 3.45e+3  0       6   e+3 Y     
    ##  15 cldtot                              2.00e- 1 7.83e-1  0       1   e+0 Y     
    ##  16 def                                 0        3.00e+2  0       3.4 e+2 Y     
    ##  17 ds30_grd                            8.16e- 1 3.48e+3  0       6   e+3 Y     
    ##  18 dusmass25                           2.32e-10 1.49e-8  1  e-11 3   e-8 Y     
    ##  19 etr                                 1.11e- 1 1.71e+1  0       1.8 e+1 Y     
    ##  20 evap                                8.79e- 7 7.15e-5  1  e- 8 1   e-4 Y     
    ##  21 evi                                -5.24e- 2 7.95e-1 -1  e- 1 1   e+0 Y     
    ##  22 fractional_impervious_surface       7.67e- 2 6.27e+1  0       1   e+2 Y     
    ##  23 grn                                 0        9.81e-1  0       1   e+0 Y     
    ##  24 gwetroot                            1.69e- 1 9.47e-1  0       1   e+0 Y     
    ##  25 koppen_1                            2.36e- 2 6.09e-1  0       1   e+0 Y     
    ##  26 koppen_14                           9.20e- 5 1   e+0  0       1   e+0 Y     
    ##  27 koppen_15                           5.20e- 5 5.97e-1  0       1   e+0 Y     
    ##  28 koppen_16                           4.38e- 4 3.76e-2  0       1   e+0 Y     
    ##  29 koppen_17                           5.31e- 3 2.83e-1  0       1   e+0 Y     
    ##  30 koppen_18                           1.62e- 4 1   e+0  0       1   e+0 Y     
    ##  31 koppen_19                           4.90e- 8 4.81e-1  0       1   e+0 Y     
    ##  32 koppen_2                            1.55e- 3 6.40e-1  0       1   e+0 Y     
    ##  33 koppen_21                           1.57e- 4 9.98e-1  0       1   e+0 Y     
    ##  34 koppen_22                           2.11e- 3 9.19e-1  0       1   e+0 Y     
    ##  35 koppen_23                           2.62e- 4 2.04e-1  0       1   e+0 Y     
    ##  36 koppen_25                           1.70e- 4 1   e+0  0       1   e+0 Y     
    ##  37 koppen_26                           6.87e- 8 1   e+0  0       1   e+0 Y     
    ##  38 koppen_27                           5.51e- 5 1   e+0  0       1   e+0 Y     
    ##  39 koppen_29                           2.41e- 5 9.57e-1  0       1   e+0 Y     
    ##  40 koppen_3                            2.97e- 2 1   e+0  0       1   e+0 Y     
    ##  41 koppen_30                           7.85e- 5 4.52e-3  0       1   e+0 Y     
    ##  42 koppen_4                            1.86e- 6 1   e+0  0       1   e+0 Y     
    ##  43 koppen_5                            3.98e- 5 9.70e-1  0       1   e+0 Y     
    ##  44 koppen_6                            1.61e- 3 1   e+0  0       1   e+0 Y     
    ##  45 koppen_7                            9.93e- 5 1   e+0  0       1   e+0 Y     
    ##  46 koppen_8                            5.65e- 5 1   e+0  0       1   e+0 Y     
    ##  47 koppen_9                            2.23e- 3 1   e+0  0       1   e+0 Y     
    ##  48 koppen_confidence                   4.46e+ 1 1   e+2  0       1   e+2 Y     
    ##  49 lai                                 1.00e-20 6.99e+0  0       9   e+0 Y     
    ##  50 land_cover_11                       0        5.75e-1  0       1   e+0 Y     
    ##  51 land_cover_12                       0        2.53e-2  0       1   e+0 Y     
    ##  52 land_cover_21                       0        3.62e-1  0       1   e+0 Y     
    ##  53 land_cover_22                       0        3.71e-1  0       1   e+0 Y     
    ##  54 land_cover_23                       0        4.85e-1  0       1   e+0 Y     
    ##  55 land_cover_24                       0        4.20e-1  0       1   e+0 Y     
    ##  56 land_cover_31                       0        3.94e-1  0       1   e+0 Y     
    ##  57 land_cover_41                       0        9.14e-1  0       1   e+0 Y     
    ##  58 land_cover_42                       0        8.82e-1  0       1   e+0 Y     
    ##  59 land_cover_43                       0        4.25e-1  0       1   e+0 Y     
    ##  60 land_cover_52                       0        9.91e-1  0       1   e+0 Y     
    ##  61 land_cover_71                       0        9.72e-1  0       1   e+0 Y     
    ##  62 land_cover_81                       0        8.39e-1  0       1   e+0 Y     
    ##  63 land_cover_82                       0        9.15e-1  0       1   e+0 Y     
    ##  64 land_cover_90                       0        7.37e-1  0       1   e+0 Y     
    ##  65 land_cover_95                       0        6.22e-1  0       1   e+0 Y     
    ##  66 land_cover_confidence               0        8.70e+1  0       1   e+2 Y     
    ##  67 lst_day_1km_k                       2.47e+ 2 3.28e+2  2.3e+ 2 4   e+2 Y     
    ##  68 lst_night_1km_k                     2.40e+ 2 3.06e+2  2.3e+ 2 3.1 e+2 Y     
    ##  69 lwgab                               2.02e+ 2 3.94e+2  2  e+ 2 4   e+2 Y     
    ##  70 md30_grd                            6.34e- 1 3.47e+3 -4  e+ 2 8.85e+3 Y     
    ##  71 mi30_grd                           -1.71e+ 0 3.31e+3 -4  e+ 2 8.85e+3 Y     
    ##  72 mn30_grd                            7.04e- 1 3.48e+3 -4  e+ 2 8.85e+3 Y     
    ##  73 mx30_grd                            4.81e+ 0 3.67e+3  0       8.85e+3 Y     
    ##  74 ndvi                               -4.93e- 2 9.15e-1 -1  e+ 0 1   e+0 Y     
    ##  75 pblh                                2.11e+ 2 1.60e+3  5  e+ 1 2.5 e+3 Y     
    ##  76 pdsi                               -8.74e+ 0 1.63e+1 -1  e+ 1 2   e+1 Y     
    ##  77 pet                                 0        3.08e+2  0       3.5 e+2 Y     
    ##  78 ppt                                 0        8.50e+2  0       1.4 e+3 Y     
    ##  79 pr                                  0        4.05e+1  0       5   e+1 Y     
    ##  80 precsno                             0        6.89e-5  0       7   e-5 Y     
    ##  81 prectotcorr                         1.01e- 6 1.36e-4  1  e- 7 5   e-4 Y     
    ##  82 prop_cover_huc2_01                  0        1   e+0  0       1   e+0 Y     
    ##  83 prop_cover_huc2_02                  0        1   e+0  0       1   e+0 Y     
    ##  84 prop_cover_huc2_03                  0        1   e+0  0       1   e+0 Y     
    ##  85 prop_cover_huc2_04                  0        1   e+0  0       1   e+0 Y     
    ##  86 prop_cover_huc2_05                  0        1   e+0  0       1   e+0 Y     
    ##  87 prop_cover_huc2_06                  0        1   e+0  0       1   e+0 Y     
    ##  88 prop_cover_huc2_07                  0        1   e+0  0       1   e+0 Y     
    ##  89 prop_cover_huc2_08                  0        1   e+0  0       1   e+0 Y     
    ##  90 prop_cover_huc2_09                  0        1   e+0  0       1   e+0 Y     
    ##  91 prop_cover_huc2_10                  0        1   e+0  0       1   e+0 Y     
    ##  92 prop_cover_huc2_11                  0        1   e+0  0       1   e+0 Y     
    ##  93 prop_cover_huc2_12                  0        1   e+0  0       1   e+0 Y     
    ##  94 prop_cover_huc2_13                  0        1   e+0  0       1   e+0 Y     
    ##  95 prop_cover_huc2_14                  0        1   e+0  0       1   e+0 Y     
    ##  96 prop_cover_huc2_15                  0        1   e+0  0       1   e+0 Y     
    ##  97 prop_cover_huc2_16                  0        1   e+0  0       1   e+0 Y     
    ##  98 prop_cover_huc2_17                  0        1   e+0  0       1   e+0 Y     
    ##  99 prop_cover_huc2_18                  0        1   e+0  0       1   e+0 Y     
    ## 100 prop_cover_huc2_20                  0        9.98e-1  0       1   e+0 Y     
    ## 101 prop_cover_huc2_21                  0        0        0       1   e+0 Y     
    ## 102 prop_cover_huc2_22                  0        0        0       1   e+0 Y     
    ## 103 prop_heavy_coverage                 0        8.76e-1  0       1   e+0 Y     
    ## 104 prop_light_coverage                 0        9.67e-1  0       1   e+0 Y     
    ## 105 prop_med_coverage                   0        7.52e-1  0       1   e+0 Y     
    ## 106 ps                                  6.85e+ 4 1.02e+5  5  e+ 4 1.05e+5 Y     
    ## 107 qv2m                                1.18e- 3 1.67e-2  0       4   e-2 Y     
    ## 108 rmax                                1.87e+ 1 1.00e+2  0       1   e+2 N     
    ## 109 rmin                                2.92e+ 0 8.77e+1  0       1   e+2 Y     
    ## 110 road_density_km_per_km2             3.87e- 4 1.14e+0  0       9   e+0 Y     
    ## 111 sd30_grd                            8.36e- 1 9.36e+1  0       6   e+2 Y     
    ## 112 slp                                 1.00e+ 5 1.02e+5  9.9e+ 4 1.02e+5 Y     
    ## 113 soil                                1.72e- 2 4.04e+2  0       5   e+2 Y     
    ## 114 solclear                            4.86e+ 0 3.37e+1  2  e+ 0 3.5 e+1 Y     
    ## 115 solslope                            1.93e+ 0 2.92e+1  0       4   e+1 Y     
    ## 116 soltotal                            1.95e+ 0 2.92e+1  0       3.2 e+1 Y     
    ## 117 soltrans                            3.47e- 1 9.25e-1  0       1   e+0 Y     
    ## 118 sph                                 6.78e- 4 2.09e-2  0       3   e-2 Y     
    ## 119 srad                                8.87e- 2 3.66e+2  0       4   e+2 Y     
    ## 120 sur_refl_b01                        1.69e- 2 9.35e-1 -1  e- 1 1   e+0 Y     
    ## 121 sur_refl_b02                        7.65e- 2 8.77e-1  0       1   e+0 Y     
    ## 122 sur_refl_b03                        1.01e- 2 9.45e-1  0       1   e+0 Y     
    ## 123 sur_refl_b04                        2.51e- 2 9.49e-1  0       1   e+0 Y     
    ## 124 sur_refl_b05                        8.66e- 2 5.52e-1  0       1   e+0 Y     
    ## 125 sur_refl_b06                        5.17e- 2 4.95e-1  0       1   e+0 Y     
    ## 126 sur_refl_b07                        2.59e- 2 4.21e-1  0       1   e+0 Y     
    ## 127 swe                                 0        1.01e+3  0       1.3 e+3 Y     
    ## 128 t2mdew                              2.54e+ 2 2.95e+2  2.3e+ 2 3.5 e+2 Y     
    ## 129 tdmean                             -1.77e+ 1 2.44e+1 -4  e+ 1 2.7 e+1 Y     
    ## 130 th                                  1.84e+ 1 3.32e+2  0       3.6 e+2 Y     
    ## 131 tmax                               -2.70e+ 1 4.45e+1 -5  e+ 1 5   e+1 Y     
    ## 132 tmax_norm                          -1.06e+ 1 4.15e+1 -5  e+ 1 5   e+1 Y     
    ## 133 tmean                              -1.55e+ 1 3.37e+1 -1.6e+ 1 4   e+1 Y     
    ## 134 tmin                               -3.66e+ 1 2.93e+1 -6  e+ 1 3.3 e+1 Y     
    ## 135 tmin_norm                          -2.07e+ 1 2.61e+1 -6  e+ 1 3   e+1 Y     
    ## 136 tmmn                                2.46e+ 2 3.03e+2  2.4e+ 2 3.5 e+2 Y     
    ## 137 tmmx                                2.57e+ 2 3.17e+2  2.4e+ 2 3.5 e+2 Y     
    ## 138 total_road_km                       1.40e+ 0 2.59e+3  0       6   e+4 Y     
    ## 139 totexttau                           5.01e- 2 2.87e-1  2  e- 2 3   e-1 Y     
    ## 140 ts                                  2.53e+ 2 3.02e+2  2.5e+ 2 3.05e+2 Y     
    ## 141 u10m                               -6.35e+ 0 4.77e+0 -2  e+ 1 2   e+1 Y     
    ## 142 vap                                 5.68e- 3 3.25e+0  0       5   e+0 Y     
    ## 143 vpd                                 0        5.52e+0  0       6   e+0 Y     
    ## 144 vpdmax                              7.57e- 1 6.52e+1  0       8   e+1 Y     
    ## 145 vpdmin                              1.41e- 1 1.87e+1  0       3   e+1 Y     
    ## 146 vs                                  1.03e+ 0 1.01e+1  0       2   e+1 Y     
    ## 147 ws                                  1.02e+ 0 1.24e+1  0       1.4 e+1 Y     
    ## 148 z0m                                 3.73e- 4 2.26e+0  1  e- 4 3   e+0 Y

# Tract-Annual Sanity checks

``` r
# check for duplicated geoid-year-variable combinations
anyDuplicated(tract_annual[c("geoid", "year", "variable")])
```

    ## [1] 0

``` r
# Basic shape
# should be over 84,119 counties, 17 years (2010-2024) normal and static, 148 vars
# 84119 census tracts - some may have had errors
tract_annual %>%
  summarise(
    n_rows     = n(),
    n_tracts = n_distinct(geoid),
    n_years    = n_distinct(year),
    n_vars     = n_distinct(variable)
  ) %>%
  collect()
```

    ## # A tibble: 1 × 4
    ##      n_rows n_tracts n_years n_vars
    ##       <int>    <int>   <int>  <int>
    ## 1 106549609    84119      17    148

``` r
# List distinct years in the dataset
tract_annual %>%
  distinct(year) %>%
  arrange(year) %>%
  collect()
```

    ## # A tibble: 17 × 1
    ##    year  
    ##    <chr> 
    ##  1 2010  
    ##  2 2011  
    ##  3 2012  
    ##  4 2013  
    ##  5 2014  
    ##  6 2015  
    ##  7 2016  
    ##  8 2017  
    ##  9 2018  
    ## 10 2019  
    ## 11 2020  
    ## 12 2021  
    ## 13 2022  
    ## 14 2023  
    ## 15 2024  
    ## 16 normal
    ## 17 static

``` r
var_summary <- tract_annual %>%
  group_by(variable) %>%
  summarise(
    min_val   = min(value, na.rm = TRUE),
    max_val   = max(value, na.rm = TRUE),
    mean_val  = mean(value, na.rm = TRUE),
    median_val = median(value, na.rm = TRUE),
    sd_val    = sd(value, na.rm = TRUE)
  ) %>%
  arrange(variable) %>%
  collect()


print(var_summary, n = Inf)
```

    ## # A tibble: 148 × 6
    ##     variable                        min_val max_val mean_val median_val   sd_val
    ##     <chr>                             <dbl>   <dbl>    <dbl>      <dbl>    <dbl>
    ##   1 aet                            1.70e+ 0 1.31e+2 6.46e+ 1   6.75e+ 1 2.49e+ 1
    ##   2 albedo                         7.46e- 2 6.06e-1 1.62e- 1   1.62e- 1 3.49e- 2
    ##   3 annual_total_air_lb            0        8.91e+6 2.37e+ 4   1.08e+ 2 1.43e+ 5
    ##   4 annual_total_air_lb_per_km2    0        8.30e+5 1.22e+ 3   4.24e+ 0 9.69e+ 3
    ##   5 annual_total_air_lb_plusbuff…  0        8.91e+6 6.37e+ 4   6.34e+ 3 2.29e+ 5
    ##   6 annual_total_fugitive_air_lb   0        8.48e+6 5.49e+ 3   3.78e+ 0 7.07e+ 4
    ##   7 annual_total_fugitive_air_lb…  0        2.66e+5 2.83e+ 2   1.63e- 1 2.61e+ 3
    ##   8 annual_total_fugitive_air_lb…  0        8.48e+6 1.43e+ 4   7.29e+ 2 8.34e+ 4
    ##   9 annual_total_stack_air_lb      0        4.50e+6 1.82e+ 4   9.36e+ 0 1.10e+ 5
    ##  10 annual_total_stack_air_lb_pe…  0        7.99e+5 9.38e+ 2   4.76e- 1 8.15e+ 3
    ##  11 annual_total_stack_air_lb_pl…  0        6.25e+6 4.94e+ 4   3.26e+ 3 1.88e+ 5
    ##  12 area_km2                       2.13e- 2 2.25e+5 1.66e+ 2   1.54e+ 1 1.31e+ 3
    ##  13 bcsmass                        1.42e-11 3.59e-9 6.17e-10   5.17e-10 3.30e-10
    ##  14 be30_grd                      -6.44e+ 1 3.56e+3 2.58e+ 2   1.76e+ 2 3.71e+ 2
    ##  15 cldtot                         1.94e- 1 8.03e-1 4.67e- 1   4.89e- 1 8.56e- 2
    ##  16 def                            0        1.99e+2 3.43e+ 1   1.73e+ 1 3.84e+ 1
    ##  17 ds30_grd                      -6.43e+ 1 3.58e+3 2.61e+ 2   1.78e+ 2 3.72e+ 2
    ##  18 dusmass25                      2.34e-10 1.78e-8 1.67e- 9   1.47e- 9 9.38e-10
    ##  19 etr                            1.66e+ 0 1.04e+1 4.86e+ 0   4.65e+ 0 1.11e+ 0
    ##  20 evap                           1.48e- 6 6.66e-5 2.62e- 5   2.72e- 5 9.41e- 6
    ##  21 evi                           -4.97e- 2 5.33e-1 2.77e- 1   2.89e- 1 8.41e- 2
    ##  22 fractional_impervious_surface  0        9.34e+1 3.08e+ 1   2.92e+ 1 2.41e+ 1
    ##  23 grn                            0        9.83e-1 8.63e- 1   9.00e- 1 1.39e- 1
    ##  24 gwetroot                       1.69e- 1 9.69e-1 5.98e- 1   6.23e- 1 1.21e- 1
    ##  25 koppen_1                       1.39e- 4 1   e+0 8.40e- 1   1   e+ 0 2.99e- 1
    ##  26 koppen_14                      1.53e- 5 1   e+0 9.85e- 1   1   e+ 0 1.03e- 1
    ##  27 koppen_15                      2.06e- 4 1   e+0 3.98e- 1   3.13e- 1 3.33e- 1
    ##  28 koppen_16                      1.00e- 3 4.12e-2 1.57e- 2   1.20e- 2 1.56e- 2
    ##  29 koppen_17                      1.96e- 4 1   e+0 6.33e- 1   8.22e- 1 4.02e- 1
    ##  30 koppen_18                      3.43e- 6 1   e+0 5.87e- 1   6.34e- 1 3.94e- 1
    ##  31 koppen_19                      4.93e- 8 1   e+0 3.64e- 1   1.99e- 1 3.74e- 1
    ##  32 koppen_2                       3.24e- 6 1   e+0 9.34e- 1   1   e+ 0 2.10e- 1
    ##  33 koppen_21                      1.57e- 4 1   e+0 3.57e- 1   2.46e- 1 3.61e- 1
    ##  34 koppen_22                      3.80e- 3 1   e+0 4.44e- 1   4.01e- 1 3.55e- 1
    ##  35 koppen_23                      2.73e- 4 2.70e-1 8.02e- 2   2.53e- 2 1.27e- 1
    ##  36 koppen_25                      1.06e- 6 1   e+0 9.66e- 1   1   e+ 0 1.53e- 1
    ##  37 koppen_26                      1.57e- 7 1   e+0 8.00e- 1   1   e+ 0 3.34e- 1
    ##  38 koppen_27                      8.26e- 5 1   e+0 3.91e- 1   3.15e- 1 3.28e- 1
    ##  39 koppen_29                      2.30e- 5 1   e+0 2.16e- 1   1.28e- 1 2.27e- 1
    ##  40 koppen_3                       3.15e- 5 1   e+0 9.09e- 1   1   e+ 0 2.32e- 1
    ##  41 koppen_30                      1.37e- 4 4.52e-3 2.18e- 3   1.88e- 3 2.21e- 3
    ##  42 koppen_4                       7.54e- 6 1   e+0 9.53e- 1   1   e+ 0 1.83e- 1
    ##  43 koppen_5                       3.91e- 5 1   e+0 7.54e- 1   1   e+ 0 3.55e- 1
    ##  44 koppen_6                       2.36e- 6 1   e+0 8.67e- 1   1   e+ 0 2.87e- 1
    ##  45 koppen_7                       2.34e- 6 1   e+0 8.37e- 1   1   e+ 0 3.03e- 1
    ##  46 koppen_8                       3.51e- 7 1   e+0 9.03e- 1   1   e+ 0 2.54e- 1
    ##  47 koppen_9                       1.27e- 5 1   e+0 9.40e- 1   1   e+ 0 2.08e- 1
    ##  48 koppen_confidence              0        1   e+2 9.28e+ 1   1   e+ 2 1.29e+ 1
    ##  49 lai                            1.00e-20 7.35e+0 1.90e+ 0   1.76e+ 0 8.98e- 1
    ##  50 land_cover_11                  0        1   e+0 2.39e- 2   1.13e- 3 7.29e- 2
    ##  51 land_cover_12                  0        5.13e-2 4.52e- 6   0        2.98e- 4
    ##  52 land_cover_21                  0        8.12e-1 1.02e- 1   6.96e- 2 1.01e- 1
    ##  53 land_cover_22                  0        9.16e-1 1.99e- 1   1.70e- 1 1.67e- 1
    ##  54 land_cover_23                  0        1.00e+0 2.45e- 1   1.77e- 1 2.40e- 1
    ##  55 land_cover_24                  0        1   e+0 8.09e- 2   2.12e- 2 1.48e- 1
    ##  56 land_cover_31                  0        9.37e-1 3.06e- 3   0        1.85e- 2
    ##  57 land_cover_41                  0        9.46e-1 7.57e- 2   4.35e- 4 1.46e- 1
    ##  58 land_cover_42                  0        9.48e-1 3.27e- 2   2.07e- 9 9.51e- 2
    ##  59 land_cover_43                  0        7.63e-1 2.02e- 2   1.38e- 8 5.27e- 2
    ##  60 land_cover_52                  0        9.96e-1 2.82e- 2   0        1.13e- 1
    ##  61 land_cover_71                  0        9.81e-1 2.11e- 2   5.13e- 6 8.14e- 2
    ##  62 land_cover_81                  0        8.75e-1 5.00e- 2   3.01e- 5 1.05e- 1
    ##  63 land_cover_82                  0        9.54e-1 6.61e- 2   0        1.66e- 1
    ##  64 land_cover_90                  0        1   e+0 3.60e- 2   1.35e- 3 8.01e- 2
    ##  65 land_cover_95                  0        9.28e-1 8.71e- 3   7.29e- 6 3.72e- 2
    ##  66 land_cover_confidence          0        9.89e+1 5.34e+ 1   5.29e+ 1 1.36e+ 1
    ##  67 lst_day_1km_k                  2.68e+ 2 3.17e+2 2.95e+ 2   2.94e+ 2 6.23e+ 0
    ##  68 lst_night_1km_k                2.64e+ 2 2.98e+2 2.83e+ 2   2.83e+ 2 4.96e+ 0
    ##  69 lwgab                          2.00e+ 2 3.99e+2 3.14e+ 2   3.08e+ 2 2.77e+ 1
    ##  70 md30_grd                      -6.44e+ 1 3.57e+3 2.60e+ 2   1.77e+ 2 3.72e+ 2
    ##  71 mi30_grd                      -6.61e+ 1 3.44e+3 2.44e+ 2   1.63e+ 2 3.66e+ 2
    ##  72 mn30_grd                      -6.43e+ 1 3.57e+3 2.61e+ 2   1.78e+ 2 3.72e+ 2
    ##  73 mx30_grd                      -6.10e+ 1 3.73e+3 2.80e+ 2   1.93e+ 2 3.79e+ 2
    ##  74 ndvi                          -1.60e- 1 8.55e-1 4.67e- 1   4.80e- 1 1.39e- 1
    ##  75 pblh                           2.87e+ 2 1.42e+3 7.25e+ 2   7.05e+ 2 1.34e+ 2
    ##  76 pdsi                          -7.44e+ 0 1.04e+1 6.72e- 1   7.58e- 1 2.46e+ 0
    ##  77 pet                            1.47e+ 0 2.04e+2 5.14e+ 1   3.45e+ 1 5.09e+ 1
    ##  78 ppt                            0        6.11e+2 8.49e+ 1   8.94e+ 1 3.94e+ 1
    ##  79 pr                             4.14e- 2 1.21e+1 2.86e+ 0   2.96e+ 0 1.26e+ 0
    ##  80 precsno                        0        6.84e-5 2.78e- 6   1.91e- 6 3.04e- 6
    ##  81 prectotcorr                    1.68e- 6 1.32e-4 3.21e- 5   3.30e- 5 1.32e- 5
    ##  82 prop_cover_huc2_01             0        1   e+0 4.17e- 2   0        1.99e- 1
    ##  83 prop_cover_huc2_02             0        1   e+0 1.49e- 1   0        3.55e- 1
    ##  84 prop_cover_huc2_03             0        1   e+0 1.62e- 1   0        3.68e- 1
    ##  85 prop_cover_huc2_04             0        1   e+0 8.13e- 2   0        2.71e- 1
    ##  86 prop_cover_huc2_05             0        1   e+0 8.04e- 2   0        2.71e- 1
    ##  87 prop_cover_huc2_06             0        1   e+0 1.64e- 2   0        1.26e- 1
    ##  88 prop_cover_huc2_07             0        1   e+0 7.56e- 2   0        2.63e- 1
    ##  89 prop_cover_huc2_08             0        1   e+0 2.72e- 2   0        1.61e- 1
    ##  90 prop_cover_huc2_09             0        1   e+0 2.89e- 3   0        5.26e- 2
    ##  91 prop_cover_huc2_10             0        1   e+0 4.38e- 2   0        2.04e- 1
    ##  92 prop_cover_huc2_11             0        1   e+0 3.63e- 2   0        1.86e- 1
    ##  93 prop_cover_huc2_12             0        1   e+0 7.45e- 2   0        2.62e- 1
    ##  94 prop_cover_huc2_13             0        1   e+0 1.01e- 2   0        9.94e- 2
    ##  95 prop_cover_huc2_14             0        1   e+0 3.23e- 3   0        5.60e- 2
    ##  96 prop_cover_huc2_15             0        1   e+0 2.81e- 2   0        1.65e- 1
    ##  97 prop_cover_huc2_16             0        1   e+0 1.10e- 2   0        1.04e- 1
    ##  98 prop_cover_huc2_17             0        1   e+0 3.94e- 2   0        1.94e- 1
    ##  99 prop_cover_huc2_18             0        1   e+0 1.08e- 1   0        3.11e- 1
    ## 100 prop_cover_huc2_20             0        1   e+0 5.06e- 3   0        7.04e- 2
    ## 101 prop_cover_huc2_21             0        0       0          0        0       
    ## 102 prop_cover_huc2_22             0        0       0          0        0       
    ## 103 prop_heavy_coverage            0        1.68e-1 6.78e- 3   2.73e- 3 1.32e- 2
    ## 104 prop_light_coverage            0        5.77e-1 1.11e- 1   7.24e- 2 1.09e- 1
    ## 105 prop_med_coverage              0        2.01e-1 2.31e- 2   1.27e- 2 2.96e- 2
    ## 106 ps                             6.83e+ 4 1.02e+5 9.81e+ 4   9.91e+ 4 4.45e+ 3
    ## 107 qv2m                           1.14e- 3 1.75e-2 8.19e- 3   7.64e- 3 2.52e- 3
    ## 108 rmax                           3.26e+ 1 9.98e+1 8.42e+ 1   8.63e+ 1 9.28e+ 0
    ## 109 rmin                           9.79e+ 0 7.38e+1 4.27e+ 1   4.46e+ 1 8.67e+ 0
    ## 110 road_density_km_per_km2        5.99e- 6 8.83e+0 5.49e- 1   3.58e- 1 5.90e- 1
    ## 111 sd30_grd                       0        1.03e+2 7.54e+ 0   4.86e+ 0 8.44e+ 0
    ## 112 slp                            1.00e+ 5 1.02e+5 1.02e+ 5   1.02e+ 5 1.69e+ 2
    ## 113 soil                           0        3.64e+2 6.92e+ 1   6.72e+ 1 4.20e+ 1
    ## 114 solclear                       4.76e+ 0 3.38e+1 2.12e+ 1   2.16e+ 1 6.92e+ 0
    ## 115 solslope                       1.82e+ 0 3.02e+1 1.50e+ 1   1.53e+ 1 5.82e+ 0
    ## 116 soltotal                       1.85e+ 0 3.03e+1 1.50e+ 1   1.53e+ 1 5.82e+ 0
    ## 117 soltrans                       3.34e- 1 9.39e-1 6.92e- 1   6.95e- 1 8.47e- 2
    ## 118 sph                            2.68e- 3 1.61e-2 7.76e- 3   7.14e- 3 2.26e- 3
    ## 119 srad                           6.17e+ 1 2.65e+2 1.86e+ 2   1.84e+ 2 2.50e+ 1
    ## 120 sur_refl_b01                   1.31e- 3 5.74e-1 1.13e- 1   1.08e- 1 4.20e- 2
    ## 121 sur_refl_b02                   3.48e- 3 5.57e-1 2.60e- 1   2.63e- 1 4.40e- 2
    ## 122 sur_refl_b03                   1.17e- 2 5.92e-1 8.02e- 2   7.41e- 2 3.69e- 2
    ## 123 sur_refl_b04                   7.23e- 3 5.94e-1 1.09e- 1   1.03e- 1 3.73e- 2
    ## 124 sur_refl_b05                   4.32e- 3 4.53e-1 2.67e- 1   2.72e- 1 4.51e- 2
    ## 125 sur_refl_b06                   5.61e- 3 4.95e-1 2.06e- 1   2.01e- 1 4.08e- 2
    ## 126 sur_refl_b07                   5.03e- 3 4.74e-1 1.27e- 1   1.21e- 1 3.73e- 2
    ## 127 swe                            0        3.44e+2 9.19e+ 0   2.49e- 1 1.59e+ 1
    ## 128 t2mdew                         2.54e+ 2 2.96e+2 2.81e+ 2   2.81e+ 2 5.15e+ 0
    ## 129 tdmean                        -1.78e+ 1 2.49e+1 7.55e+ 0   7.58e+ 0 8.47e+ 0
    ## 130 th                             6.48e+ 1 3.14e+2 1.91e+ 2   1.96e+ 2 2.91e+ 1
    ## 131 tmax                          -7.97e+ 0 3.35e+1 2.08e+ 1   2.00e+ 1 4.91e+ 0
    ## 132 tmax_norm                     -1.08e+ 1 4.32e+1 1.99e+ 1   2.11e+ 1 9.22e+ 0
    ## 133 tmean                         -1.57e+ 1 3.56e+1 1.43e+ 1   1.51e+ 1 8.87e+ 0
    ## 134 tmin                          -1.52e+ 1 2.39e+1 9.06e+ 0   8.52e+ 0 4.72e+ 0
    ## 135 tmin_norm                     -2.11e+ 1 2.80e+1 8.60e+ 0   9.04e+ 0 8.67e+ 0
    ## 136 tmmn                           2.65e+ 2 2.97e+2 2.82e+ 2   2.82e+ 2 4.61e+ 0
    ## 137 tmmx                           2.79e+ 2 3.07e+2 2.94e+ 2   2.93e+ 2 4.86e+ 0
    ## 138 total_road_km                  2.00e- 4 2.01e+3 1.28e+ 1   3.78e+ 0 2.83e+ 1
    ## 139 totexttau                      5.04e- 2 2.64e-1 1.50e- 1   1.56e- 1 3.27e- 2
    ## 140 ts                             2.53e+ 2 3.01e+2 2.87e+ 2   2.86e+ 2 5.33e+ 0
    ## 141 u10m                          -6.59e+ 0 4.87e+0 5.19e- 1   6.35e- 1 8.67e- 1
    ## 142 vap                            2.42e- 1 2.59e+0 1.23e+ 0   1.13e+ 0 3.73e- 1
    ## 143 vpd                            6.22e- 2 3.03e+0 8.48e- 1   7.66e- 1 3.70e- 1
    ## 144 vpdmax                         7.05e- 1 7.66e+1 1.50e+ 1   1.42e+ 1 9.00e+ 0
    ## 145 vpdmin                         1.03e- 3 2.53e+1 1.59e+ 0   1.09e+ 0 1.96e+ 0
    ## 146 vs                             1.76e+ 0 7.28e+0 3.97e+ 0   4.00e+ 0 6.97e- 1
    ## 147 ws                             1.70e+ 0 8.29e+0 3.31e+ 0   3.30e+ 0 6.32e- 1
    ## 148 z0m                            1.77e- 4 2.26e+0 6.25e- 1   1.43e- 1 7.31e- 1

``` r
# Compare observed vs expected
comparison <- var_summary %>%
  left_join(expected_ranges, by = "variable") %>%
  mutate(
    min_ok = min_val >= min_exp,
    max_ok = max_val <= max_exp,
    passes = ifelse(min_ok & max_ok, "Y", "N")
  ) %>%
  select(variable, min_val, max_val, min_exp, max_exp, passes)

print(comparison, n = Inf)
```

    ## # A tibble: 148 × 6
    ##     variable                             min_val max_val  min_exp max_exp passes
    ##     <chr>                                  <dbl>   <dbl>    <dbl>   <dbl> <chr> 
    ##   1 aet                                 1.70e+ 0 1.31e+2  0       2.5 e+2 Y     
    ##   2 albedo                              7.46e- 2 6.06e-1  0       9   e-1 Y     
    ##   3 annual_total_air_lb                 0        8.91e+6  0       1   e+8 Y     
    ##   4 annual_total_air_lb_per_km2         0        8.30e+5  0       1   e+6 Y     
    ##   5 annual_total_air_lb_plusbuffer      0        8.91e+6  0       1   e+8 Y     
    ##   6 annual_total_fugitive_air_lb        0        8.48e+6  0       1   e+8 Y     
    ##   7 annual_total_fugitive_air_lb_per_…  0        2.66e+5  0       5   e+5 Y     
    ##   8 annual_total_fugitive_air_lb_plus…  0        8.48e+6  0       1   e+8 Y     
    ##   9 annual_total_stack_air_lb           0        4.50e+6  0       1   e+8 Y     
    ##  10 annual_total_stack_air_lb_per_km2   0        7.99e+5  0       1   e+6 Y     
    ##  11 annual_total_stack_air_lb_plusbuf…  0        6.25e+6  0       1   e+8 Y     
    ##  12 area_km2                            2.13e- 2 2.25e+5  5  e+ 0 3.83e+5 N     
    ##  13 bcsmass                             1.42e-11 3.59e-9  1  e-11 1   e-8 Y     
    ##  14 be30_grd                           -6.44e+ 1 3.56e+3  0       6   e+3 N     
    ##  15 cldtot                              1.94e- 1 8.03e-1  0       1   e+0 Y     
    ##  16 def                                 0        1.99e+2  0       3.4 e+2 Y     
    ##  17 ds30_grd                           -6.43e+ 1 3.58e+3  0       6   e+3 N     
    ##  18 dusmass25                           2.34e-10 1.78e-8  1  e-11 3   e-8 Y     
    ##  19 etr                                 1.66e+ 0 1.04e+1  0       1.8 e+1 Y     
    ##  20 evap                                1.48e- 6 6.66e-5  1  e- 8 1   e-4 Y     
    ##  21 evi                                -4.97e- 2 5.33e-1 -1  e- 1 1   e+0 Y     
    ##  22 fractional_impervious_surface       0        9.34e+1  0       1   e+2 Y     
    ##  23 grn                                 0        9.83e-1  0       1   e+0 Y     
    ##  24 gwetroot                            1.69e- 1 9.69e-1  0       1   e+0 Y     
    ##  25 koppen_1                            1.39e- 4 1   e+0  0       1   e+0 Y     
    ##  26 koppen_14                           1.53e- 5 1   e+0  0       1   e+0 Y     
    ##  27 koppen_15                           2.06e- 4 1   e+0  0       1   e+0 Y     
    ##  28 koppen_16                           1.00e- 3 4.12e-2  0       1   e+0 Y     
    ##  29 koppen_17                           1.96e- 4 1   e+0  0       1   e+0 Y     
    ##  30 koppen_18                           3.43e- 6 1   e+0  0       1   e+0 Y     
    ##  31 koppen_19                           4.93e- 8 1   e+0  0       1   e+0 Y     
    ##  32 koppen_2                            3.24e- 6 1   e+0  0       1   e+0 Y     
    ##  33 koppen_21                           1.57e- 4 1   e+0  0       1   e+0 Y     
    ##  34 koppen_22                           3.80e- 3 1   e+0  0       1   e+0 Y     
    ##  35 koppen_23                           2.73e- 4 2.70e-1  0       1   e+0 Y     
    ##  36 koppen_25                           1.06e- 6 1   e+0  0       1   e+0 Y     
    ##  37 koppen_26                           1.57e- 7 1   e+0  0       1   e+0 Y     
    ##  38 koppen_27                           8.26e- 5 1   e+0  0       1   e+0 Y     
    ##  39 koppen_29                           2.30e- 5 1   e+0  0       1   e+0 Y     
    ##  40 koppen_3                            3.15e- 5 1   e+0  0       1   e+0 Y     
    ##  41 koppen_30                           1.37e- 4 4.52e-3  0       1   e+0 Y     
    ##  42 koppen_4                            7.54e- 6 1   e+0  0       1   e+0 Y     
    ##  43 koppen_5                            3.91e- 5 1   e+0  0       1   e+0 Y     
    ##  44 koppen_6                            2.36e- 6 1   e+0  0       1   e+0 Y     
    ##  45 koppen_7                            2.34e- 6 1   e+0  0       1   e+0 Y     
    ##  46 koppen_8                            3.51e- 7 1   e+0  0       1   e+0 Y     
    ##  47 koppen_9                            1.27e- 5 1   e+0  0       1   e+0 Y     
    ##  48 koppen_confidence                   0        1   e+2  0       1   e+2 Y     
    ##  49 lai                                 1.00e-20 7.35e+0  0       9   e+0 Y     
    ##  50 land_cover_11                       0        1   e+0  0       1   e+0 Y     
    ##  51 land_cover_12                       0        5.13e-2  0       1   e+0 Y     
    ##  52 land_cover_21                       0        8.12e-1  0       1   e+0 Y     
    ##  53 land_cover_22                       0        9.16e-1  0       1   e+0 Y     
    ##  54 land_cover_23                       0        1.00e+0  0       1   e+0 Y     
    ##  55 land_cover_24                       0        1   e+0  0       1   e+0 Y     
    ##  56 land_cover_31                       0        9.37e-1  0       1   e+0 Y     
    ##  57 land_cover_41                       0        9.46e-1  0       1   e+0 Y     
    ##  58 land_cover_42                       0        9.48e-1  0       1   e+0 Y     
    ##  59 land_cover_43                       0        7.63e-1  0       1   e+0 Y     
    ##  60 land_cover_52                       0        9.96e-1  0       1   e+0 Y     
    ##  61 land_cover_71                       0        9.81e-1  0       1   e+0 Y     
    ##  62 land_cover_81                       0        8.75e-1  0       1   e+0 Y     
    ##  63 land_cover_82                       0        9.54e-1  0       1   e+0 Y     
    ##  64 land_cover_90                       0        1   e+0  0       1   e+0 Y     
    ##  65 land_cover_95                       0        9.28e-1  0       1   e+0 Y     
    ##  66 land_cover_confidence               0        9.89e+1  0       1   e+2 Y     
    ##  67 lst_day_1km_k                       2.68e+ 2 3.17e+2  2.3e+ 2 4   e+2 Y     
    ##  68 lst_night_1km_k                     2.64e+ 2 2.98e+2  2.3e+ 2 3.1 e+2 Y     
    ##  69 lwgab                               2.00e+ 2 3.99e+2  2  e+ 2 4   e+2 Y     
    ##  70 md30_grd                           -6.44e+ 1 3.57e+3 -4  e+ 2 8.85e+3 Y     
    ##  71 mi30_grd                           -6.61e+ 1 3.44e+3 -4  e+ 2 8.85e+3 Y     
    ##  72 mn30_grd                           -6.43e+ 1 3.57e+3 -4  e+ 2 8.85e+3 Y     
    ##  73 mx30_grd                           -6.10e+ 1 3.73e+3  0       8.85e+3 N     
    ##  74 ndvi                               -1.60e- 1 8.55e-1 -1  e+ 0 1   e+0 Y     
    ##  75 pblh                                2.87e+ 2 1.42e+3  5  e+ 1 2.5 e+3 Y     
    ##  76 pdsi                               -7.44e+ 0 1.04e+1 -1  e+ 1 2   e+1 Y     
    ##  77 pet                                 1.47e+ 0 2.04e+2  0       3.5 e+2 Y     
    ##  78 ppt                                 0        6.11e+2  0       1.4 e+3 Y     
    ##  79 pr                                  4.14e- 2 1.21e+1  0       5   e+1 Y     
    ##  80 precsno                             0        6.84e-5  0       7   e-5 Y     
    ##  81 prectotcorr                         1.68e- 6 1.32e-4  1  e- 7 5   e-4 Y     
    ##  82 prop_cover_huc2_01                  0        1   e+0  0       1   e+0 Y     
    ##  83 prop_cover_huc2_02                  0        1   e+0  0       1   e+0 Y     
    ##  84 prop_cover_huc2_03                  0        1   e+0  0       1   e+0 Y     
    ##  85 prop_cover_huc2_04                  0        1   e+0  0       1   e+0 Y     
    ##  86 prop_cover_huc2_05                  0        1   e+0  0       1   e+0 Y     
    ##  87 prop_cover_huc2_06                  0        1   e+0  0       1   e+0 Y     
    ##  88 prop_cover_huc2_07                  0        1   e+0  0       1   e+0 Y     
    ##  89 prop_cover_huc2_08                  0        1   e+0  0       1   e+0 Y     
    ##  90 prop_cover_huc2_09                  0        1   e+0  0       1   e+0 Y     
    ##  91 prop_cover_huc2_10                  0        1   e+0  0       1   e+0 Y     
    ##  92 prop_cover_huc2_11                  0        1   e+0  0       1   e+0 Y     
    ##  93 prop_cover_huc2_12                  0        1   e+0  0       1   e+0 Y     
    ##  94 prop_cover_huc2_13                  0        1   e+0  0       1   e+0 Y     
    ##  95 prop_cover_huc2_14                  0        1   e+0  0       1   e+0 Y     
    ##  96 prop_cover_huc2_15                  0        1   e+0  0       1   e+0 Y     
    ##  97 prop_cover_huc2_16                  0        1   e+0  0       1   e+0 Y     
    ##  98 prop_cover_huc2_17                  0        1   e+0  0       1   e+0 Y     
    ##  99 prop_cover_huc2_18                  0        1   e+0  0       1   e+0 Y     
    ## 100 prop_cover_huc2_20                  0        1   e+0  0       1   e+0 Y     
    ## 101 prop_cover_huc2_21                  0        0        0       1   e+0 Y     
    ## 102 prop_cover_huc2_22                  0        0        0       1   e+0 Y     
    ## 103 prop_heavy_coverage                 0        1.68e-1  0       1   e+0 Y     
    ## 104 prop_light_coverage                 0        5.77e-1  0       1   e+0 Y     
    ## 105 prop_med_coverage                   0        2.01e-1  0       1   e+0 Y     
    ## 106 ps                                  6.83e+ 4 1.02e+5  5  e+ 4 1.05e+5 Y     
    ## 107 qv2m                                1.14e- 3 1.75e-2  0       4   e-2 Y     
    ## 108 rmax                                3.26e+ 1 9.98e+1  0       1   e+2 Y     
    ## 109 rmin                                9.79e+ 0 7.38e+1  0       1   e+2 Y     
    ## 110 road_density_km_per_km2             5.99e- 6 8.83e+0  0       9   e+0 Y     
    ## 111 sd30_grd                            0        1.03e+2  0       6   e+2 Y     
    ## 112 slp                                 1.00e+ 5 1.02e+5  9.9e+ 4 1.02e+5 Y     
    ## 113 soil                                0        3.64e+2  0       5   e+2 Y     
    ## 114 solclear                            4.76e+ 0 3.38e+1  2  e+ 0 3.5 e+1 Y     
    ## 115 solslope                            1.82e+ 0 3.02e+1  0       4   e+1 Y     
    ## 116 soltotal                            1.85e+ 0 3.03e+1  0       3.2 e+1 Y     
    ## 117 soltrans                            3.34e- 1 9.39e-1  0       1   e+0 Y     
    ## 118 sph                                 2.68e- 3 1.61e-2  0       3   e-2 Y     
    ## 119 srad                                6.17e+ 1 2.65e+2  0       4   e+2 Y     
    ## 120 sur_refl_b01                        1.31e- 3 5.74e-1 -1  e- 1 1   e+0 Y     
    ## 121 sur_refl_b02                        3.48e- 3 5.57e-1  0       1   e+0 Y     
    ## 122 sur_refl_b03                        1.17e- 2 5.92e-1  0       1   e+0 Y     
    ## 123 sur_refl_b04                        7.23e- 3 5.94e-1  0       1   e+0 Y     
    ## 124 sur_refl_b05                        4.32e- 3 4.53e-1  0       1   e+0 Y     
    ## 125 sur_refl_b06                        5.61e- 3 4.95e-1  0       1   e+0 Y     
    ## 126 sur_refl_b07                        5.03e- 3 4.74e-1  0       1   e+0 Y     
    ## 127 swe                                 0        3.44e+2  0       1.3 e+3 Y     
    ## 128 t2mdew                              2.54e+ 2 2.96e+2  2.3e+ 2 3.5 e+2 Y     
    ## 129 tdmean                             -1.78e+ 1 2.49e+1 -4  e+ 1 2.7 e+1 Y     
    ## 130 th                                  6.48e+ 1 3.14e+2  0       3.6 e+2 Y     
    ## 131 tmax                               -7.97e+ 0 3.35e+1 -5  e+ 1 5   e+1 Y     
    ## 132 tmax_norm                          -1.08e+ 1 4.32e+1 -5  e+ 1 5   e+1 Y     
    ## 133 tmean                              -1.57e+ 1 3.56e+1 -1.6e+ 1 4   e+1 Y     
    ## 134 tmin                               -1.52e+ 1 2.39e+1 -6  e+ 1 3.3 e+1 Y     
    ## 135 tmin_norm                          -2.11e+ 1 2.80e+1 -6  e+ 1 3   e+1 Y     
    ## 136 tmmn                                2.65e+ 2 2.97e+2  2.4e+ 2 3.5 e+2 Y     
    ## 137 tmmx                                2.79e+ 2 3.07e+2  2.4e+ 2 3.5 e+2 Y     
    ## 138 total_road_km                       2.00e- 4 2.01e+3  0       6   e+4 Y     
    ## 139 totexttau                           5.04e- 2 2.64e-1  2  e- 2 3   e-1 Y     
    ## 140 ts                                  2.53e+ 2 3.01e+2  2.5e+ 2 3.05e+2 Y     
    ## 141 u10m                               -6.59e+ 0 4.87e+0 -2  e+ 1 2   e+1 Y     
    ## 142 vap                                 2.42e- 1 2.59e+0  0       5   e+0 Y     
    ## 143 vpd                                 6.22e- 2 3.03e+0  0       6   e+0 Y     
    ## 144 vpdmax                              7.05e- 1 7.66e+1  0       8   e+1 Y     
    ## 145 vpdmin                              1.03e- 3 2.53e+1  0       3   e+1 Y     
    ## 146 vs                                  1.76e+ 0 7.28e+0  0       2   e+1 Y     
    ## 147 ws                                  1.70e+ 0 8.29e+0  0       1.4 e+1 Y     
    ## 148 z0m                                 1.77e- 4 2.26e+0  1  e- 4 3   e+0 Y

# Tract-Monthly Sanity checks

``` r
# check for duplicated geoid-year-variable combinations
anyDuplicated(tract_monthly[c("geoid", "year", "variable")])
```

    ## [1] 0

``` r
# Basic shape
# should be over 84,414 counties, 17 years (2010-2024) normal and static, 140 vars
# 84119 census tracts - some may have had errors

tract_monthly %>%
  summarise(
    n_rows     = n(),
    n_tracts = n_distinct(geoid),
    n_years    = n_distinct(year),
    n_vars     = n_distinct(variable)
  ) %>%
  collect()
```

    ## # A tibble: 1 × 4
    ##       n_rows n_tracts n_years n_vars
    ##        <int>    <int>   <int>  <int>
    ## 1 1108260367    84119      17    148

``` r
# List distinct years in the dataset
tract_monthly %>%
  distinct(year, month) %>%
  arrange(year, month) %>%
  collect() %>%
  print(n = Inf)
```

    ## # A tibble: 193 × 2
    ##     year   month
    ##     <chr>  <int>
    ##   1 2010       1
    ##   2 2010       2
    ##   3 2010       3
    ##   4 2010       4
    ##   5 2010       5
    ##   6 2010       6
    ##   7 2010       7
    ##   8 2010       8
    ##   9 2010       9
    ##  10 2010      10
    ##  11 2010      11
    ##  12 2010      12
    ##  13 2011       1
    ##  14 2011       2
    ##  15 2011       3
    ##  16 2011       4
    ##  17 2011       5
    ##  18 2011       6
    ##  19 2011       7
    ##  20 2011       8
    ##  21 2011       9
    ##  22 2011      10
    ##  23 2011      11
    ##  24 2011      12
    ##  25 2012       1
    ##  26 2012       2
    ##  27 2012       3
    ##  28 2012       4
    ##  29 2012       5
    ##  30 2012       6
    ##  31 2012       7
    ##  32 2012       8
    ##  33 2012       9
    ##  34 2012      10
    ##  35 2012      11
    ##  36 2012      12
    ##  37 2013       1
    ##  38 2013       2
    ##  39 2013       3
    ##  40 2013       4
    ##  41 2013       5
    ##  42 2013       6
    ##  43 2013       7
    ##  44 2013       8
    ##  45 2013       9
    ##  46 2013      10
    ##  47 2013      11
    ##  48 2013      12
    ##  49 2014       1
    ##  50 2014       2
    ##  51 2014       3
    ##  52 2014       4
    ##  53 2014       5
    ##  54 2014       6
    ##  55 2014       7
    ##  56 2014       8
    ##  57 2014       9
    ##  58 2014      10
    ##  59 2014      11
    ##  60 2014      12
    ##  61 2015       1
    ##  62 2015       2
    ##  63 2015       3
    ##  64 2015       4
    ##  65 2015       5
    ##  66 2015       6
    ##  67 2015       7
    ##  68 2015       8
    ##  69 2015       9
    ##  70 2015      10
    ##  71 2015      11
    ##  72 2015      12
    ##  73 2016       1
    ##  74 2016       2
    ##  75 2016       3
    ##  76 2016       4
    ##  77 2016       5
    ##  78 2016       6
    ##  79 2016       7
    ##  80 2016       8
    ##  81 2016       9
    ##  82 2016      10
    ##  83 2016      11
    ##  84 2016      12
    ##  85 2017       1
    ##  86 2017       2
    ##  87 2017       3
    ##  88 2017       4
    ##  89 2017       5
    ##  90 2017       6
    ##  91 2017       7
    ##  92 2017       8
    ##  93 2017       9
    ##  94 2017      10
    ##  95 2017      11
    ##  96 2017      12
    ##  97 2018       1
    ##  98 2018       2
    ##  99 2018       3
    ## 100 2018       4
    ## 101 2018       5
    ## 102 2018       6
    ## 103 2018       7
    ## 104 2018       8
    ## 105 2018       9
    ## 106 2018      10
    ## 107 2018      11
    ## 108 2018      12
    ## 109 2019       1
    ## 110 2019       2
    ## 111 2019       3
    ## 112 2019       4
    ## 113 2019       5
    ## 114 2019       6
    ## 115 2019       7
    ## 116 2019       8
    ## 117 2019       9
    ## 118 2019      10
    ## 119 2019      11
    ## 120 2019      12
    ## 121 2020       1
    ## 122 2020       2
    ## 123 2020       3
    ## 124 2020       4
    ## 125 2020       5
    ## 126 2020       6
    ## 127 2020       7
    ## 128 2020       8
    ## 129 2020       9
    ## 130 2020      10
    ## 131 2020      11
    ## 132 2020      12
    ## 133 2021       1
    ## 134 2021       2
    ## 135 2021       3
    ## 136 2021       4
    ## 137 2021       5
    ## 138 2021       6
    ## 139 2021       7
    ## 140 2021       8
    ## 141 2021       9
    ## 142 2021      10
    ## 143 2021      11
    ## 144 2021      12
    ## 145 2022       1
    ## 146 2022       2
    ## 147 2022       3
    ## 148 2022       4
    ## 149 2022       5
    ## 150 2022       6
    ## 151 2022       7
    ## 152 2022       8
    ## 153 2022       9
    ## 154 2022      10
    ## 155 2022      11
    ## 156 2022      12
    ## 157 2023       1
    ## 158 2023       2
    ## 159 2023       3
    ## 160 2023       4
    ## 161 2023       5
    ## 162 2023       6
    ## 163 2023       7
    ## 164 2023       8
    ## 165 2023       9
    ## 166 2023      10
    ## 167 2023      11
    ## 168 2023      12
    ## 169 2024       1
    ## 170 2024       2
    ## 171 2024       3
    ## 172 2024       4
    ## 173 2024       5
    ## 174 2024       6
    ## 175 2024       7
    ## 176 2024       8
    ## 177 2024       9
    ## 178 2024      10
    ## 179 2024      11
    ## 180 2024      12
    ## 181 normal     1
    ## 182 normal     2
    ## 183 normal     3
    ## 184 normal     4
    ## 185 normal     5
    ## 186 normal     6
    ## 187 normal     7
    ## 188 normal     8
    ## 189 normal     9
    ## 190 normal    10
    ## 191 normal    11
    ## 192 normal    12
    ## 193 static    NA

``` r
var_summary <- tract_monthly %>%
  group_by(variable) %>%
  summarise(
    min_val   = min(value, na.rm = TRUE),
    max_val   = max(value, na.rm = TRUE),
    mean_val  = mean(value, na.rm = TRUE),
    median_val = median(value, na.rm = TRUE),
    sd_val    = sd(value, na.rm = TRUE)
  ) %>%
  arrange(variable) %>%
  collect()

# Compare observed vs expected
comparison <- var_summary %>%
  left_join(expected_ranges, by = "variable") %>%
  mutate(
    min_ok = min_val >= min_exp,
    max_ok = max_val <= max_exp,
    passes = ifelse(min_ok & max_ok, "Y", "N")
  ) %>%
  select(variable, min_val, max_val, min_exp, max_exp, passes)

print(comparison, n = Inf)
```

    ## # A tibble: 148 × 6
    ##     variable                             min_val max_val  min_exp max_exp passes
    ##     <chr>                                  <dbl>   <dbl>    <dbl>   <dbl> <chr> 
    ##   1 aet                                 0        2.14e+2  0       2.5 e+2 Y     
    ##   2 albedo                              4.82e- 2 6.10e-1  0       9   e-1 Y     
    ##   3 annual_total_air_lb                 0        8.91e+6  0       1   e+8 Y     
    ##   4 annual_total_air_lb_per_km2         0        8.30e+5  0       1   e+6 Y     
    ##   5 annual_total_air_lb_plusbuffer      0        8.91e+6  0       1   e+8 Y     
    ##   6 annual_total_fugitive_air_lb        0        8.48e+6  0       1   e+8 Y     
    ##   7 annual_total_fugitive_air_lb_per_…  0        2.66e+5  0       5   e+5 Y     
    ##   8 annual_total_fugitive_air_lb_plus…  0        8.48e+6  0       1   e+8 Y     
    ##   9 annual_total_stack_air_lb           0        4.50e+6  0       1   e+8 Y     
    ##  10 annual_total_stack_air_lb_per_km2   0        7.99e+5  0       1   e+6 Y     
    ##  11 annual_total_stack_air_lb_plusbuf…  0        6.25e+6  0       1   e+8 Y     
    ##  12 area_km2                            2.13e- 2 2.25e+5  5  e+ 0 3.83e+5 N     
    ##  13 bcsmass                             1.41e-11 4.36e-9  1  e-11 1   e-8 Y     
    ##  14 be30_grd                           -6.44e+ 1 3.56e+3  0       6   e+3 N     
    ##  15 cldtot                              1.89e- 1 8.09e-1  0       1   e+0 Y     
    ##  16 def                                 0        3.33e+2  0       3.4 e+2 Y     
    ##  17 ds30_grd                           -6.43e+ 1 3.58e+3  0       6   e+3 N     
    ##  18 dusmass25                           2.24e-10 2.03e-8  1  e-11 3   e-8 Y     
    ##  19 etr                                 1.03e- 1 1.78e+1  0       1.8 e+1 Y     
    ##  20 evap                                3.76e- 7 7.49e-5  1  e- 8 1   e-4 Y     
    ##  21 evi                                -8.36e- 2 8.34e-1 -1  e- 1 1   e+0 Y     
    ##  22 fractional_impervious_surface_mean  0        9.34e+1  0       1   e+2 Y     
    ##  23 grn                                 0        9.83e-1  0       1   e+0 Y     
    ##  24 gwetroot                            1.69e- 1 9.69e-1  0       1   e+0 Y     
    ##  25 koppen_1                            1.39e- 4 1   e+0  0       1   e+0 Y     
    ##  26 koppen_14                           1.53e- 5 1   e+0  0       1   e+0 Y     
    ##  27 koppen_15                           2.06e- 4 1   e+0  0       1   e+0 Y     
    ##  28 koppen_16                           1.00e- 3 4.12e-2  0       1   e+0 Y     
    ##  29 koppen_17                           1.96e- 4 1   e+0  0       1   e+0 Y     
    ##  30 koppen_18                           3.43e- 6 1   e+0  0       1   e+0 Y     
    ##  31 koppen_19                           4.93e- 8 1   e+0  0       1   e+0 Y     
    ##  32 koppen_2                            3.24e- 6 1   e+0  0       1   e+0 Y     
    ##  33 koppen_21                           1.57e- 4 1   e+0  0       1   e+0 Y     
    ##  34 koppen_22                           3.80e- 3 1   e+0  0       1   e+0 Y     
    ##  35 koppen_23                           2.73e- 4 2.70e-1  0       1   e+0 Y     
    ##  36 koppen_25                           1.06e- 6 1   e+0  0       1   e+0 Y     
    ##  37 koppen_26                           1.57e- 7 1   e+0  0       1   e+0 Y     
    ##  38 koppen_27                           8.26e- 5 1   e+0  0       1   e+0 Y     
    ##  39 koppen_29                           2.30e- 5 1   e+0  0       1   e+0 Y     
    ##  40 koppen_3                            3.15e- 5 1   e+0  0       1   e+0 Y     
    ##  41 koppen_30                           1.37e- 4 4.52e-3  0       1   e+0 Y     
    ##  42 koppen_4                            7.54e- 6 1   e+0  0       1   e+0 Y     
    ##  43 koppen_5                            3.91e- 5 1   e+0  0       1   e+0 Y     
    ##  44 koppen_6                            2.36e- 6 1   e+0  0       1   e+0 Y     
    ##  45 koppen_7                            2.34e- 6 1   e+0  0       1   e+0 Y     
    ##  46 koppen_8                            3.51e- 7 1   e+0  0       1   e+0 Y     
    ##  47 koppen_9                            1.27e- 5 1   e+0  0       1   e+0 Y     
    ##  48 koppen_confidence                   0        1   e+2  0       1   e+2 Y     
    ##  49 lai                                 1.00e-20 7.35e+0  0       9   e+0 Y     
    ##  50 land_cover_11                       0        1   e+0  0       1   e+0 Y     
    ##  51 land_cover_12                       0        5.13e-2  0       1   e+0 Y     
    ##  52 land_cover_21                       0        8.12e-1  0       1   e+0 Y     
    ##  53 land_cover_22                       0        9.16e-1  0       1   e+0 Y     
    ##  54 land_cover_23                       0        1.00e+0  0       1   e+0 Y     
    ##  55 land_cover_24                       0        1   e+0  0       1   e+0 Y     
    ##  56 land_cover_31                       0        9.37e-1  0       1   e+0 Y     
    ##  57 land_cover_41                       0        9.46e-1  0       1   e+0 Y     
    ##  58 land_cover_42                       0        9.48e-1  0       1   e+0 Y     
    ##  59 land_cover_43                       0        7.63e-1  0       1   e+0 Y     
    ##  60 land_cover_52                       0        9.96e-1  0       1   e+0 Y     
    ##  61 land_cover_71                       0        9.81e-1  0       1   e+0 Y     
    ##  62 land_cover_81                       0        8.75e-1  0       1   e+0 Y     
    ##  63 land_cover_82                       0        9.54e-1  0       1   e+0 Y     
    ##  64 land_cover_90                       0        1   e+0  0       1   e+0 Y     
    ##  65 land_cover_95                       0        9.28e-1  0       1   e+0 Y     
    ##  66 land_cover_confidence_mean          0        9.89e+1  0       1   e+2 Y     
    ##  67 lst_day_1km_k                       2.45e+ 2 3.34e+2  2.3e+ 2 4   e+2 Y     
    ##  68 lst_night_1km_k                     2.40e+ 2 3.09e+2  2.3e+ 2 3.1 e+2 Y     
    ##  69 lwgab                               2.00e+ 2 4.00e+2  2  e+ 2 4   e+2 N     
    ##  70 md30_grd                           -6.44e+ 1 3.57e+3 -4  e+ 2 8.85e+3 Y     
    ##  71 mi30_grd                           -6.61e+ 1 3.44e+3 -4  e+ 2 8.85e+3 Y     
    ##  72 mn30_grd                           -6.43e+ 1 3.57e+3 -4  e+ 2 8.85e+3 Y     
    ##  73 mx30_grd                           -6.10e+ 1 3.73e+3  0       8.85e+3 N     
    ##  74 ndvi                               -1.99e- 1 9.71e-1 -1  e+ 0 1   e+0 Y     
    ##  75 pblh                                2.06e+ 2 1.64e+3  5  e+ 1 2.5 e+3 Y     
    ##  76 pdsi                               -9.11e+ 0 1.63e+1 -1  e+ 1 2   e+1 Y     
    ##  77 pet                                 0        3.40e+2  0       3.5 e+2 Y     
    ##  78 ppt                                 0        1.35e+3  0       1.4 e+3 Y     
    ##  79 pr                                  0        4.97e+1  0       5   e+1 Y     
    ##  80 precsno                             0        6.93e-5  0       7   e-5 Y     
    ##  81 prectotcorr                         6.36e- 7 1.36e-4  1  e- 7 5   e-4 Y     
    ##  82 prop_cover_huc2_01                  0        1   e+0  0       1   e+0 Y     
    ##  83 prop_cover_huc2_02                  0        1   e+0  0       1   e+0 Y     
    ##  84 prop_cover_huc2_03                  0        1   e+0  0       1   e+0 Y     
    ##  85 prop_cover_huc2_04                  0        1   e+0  0       1   e+0 Y     
    ##  86 prop_cover_huc2_05                  0        1   e+0  0       1   e+0 Y     
    ##  87 prop_cover_huc2_06                  0        1   e+0  0       1   e+0 Y     
    ##  88 prop_cover_huc2_07                  0        1   e+0  0       1   e+0 Y     
    ##  89 prop_cover_huc2_08                  0        1   e+0  0       1   e+0 Y     
    ##  90 prop_cover_huc2_09                  0        1   e+0  0       1   e+0 Y     
    ##  91 prop_cover_huc2_10                  0        1   e+0  0       1   e+0 Y     
    ##  92 prop_cover_huc2_11                  0        1   e+0  0       1   e+0 Y     
    ##  93 prop_cover_huc2_12                  0        1   e+0  0       1   e+0 Y     
    ##  94 prop_cover_huc2_13                  0        1   e+0  0       1   e+0 Y     
    ##  95 prop_cover_huc2_14                  0        1   e+0  0       1   e+0 Y     
    ##  96 prop_cover_huc2_15                  0        1   e+0  0       1   e+0 Y     
    ##  97 prop_cover_huc2_16                  0        1   e+0  0       1   e+0 Y     
    ##  98 prop_cover_huc2_17                  0        1   e+0  0       1   e+0 Y     
    ##  99 prop_cover_huc2_18                  0        1   e+0  0       1   e+0 Y     
    ## 100 prop_cover_huc2_20                  0        1   e+0  0       1   e+0 Y     
    ## 101 prop_cover_huc2_21                  0        0        0       1   e+0 Y     
    ## 102 prop_cover_huc2_22                  0        0        0       1   e+0 Y     
    ## 103 prop_heavy_coverage                 0        9.59e-1  0       1   e+0 Y     
    ## 104 prop_light_coverage                 0        9.67e-1  0       1   e+0 Y     
    ## 105 prop_med_coverage                   0        7.67e-1  0       1   e+0 Y     
    ## 106 ps                                  6.83e+ 4 1.02e+5  5  e+ 4 1.05e+5 Y     
    ## 107 qv2m                                1.14e- 3 1.75e-2  0       4   e-2 Y     
    ## 108 rmax                                1.44e+ 1 1.00e+2  0       1   e+2 N     
    ## 109 rmin                                2.80e+ 0 9.22e+1  0       1   e+2 Y     
    ## 110 road_density_km_per_km2             5.99e- 6 8.83e+0  0       9   e+0 Y     
    ## 111 sd30_grd                            0        1.03e+2  0       6   e+2 Y     
    ## 112 slp                                 1.00e+ 5 1.02e+5  9.9e+ 4 1.02e+5 Y     
    ## 113 soil                                0        4.91e+2  0       5   e+2 Y     
    ## 114 solclear                            4.76e+ 0 3.38e+1  2  e+ 0 3.5 e+1 Y     
    ## 115 solslope                            1.82e+ 0 3.02e+1  0       4   e+1 Y     
    ## 116 soltotal                            1.85e+ 0 3.03e+1  0       3.2 e+1 Y     
    ## 117 soltrans                            3.34e- 1 9.39e-1  0       1   e+0 Y     
    ## 118 sph                                 6.59e- 4 2.27e-2  0       3   e-2 Y     
    ## 119 srad                                0        3.82e+2  0       4   e+2 Y     
    ## 120 sur_refl_b01                       -1.96e- 3 9.35e-1 -1  e- 1 1   e+0 Y     
    ## 121 sur_refl_b02                        1.11e- 3 8.79e-1  0       1   e+0 Y     
    ## 122 sur_refl_b03                        4.96e- 3 9.48e-1  0       1   e+0 Y     
    ## 123 sur_refl_b04                        8.90e- 4 9.49e-1  0       1   e+0 Y     
    ## 124 sur_refl_b05                        1.67e- 3 5.87e-1  0       1   e+0 Y     
    ## 125 sur_refl_b06                        1.72e- 3 5.25e-1  0       1   e+0 Y     
    ## 126 sur_refl_b07                        1.45e- 3 5.03e-1  0       1   e+0 Y     
    ## 127 swe                                 0        1.26e+3  0       1.3 e+3 Y     
    ## 128 t2mdew                              2.54e+ 2 2.96e+2  2.3e+ 2 3.5 e+2 Y     
    ## 129 tdmean                             -1.78e+ 1 2.49e+1 -4  e+ 1 2.7 e+1 Y     
    ## 130 th                                  1.44e+ 1 3.50e+2  0       3.6 e+2 Y     
    ## 131 tmax                               -3.00e+ 1 4.99e+1 -5  e+ 1 5   e+1 Y     
    ## 132 tmax_norm                          -1.08e+ 1 4.32e+1 -5  e+ 1 5   e+1 Y     
    ## 133 tmean                              -1.57e+ 1 3.56e+1 -1.6e+ 1 4   e+1 Y     
    ## 134 tmin                               -3.86e+ 1 3.25e+1 -6  e+ 1 3.3 e+1 Y     
    ## 135 tmin_norm                          -2.11e+ 1 2.80e+1 -6  e+ 1 3   e+1 Y     
    ## 136 tmmn                                2.45e+ 2 3.05e+2  2.4e+ 2 3.5 e+2 Y     
    ## 137 tmmx                                2.57e+ 2 3.20e+2  2.4e+ 2 3.5 e+2 Y     
    ## 138 total_road_km                       2.00e- 4 2.01e+3  0       6   e+4 Y     
    ## 139 totexttau                           4.90e- 2 2.98e-1  2  e- 2 3   e-1 Y     
    ## 140 ts                                  2.53e+ 2 3.02e+2  2.5e+ 2 3.05e+2 Y     
    ## 141 u10m                               -6.85e+ 0 5.28e+0 -2  e+ 1 2   e+1 Y     
    ## 142 vap                                 0        3.26e+0  0       5   e+0 Y     
    ## 143 vpd                                -2.10e- 2 7.73e+0  0       6   e+0 N     
    ## 144 vpdmax                              7.05e- 1 7.66e+1  0       8   e+1 Y     
    ## 145 vpdmin                              1.03e- 3 2.53e+1  0       3   e+1 Y     
    ## 146 vs                                  7.26e- 1 1.01e+1  0       2   e+1 Y     
    ## 147 ws                                  8.00e- 1 1.35e+1  0       1.4 e+1 Y     
    ## 148 z0m                                 1.67e- 4 2.26e+0  1  e- 4 3   e+0 Y
