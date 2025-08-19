.libPaths("/usr/local/lib/R/site-library")


# ---- Libraries ----
library(sf)
library(terra)
library(exactextractr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(readr)
library(rlang)

# ---- Inputs ----
counties <- st_read(
    "clean_data/county_census/canonical_2024.gpkg",
    layer = "counties_500k",
    quiet = TRUE
) |>
    st_make_valid()

# Map of variable directories -> output label -> scale factor
# Adjust or extend as needed.
vars <- tibble::tribble(
    ~dir,
    ~label,
    ~scale,
    "clean_data/modis_clean/MOD09A1/sur_refl_b01",
    "MOD09A1_sur_refl_b01",
    1e-4,
    "clean_data/modis_clean/MOD09A1/sur_refl_b02",
    "MOD09A1_sur_refl_b02",
    1e-4,
    "clean_data/modis_clean/MOD09A1/sur_refl_b03",
    "MOD09A1_sur_refl_b03",
    1e-4,
    "clean_data/modis_clean/MOD09A1/sur_refl_b04",
    "MOD09A1_sur_refl_b04",
    1e-4,
    "clean_data/modis_clean/MOD09A1/sur_refl_b05",
    "MOD09A1_sur_refl_b05",
    1e-4,
    "clean_data/modis_clean/MOD09A1/sur_refl_b06",
    "MOD09A1_sur_refl_b06",
    1e-4,
    "clean_data/modis_clean/MOD09A1/sur_refl_b07",
    "MOD09A1_sur_refl_b07",
    1e-4,
    # day_of_year is a utility layer (no scaling); averaging may or may not be useful—kept for completeness.
    "clean_data/modis_clean/MOD09A1/sur_refl_day_of_year",
    "MOD09A1_sur_refl_doy",
    1.0,
    # QC directory is intentionally omitted from averaging; we can use it later for masking.

    # MOD11A2 (8-day LST; units Kelvin after scaling)
    "clean_data/modis_clean/MOD11A2/LST_Day_1km",
    "MOD11A2_LST_Day_1km_K",
    0.02,
    "clean_data/modis_clean/MOD11A2/LST_Night_1km",
    "MOD11A2_LST_Night_1km_K",
    0.02,

    # MOD13A3 (monthly VI; unitless after scaling)
    "clean_data/modis_clean/MOD13A3/NDVI",
    "MOD13A3_NDVI",
    1e-4,
    "clean_data/modis_clean/MOD13A3/EVI",
    "MOD13A3_EVI",
    1e-4
)

# ---- Helpers ----

index_modis_files <- function(var_dir) {
    tibble(
        path = list.files(var_dir, pattern = "\\.tif$", full.names = TRUE)
    ) |>
        mutate(
            date = as.Date(str_extract(basename(path), "\\d{4}-\\d{2}-\\d{2}")),
            year = as.integer(format(date, "%Y"))
        ) |>
        arrange(date)
}

extract_means_factory <- function(counties_in_crs) {
    # returns a function(fp) that extracts area-weighted mean for polygons
    function(fp) {
        r <- terra::rast(fp)
        vals <- exactextractr::exact_extract(
            r,
            counties_in_crs,
            fun = "mean",
            progress = FALSE
        )
        tibble::tibble(
            geoid = counties_in_crs$geoid,
            mean_raw = as.numeric(vals)
        )
    }
}

summarize_modis_dir <- function(
    var_dir,
    variable_label,
    scale_factor,
    counties_ll
) {
    files <- index_modis_files(var_dir)

    if (nrow(files) == 0) {
        message("No files in: ", var_dir)
        return(tibble(
            variable = character(),
            geoid = character(),
            year = integer(),
            value = double()
        ))
    }

    r0 <- terra::rast(files$path[1])
    counties_in_crs <- sf::st_transform(counties_ll, terra::crs(r0))

    extract_means <- extract_means_factory(counties_in_crs)

    daily <- files |>
        mutate(res = purrr::map(path, extract_means)) |>
        tidyr::unnest(res) |>
        mutate(value = mean_raw * scale_factor) |>
        select(geoid, date, year, value)

    annual <- daily |>
        group_by(geoid, year) |>
        summarize(value = mean(value, na.rm = TRUE), .groups = "drop") |>
        mutate(variable = variable_label) |>
        relocate(variable, geoid, year, value)

    annual
}

# ---- Run for all variables and combine ----
all_annual <- vars |>
    mutate(
        tbl = purrr::pmap(
            list(dir, label, scale),
            ~ summarize_modis_dir(..1, ..2, ..3, counties)
        )
    ) |>
    pull(tbl) |>
    list_rbind()


all_annual_out <- bind_rows(all_annual)

# ---- Save ----
dir.create("summary_sets", showWarnings = FALSE, recursive = TRUE)
readr::write_csv(
    all_annual_out,
    "summary_sets/annual_county_modis.csv"
)

# Helpful print
all_annual_out |>
    count(variable, year, name = "n_counties") |>
    arrange(variable, year) |>
    print(n = 50)
