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
library(tibble)

# ---- Runtime/memory & dirs ----
terra::terraOptions(todisk = TRUE, memfrac = 0.6)

dir.create("summary_sets", showWarnings = FALSE, recursive = TRUE)
dir.create("logs", showWarnings = FALSE, recursive = TRUE)
bad_log <- file.path("logs", "modis_bad_tiles.txt")

log_bad <- function(paths, why = "unreadable") {
    if (length(paths) == 0) {
        return(invisible(NULL))
    }
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    lines <- sprintf("[%s] %s\t%s", timestamp, why, paths)
    cat(paste0(lines, collapse = "\n"), "\n", file = bad_log, append = TRUE)
    invisible(NULL)
}

# Cheap probe that triggers GDAL open without fully loading data
is_readable_raster <- function(fp) {
    tryCatch(
        {
            r <- terra::rast(fp)
            terra::nlyr(r) >= 1
        },
        error = function(e) FALSE
    )
}

# ---- Inputs ----
counties <- st_read(
    "clean_data/county_census/canonical_2024.gpkg",
    layer = "counties_500k",
    quiet = TRUE
) |>
    st_make_valid()

# Map of variable directories -> output label -> scale factor
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
    "clean_data/modis_clean/MOD09A1/sur_refl_day_of_year",
    "MOD09A1_sur_refl_doy",
    1.0,

    "clean_data/modis_clean/MOD11A2/LST_Day_1km",
    "MOD11A2_LST_Day_1km_K",
    0.02,
    "clean_data/modis_clean/MOD11A2/LST_Night_1km",
    "MOD11A2_LST_Night_1km_K",
    0.02,

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
            date = as.Date(stringr::str_extract(
                basename(path),
                "\\d{4}-\\d{2}-\\d{2}"
            )),
            year = as.integer(format(date, "%Y"))
        ) |>
        arrange(date) |>
        mutate(read_ok = purrr::map_lgl(path, is_readable_raster)) |>
        (\(df) {
            bad <- dplyr::filter(df, !read_ok)
            if (nrow(bad) > 0) {
                log_bad(bad$path, why = "readStart failure or not a raster")
            }
            dplyr::filter(df, read_ok)
        })() |>
        select(-read_ok)
}


extract_means_factory <- function(counties_in_crs) {
    function(fp) {
        r <- terra::rast(fp)
        vals <- exactextractr::exact_extract(
            r,
            counties_in_crs,
            fun = "mean",
            progress = FALSE
        )
        tibble(
            geoid = counties_in_crs$geoid,
            mean_raw = as.numeric(vals)
        )
    }
}

# Retry wrapper for transient IO issues
safe_extract_means <- function(extract_means, fp, tries = 3, sleep0 = 0.5) {
    for (i in seq_len(tries)) {
        res <- try(extract_means(fp), silent = TRUE)
        if (!inherits(res, "try-error")) {
            return(res)
        }
        Sys.sleep(sleep0 * i)
    }
    log_bad(fp, why = sprintf("retry_exhausted_%dx", tries))
    tibble(geoid = character(), mean_raw = double()) # empty -> harmless on unnest
}

summarize_modis_dir <- function(
    var_dir,
    variable_label,
    scale_factor,
    counties_ll
) {
    files <- index_modis_files(var_dir)

    if (nrow(files) == 0) {
        message("No readable files in: ", var_dir)
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
        mutate(
            res = purrr::map(path, ~ safe_extract_means(extract_means, .x))
        ) |>
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

# ---- Save ----
readr::write_csv(
    all_annual,
    "summary_sets/annual_county_modis.csv"
)

# ---- Helpful print ----
all_annual |>
    count(variable, year, name = "n_counties") |>
    arrange(variable, year) |>
    print(n = 50)

message("Done. If any tiles were skipped, see: ", bad_log)
