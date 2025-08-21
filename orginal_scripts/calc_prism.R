# make sure to use container libs
.libPaths("/usr/local/lib/R/site-library")

# load packages you need for the calc functions
library(sf)
library(terra)
library(exactextractr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(readr)
library(units)
library(lubridate)

# source your functions
purrr::walk(list.files("R", full.names = TRUE, pattern = "\\.R$"), source)


# PRISM normals ---------------------------------------------------------

prism_normals_from_tifs(
    input_dir = "clean_data/prism_clean",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = "county",
    agg = "monthly",
    id_col = "geoid",
    write_csv = "summary_sets/normal_monthly_county_prism.csv"
)

prism_normals_from_tifs(
    input_dir = "clean_data/prism_clean",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = "tract",
    agg = "monthly",
    id_col = "geoid",
    write_csv = "summary_sets/normal_monthly_tract_prism.csv"
)

prism_normals_from_tifs(
    input_dir = "clean_data/prism_clean",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = "tract",
    agg = "annual",
    id_col = "geoid",
    write_csv = "summary_sets/normal_annual_tract_prism.csv"
)
