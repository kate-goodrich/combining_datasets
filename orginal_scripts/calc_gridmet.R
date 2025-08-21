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

# GRIDMET ---------------------------------------------------------------

gridmet_zonal(
    tif_dir = "clean_data/gridmet_clean",
    level = "tract",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    zone_layer = "tracts_500k",
    aggregate_to = "annual",
    chunk_size = 365,
    write_csv = "summary_sets/annual_tract_gridmet.csv"
)

gridmet_zonal(
    tif_dir = "clean_data/gridmet_clean",
    level = "tract",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    zone_layer = "tracts_500k",
    aggregate_to = "monthly",
    chunk_size = 365,
    write_csv = "summary_sets/monthly_tract_gridmet.csv"
)
