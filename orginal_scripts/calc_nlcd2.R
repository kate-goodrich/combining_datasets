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


# NLCD ------------------------------------------------------------------

zonal_means_from_tifs(
    input_dir = "clean_data/nlcd_clean",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = "county",
    agg = "monthly",
    id_col = "geoid",
    file_pattern = "_processed\\.tif$",
    write_csv = "summary_sets/monthly_county_nlcd.csv"
)
