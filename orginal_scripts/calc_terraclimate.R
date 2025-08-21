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


# TerraClimate ----------------------------------------------------------

summarize_terraclimate(
    tif_dir = "clean_data/terraclimate_clean",
    county_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = "county",
    agg = "monthly",
    write_csv = "summary_sets/monthly_county_terraclimate.csv"
)

summarize_terraclimate(
    tif_dir = "clean_data/terraclimate_clean",
    county_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = "tract",
    agg = "annual",
    write_csv = "summary_sets/annual_tract_terraclimate.csv"
)

summarize_terraclimate(
    tif_dir = "clean_data/terraclimate_clean",
    county_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = "tract",
    agg = "monthly",
    write_csv = "summary_sets/monthly_tract_terraclimate.csv"
)
