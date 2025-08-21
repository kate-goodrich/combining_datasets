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


# HMS -------------------------------------------------------------------

hms_fire_exposure(
    hms_dir = "clean_data/hms_clean",
    level = "tract",
    agg = "monthly",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    write_csv = "summary_sets/monthly_tract_hms.csv"
)
