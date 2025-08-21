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


# MERRA2 ----------------------------------------------------------------

merra2_summary(
    tif_dir = "clean_data/merra2_clean",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = "tract",
    agg = "annual",
    write_csv = "summary_sets/annual_tract_merra2.csv"
)
