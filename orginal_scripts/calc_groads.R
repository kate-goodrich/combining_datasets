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


# GROADS (static) -------------------------------------------------------

road_density_zonal(
    roads_gpkg = "clean_data/groads_clean/groads_clean.gpkg",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = "tract",
    write_csv = "summary_sets/static_tract_groads.csv"
)
