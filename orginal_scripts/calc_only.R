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


# TRI -------------------------------------------------------------------

tri_county_monthly <- summarise_tri_air_totals(
    tri_dir = "clean_data/tri_clean",
    county_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = "county",
    agg = "monthly"
)
readr::write_csv(tri_county_monthly, "summary_sets/monthly_county_tri.csv")

tri_tract_annual <- summarise_tri_air_totals(
    tri_dir = "clean_data/tri_clean",
    county_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = "tract",
    agg = "annual"
)
readr::write_csv(tri_tract_annual, "summary_sets/annual_tract_tri.csv")

tri_tract_monthly <- summarise_tri_air_totals(
    tri_dir = "clean_data/tri_clean",
    county_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = "tract",
    agg = "monthly"
)
readr::write_csv(tri_tract_monthly, "summary_sets/monthly_tract_tri.csv")
