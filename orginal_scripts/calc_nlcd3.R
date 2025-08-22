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

summarize_nlcd(
    level = "tract",
    agg = "annual",
    write_csv = "summary_sets/annual_tract_nlcd.csv"
)
