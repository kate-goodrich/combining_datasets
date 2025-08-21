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
library(arrow)
library(tidyselect)
library(tools)

# source your functions
purrr::walk(list.files("R", full.names = TRUE, pattern = "\\.R$"), source)

# ----------------------------
# Run pipelines directly
# ----------------------------

# county annual
county_annual_long <- build_exposure_long(
    agg = "annual",
    level = "county",
    write_csv = "handoffs/county_annual_long/county_annual.csv",
    write_parquet = "handoffs/county_annual_long/county_annual.parquet"
)
county_annual_layers <- build_exposure_wide_layers(
    data = county_annual_long,
    agg = "annual",
    level = "county",
    output_dir = "handoffs/county_annual_wide"
)

# county monthly
county_monthly_long <- build_exposure_long(
    agg = "monthly",
    level = "county",
    write_csv = "handoffs/county_monthly_long/county_monthly.csv",
    write_parquet = "handoffs/county_monthly_long/county_monthly.parquet"
)
county_monthly_layers <- build_exposure_wide_layers(
    data = county_monthly_long,
    agg = "monthly",
    level = "county",
    output_dir = "handoffs/county_monthly_wide"
)

# tract annual
tract_annual_long <- build_exposure_long(
    agg = "annual",
    level = "tract",
    write_csv = "handoffs/tract_annual_long/tract_annual.csv",
    write_parquet = "handoffs/tract_annual_long/tract_annual.parquet"
)
tract_annual_layers <- build_exposure_wide_layers(
    data = tract_annual_long,
    agg = "annual",
    level = "tract",
    output_dir = "handoffs/tract_annual_wide"
)

# tract monthly
tract_monthly_long <- build_exposure_long(
    agg = "monthly",
    level = "tract",
    write_csv = "handoffs/tract_monthly_long/tract_monthly.csv",
    write_parquet = "handoffs/tract_monthly_long/tract_monthly.parquet"
)
tract_monthly_layers <- build_exposure_wide_layers(
    data = tract_monthly_long,
    agg = "monthly",
    level = "tract",
    output_dir = "handoffs/tract_monthly_wide"
)
