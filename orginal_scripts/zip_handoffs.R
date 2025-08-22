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


zip_handoffs(
    handoffs_dir = "handoffs",
    zipfile = "handoffs.zip",
    junk_paths = FALSE
)
