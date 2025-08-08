# Load Packages
# make sure it's pulling from the container
.libPaths("/usr/local/lib/R/site-library")
library(targets)
library(tarchetypes)
library(amadeus)
library(dplyr)
library(tidyr)
library(archive)
library(languageserver)

# Load all files in the R directory
purrr::walk(list.files("R", full.names = TRUE, pattern = "\\.R$"), source)

# settings
tar_option_set(
    packages = c(
        "targets",
        "tarchetypes",
        "amadeus",
        "dplyr",
        "tidyr",
        "archive",
        "languageserver"
    ),
    library = "/usr/local/lib/R/site-library",
    format = "rds" # storage format
)


# Define targets
list(
    tar_target(
        gmted,
        load_gmted(save_dir = "raw_data/gmted")
    ),

    tar_target(
        gridmet,
        load_gridmet(save_dir = "raw_data/gridmet")
    ),

    tar_target(
        groads,
        load_groads()
    ),

    tar_target(
        hms,
        load_hms(save_dir = "raw_data/hms")
    ),

    tar_target(
        huc,
        load_huc(
            save_dir = "raw_data/huc",
            container_path = "raw_data/huc/NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z"
        )
    ),

    tar_target(
        koppen_geiger,
        load_koppen_geiger(save_dir = "raw_data/koppen_geiger")
    ),

    tar_target(
        merra2,
        load_merra2(save_dir = "raw_data/merra2")
    ),

    tar_target(
        modis,
        load_modis(dir = "raw_data/modis")
    ),

    tar_target(
        nlcd,
        load_nlcd(base_dir = "raw_data/nlcd")
    ),

    tar_target(
        prism,
        load_prism(save_dir = "raw_data/prism")
    ),

    tar_target(
        terraclimate,
        load_terraclimate(save_dir = "raw_data/terraclimate")
    ),

    tar_target(
        tri,
        load_tri(save_dir = "raw_data/tri")
    )
)

# run pipeline with targets::tar_make()
