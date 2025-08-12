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
        "languageserver",
        "terra",
        "purrr",
        "stringr",
        "tibble"
    ),
    error = "continue", # continue on error
    memory = "transient", # use transient memory
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
        download_and_unzip_prism(save_dir = "raw_data/prism")
    ),

    tar_target(
        terraclimate,
        load_terraclimate(save_dir = "raw_data/terraclimate")
    ),

    tar_target(
        tri,
        load_tri(save_dir = "raw_data/tri")
    ),

    # Processing

    tar_target(
        modis_clean_log,
        build_modis_clean(
            modis_vars = list(
                MOD09A1 = c(
                    #   "sur_refl_b01",
                    #   "sur_refl_b02",
                    #   "sur_refl_b03",
                    "sur_refl_b04",
                    "sur_refl_b05",
                    "sur_refl_b06",
                    "sur_refl_b07",
                    "sur_refl_qc500m",
                    "sur_refl_day_of_year"
                ),
                MOD11A2 = c("LST_Day_1km", "LST_Night_1km"),
                MOD13A3 = c("NDVI", "EVI", "Pixel_reliability"),
                VNP13A2 = c(
                    "Gap_Filled_DNB_BRDF-Corrected_NTL",
                    "QF_Cloud_Mask",
                    "Snow_Flag",
                    "DNB_Lunar_Irradiance"
                )
            ),
            raw_dir = "raw_data/modis",
            out_dir = "clean_data/modis_clean",
            start_date = "2010-01-01",
            end_date = "2024-12-31",
            fun_agg = "mean",
            overwrite = TRUE,
            verbose = TRUE
        ),
        format = "rds" # saves the returned log as an RDS
    )
)
