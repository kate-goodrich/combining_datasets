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

# all functions are idempotent - delete the file if you want it to redo

# Define targets
list(
    #load gmted
    tar_target(
        gmted,
        load_gmted(save_dir = "raw_data/gmted")
    ),

    #load gridmet
    tar_target(
        gridmet,
        load_gridmet(save_dir = "raw_data/gridmet")
    ),

    #load groads
    tar_target(
        groads,
        load_groads()
    ),

    #load hms
    tar_target(
        hms,
        load_hms(save_dir = "raw_data/hms")
    ),

    #load huc
    tar_target(
        huc,
        load_huc(
            save_dir = "raw_data/huc",
            container_path = "raw_data/huc/NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z"
        )
    ),

    #load koppen
    tar_target(
        koppen_geiger,
        load_koppen_geiger(save_dir = "raw_data/koppen_geiger")
    ),

    #load merra2
    tar_target(
        merra2,
        load_merra2(save_dir = "raw_data/merra2")
    ),

    #load modis
    tar_target(
        modis,
        load_modis(dir = "raw_data/modis")
    ),

    #load nlcd
    tar_target(
        nlcd,
        load_nlcd(base_dir = "raw_data/nlcd")
    ),

    #load prism
    tar_target(
        prism,
        download_and_unzip_prism(save_dir = "raw_data/prism")
    ),

    #load terraclimate
    tar_target(
        terraclimate,
        load_terraclimate(save_dir = "raw_data/terraclimate")
    ),

    #load tri
    tar_target(
        tri,
        load_tri(save_dir = "raw_data/tri")
    ),

    ################## Processing ##################

    #process gmted
    tar_target(
        gmted_outputs,
        clean_gmted(),
        format = "file" # tracks all cleaned .tif files + gmted_log.csv
    ),

    #process gridmet
    tar_target(
        gridmet_files,
        clean_gridmet(),
        format = "file"
    ),

    #process groads
    tar_target(
        groads_files,
        clean_groads(),
        format = "file"
    ),

    #process hms
    tar_target(
        hms_files,
        clean_hms(years = 2010:2024, months = 1:12, overwrite = TRUE),
        format = "file"
    ),

    #process huc
    tar_target(
        huc_files,
        clean_huc_layers(),
        format = "file"
    ),

    #process koppen
    tar_target(
        kg_files_out,
        clean_koppen_geiger(),
        format = "file"
    ),

    #process merra2
    tar_target(
        merra2_files, # single target
        clean_merra2(
            years = 2011:2024,
            raw_dir = "./raw_data/merra2",
            out_dir = "./clean_data/merra2_clean",
            use_collection_by_year = TRUE,
            overwrite = TRUE,
            verbose = TRUE
        ),
        format = "file" # tracks all returned paths (GeoTIFFs + merra2_log.csv)
    ),

    #process modis
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
    ),

    #process nlcd
    tar_target(
        nlcd_files,
        clean_nlcd(),
        format = "file"
    ),

    #process prism
    tar_target(
        prism_files,
        clean_prism_normals(),
        format = "file"
    ),

    #process terraclimate
    tar_target(
        terraclimate_files,
        clean_terraclimate(),
        format = "file"
    ),

    #process tri
    tar_target(
        tri_files,
        clean_tri(),
        format = "file"
    )
)
