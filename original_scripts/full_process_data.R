####### Process the data!!!!!!!! #######

# apptainer shell /ddn/gs1/group/set/chords/combining_datasets/container_combining_datasets.sif
# .libPaths("/usr/local/lib/R/site-library")

library(amadeus)
library(dplyr)
library(tidyr)
library(archive)
library(languageserver)
library(terra)
library(lubridate)
library(fs)


#### Process gmted
# PROCESSED - tif

statistics <- c(
    "Breakline Emphasis",
    "Systematic Subsample",
    "Median Statistic",
    "Minimum Statistic",
    "Mean Statistic",
    "Maximum Statistic",
    "Standard Deviation Statistic"
)

filenames <- c(
    "be30_grd",
    "ds30_grd",
    "md30_grd",
    "mi30_grd",
    "mn30_grd",
    "mx30_grd",
    "sd30_grd"
)

for (i in seq_along(statistics)) {
    stat <- statistics[i]
    file <- filenames[i]

    # Set input and output paths
    input_path <- file.path(
        "./raw_data/gmted/data_files",
        file
    )
    output_dir <- file.path("./clean_data/gmted_clean")
    output_file <- file.path(output_dir, paste0(file, "_clean.tif"))

    # Create output directory
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    # Process and write
    result <- process_gmted(
        variable = c(stat, "30 arc-seconds"),
        path = input_path
    )

    terra::writeRaster(result, output_file, overwrite = TRUE)
}

#### Process gridmet
# PROCESSED - tif

statistics <- c(
    "Reference alfalfa evaportranspiration",
    "Reference grass evaportranspiration",
    "Precipitation",
    "Maximum Near-Surface Relative Humidity",
    "Minimum Near-Surface Relative Humidity",
    "Near-Surface Specific Humidity",
    "Surface Downwelling Solar Radiation",
    "Wind direction at 10 m",
    "Minimum Near-Surface Air Temperature",
    "Maximum Near-Surface Air Temperature",
    "Wind speed at 10 m"
)

filenames <- c(
    "etr",
    "pet",
    "pr",
    "rmax",
    "rmin",
    "sph",
    "srad",
    "th",
    "tmmn",
    "tmmx",
    "vs"
)

for (i in seq_along(statistics)) {
    stat <- statistics[i]
    file <- filenames[i]

    input_path <- file.path("./raw_data/gridmet", file)
    output_dir <- file.path("./clean_data/gridmet_clean")
    output_file <- file.path(output_dir, paste0(file, "_clean.tif"))

    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    tryCatch(
        {
            message("Processing: ", file, " | Variable: ", stat)

            result <- process_gridmet(
                date = c("2010-01-01", "2024-12-31"),
                variable = stat,
                path = input_path
            )

            terra::writeRaster(result, output_file, overwrite = TRUE)
        },
        error = function(e) {
            message("Failed to process ", file, ": ", e$message)
        }
    )
}


#### Process groads

groads <- process_groads(
    path = "./raw_data/groads/data_files/groads-v1-americas-shp/gROADS-v1-americas.shp"
)

out_dir <- "./clean_data/groads_clean"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

terra::writeVector(
    groads,
    filename = "./clean_data/groads_clean/groads_clean.gpkg",
    overwrite = TRUE
)

#### Process hms

dir.create("./clean_data/hms_clean", recursive = TRUE, showWarnings = FALSE)

years <- 2010:2011
months <- 1:12

days_in_month <- c(
    "01" = 31,
    "02" = 28,
    "03" = 31,
    "04" = 30,
    "05" = 31,
    "06" = 30,
    "07" = 31,
    "08" = 31,
    "09" = 30,
    "10" = 31,
    "11" = 30,
    "12" = 31
)

for (year in years) {
    for (month in months) {
        month_str <- sprintf("%02d", month)
        last_day <- if (
            month_str == "02" &&
                ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0))
        ) {
            29
        } else {
            days_in_month[[month_str]]
        }

        message("Processing month: ", year, "-", month_str)

        all_days <- paste0(
            year,
            "-",
            month_str,
            "-",
            sprintf("%02d", 1:last_day)
        )
        month_results <- list()

        for (day in all_days) {
            hms_day <- tryCatch(
                process_hms(
                    date = c(day, day),
                    path = "./raw_data/hms/data_files"
                ),
                error = function(e) {
                    message("Skipped ", day, " due to error: ", e$message)
                    return(NULL)
                }
            )

            if (inherits(hms_day, "SpatVector")) {
                # Filter out invalid geometries
                valid <- is.valid(hms_day)
                hms_day <- hms_day[valid, ]
                if (nrow(hms_day) > 0) {
                    month_results[[length(month_results) + 1]] <- hms_day
                }
            }
        }

        # Combine all valid days
        if (length(month_results) > 0) {
            month_combined <- do.call(rbind, month_results)
            out_path <- sprintf(
                "./clean_data/hms_clean/hms_%d_%s.gpkg",
                year,
                month_str
            )
            writeVector(
                month_combined,
                out_path,
                filetype = "GPKG",
                overwrite = TRUE
            )
            message("Saved: ", out_path)
        } else {
            message("No valid smoke plumes in ", year, "-", month_str)
        }
    }
}

#### Process huc

# Create output directory
dir.create("clean_data/huc_clean", recursive = TRUE, showWarnings = FALSE)

# Path to your geodatabase
gdb_path <- "./raw_data/huc/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"

# Layers to process
layers_to_process <- c(
    "BurnAddWaterbody",
    "LandSea",
    "Catchment",
    "CatchmentSP",
    "NHDArea",
    "NHDWaterbody",
    "HUC12"
)

# Loop over each layer
for (layer in layers_to_process) {
    message("Processing layer: ", layer)

    tryCatch(
        {
            result <- process_huc(
                path = gdb_path,
                layer_name = layer,
                huc_level = "huc12"
            )

            # Save as geopackage for reliability
            out_file <- file.path(
                "clean_data/huc_clean",
                paste0(layer, ".gpkg")
            )
            terra::writeVector(
                result,
                out_file,
                filetype = "GPKG",
                overwrite = TRUE
            )

            message("Saved: ", out_file)
        },
        error = function(e) {
            warning(
                "Failed to process layer: ",
                layer,
                "\n  Reason: ",
                e$message
            )
        }
    )
}

#### Process koppen_geiger

# Create output directory
out_dir <- "./clean_data/koppen_geiger_clean"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# File paths to process
kg_files <- c(
    "raw_data/koppen_geiger/data_files/Beck_KG_V1_present_0p5.tif",
    "raw_data/koppen_geiger/data_files/Beck_KG_V1_present_0p083.tif",
    "raw_data/koppen_geiger/data_files/Beck_KG_V1_present_0p0083.tif",
    "raw_data/koppen_geiger/data_files/Beck_KG_V1_present_conf_0p5.tif",
    "raw_data/koppen_geiger/data_files/Beck_KG_V1_present_conf_0p083.tif",
    "raw_data/koppen_geiger/data_files/Beck_KG_V1_present_conf_0p0083.tif"
)

# Loop over each file
for (kg_path in kg_files) {
    message("Processing: ", kg_path)

    tryCatch(
        {
            kg <- process_koppen_geiger(path = kg_path)

            # Construct output filename
            filename <- basename(kg_path)
            out_path <- file.path(
                out_dir,
                gsub("\\.tif$", "_processed.tif", filename)
            )

            # Save result (assuming it's a SpatRaster)
            terra::writeRaster(kg, out_path, overwrite = TRUE)

            message("Saved to: ", out_path)
        },
        error = function(e) {
            warning("Failed to process ", kg_path, ": ", e$message)
        }
    )
}


#### Process merra2

# Output directory
out_dir <- "./clean_data/merra2_clean"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Years to process
years <- 2010:2024

# Define variable-folder map
merra2_vars <- list(
    # Aerosol Diagnostics
    BCSMASS = "tavg1_2d_aer_Nx",
    DUSMASS25 = "tavg1_2d_aer_Nx",
    TOTEXTTAU = "tavg1_2d_aer_Nx",

    # Surface Flux Diagnostics
    EVAP = "tavg1_2d_flx_Nx",
    PRECSNO = "tavg1_2d_flx_Nx",
    PRECTOTCORR = "tavg1_2d_flx_Nx",
    SPEED = "tavg1_2d_flx_Nx",
    Z0M = "tavg1_2d_flx_Nx",

    # Land Surface Forcings
    LWGAB = "tavg1_2d_lfo_Nx",

    # Land Surface Diagnostics
    GRN = "tavg1_2d_lnd_Nx",

    # Radiation Diagnostics
    ALBEDO = "tavg1_2d_rad_Nx",
    SWGDN = "tavg1_2d_rad_Nx",

    # Single-Level Diagnostics
    PS = "tavg1_2d_slv_Nx",
    QV2M = "tavg1_2d_slv_Nx",
    SLP = "tavg1_2d_slv_Nx",
    T2M = "tavg1_2d_slv_Nx",
    T2MDEW = "tavg1_2d_slv_Nx",
    TS = "tavg1_2d_slv_Nx",
    U10M = "tavg1_2d_slv_Nx",
    V10M = "tavg1_2d_slv_Nx"
)

# Loop over variables and years
for (var in names(merra2_vars)) {
    folder <- merra2_vars[[var]]
    path <- file.path("./raw_data/merra2", folder)

    for (year in years) {
        date_range <- c(
            paste0(year, "-01-01"),
            paste0(year, "-12-31")
        )

        message("Processing ", var, " for year ", year)

        tryCatch(
            {
                result <- process_merra2(
                    date = date_range,
                    variable = var,
                    path = path
                )

                # Save output
                out_file <- file.path(
                    out_dir,
                    paste0(var, "_", folder, "_", year, ".tif")
                )
                terra::writeRaster(result, out_file, overwrite = TRUE)

                message("Saved to: ", out_file)
            },
            error = function(e) {
                warning(
                    "Failed to process ",
                    var,
                    " for ",
                    year,
                    ": ",
                    e$message
                )
            }
        )
    }
}

#### Process modis

#### Process nlcd

#### Process prism

#### Process terraclimate

#### Process tri
