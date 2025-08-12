####### Process the data!!!!!!!! #######

# apptainer shell /ddn/gs1/group/set/chords/combining_datasets/container_combining_datasets.sif
.libPaths("/usr/local/lib/R/site-library")

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

years <- 2010:2024
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

            # Save result
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

years <- 2011:2024

# Deduplicated variable -> folder map (removed duplicates and typos)
merra2_vars <- list(
    # Aerosol Diagnostics
    BCSMASS = "tavg1_2d_aer_Nx",
    DUSMASS25 = "tavg1_2d_aer_Nx",
    SSSMASS = "tavg1_2d_aer_Nx",
    TOTEXTTAU = "tavg1_2d_aer_Nx",

    # Surface Flux Diagnostics
    EVAP = "tavg1_2d_flx_Nx",
    PRECSNO = "tavg1_2d_flx_Nx",
    PRECTOTCORR = "tavg1_2d_flx_Nx",
    SPEED = "tavg1_2d_flx_Nx",
    Z0M = "tavg1_2d_flx_Nx",
    PBLH = "tavg1_2d_flx_Nx",

    # Land Surface Forcings
    LWGAB = "tavg1_2d_lfo_Nx",

    # Land Surface Diagnostics
    GRN = "tavg1_2d_lnd_Nx",
    GWETROOT = "tavg1_2d_lnd_Nx",
    LAI = "tavg1_2d_lnd_Nx",

    # Radiation Diagnostics
    ALBEDO = "tavg1_2d_rad_Nx",
    SWGDN = "tavg1_2d_rad_Nx",
    LWGNT = "tavg1_2d_rad_Nx",
    CLDTOT = "tavg1_2d_rad_Nx",

    # Single-Level Diagnostics
    QV2M = "tavg1_2d_slv_Nx",
    PS = "tavg1_2d_slv_Nx",
    SLP = "tavg1_2d_slv_Nx",
    T2M = "tavg1_2d_slv_Nx",
    T2MDEW = "tavg1_2d_slv_Nx",
    U10M = "tavg1_2d_slv_Nx",
    V10M = "tavg1_2d_slv_Nx",
    TS = "tavg1_2d_slv_Nx"
)

# Optional: pick collection by year (useful if your process_merra2 allows passing it)
collection_for_year <- function(y) {
    if (y <= 1991) {
        "MERRA2_100"
    } else if (y <= 2000) {
        "MERRA2_200"
    } else if (y <= 2010) {
        "MERRA2_300"
    } else {
        "MERRA2_400"
    }
}


for (var in names(merra2_vars)) {
    folder <- merra2_vars[[var]]
    path <- file.path("./raw_data/merra2", folder)

    for (year in years) {
        date_range <- c(sprintf("%d-01-01", year), sprintf("%d-12-31", year))
        message("Processing ", var, " for year ", year)

        # Fast existence check so 2010 isn’t “empty”
        files_year <- list.files(
            path,
            pattern = paste0("\\.", year, "(\\d{2}){2}\\.nc4$"),
            full.names = TRUE,
            recursive = TRUE
        )
        if (length(files_year) == 0) {
            warning(
                "No files found for ",
                var,
                " in ",
                year,
                " under ",
                path,
                ". Skipping."
            )
            next
        }

        # If your process_merra2() accepts collection, pass it here:
        # coll <- collection_for_year(year)

        res <- try(
            process_merra2(
                date = date_range,
                variable = var,
                path = path
            ),
            silent = TRUE
        )

        if (inherits(res, "try-error")) {
            warning(
                "Failed to process ",
                var,
                " for ",
                year,
                ": ",
                attr(res, "condition")$message
            )
            next
        }
        if (is.null(res) || (inherits(res, "SpatRaster") && nlyr(res) == 0)) {
            warning(
                "Empty result for ",
                var,
                " in ",
                year,
                ". Check input files and collection."
            )
            next
        }

        out_file <- file.path(
            out_dir,
            paste0(var, "_", folder, "_", year, ".tif")
        )
        terra::writeRaster(res, out_file, overwrite = TRUE)
        message("Saved to: ", out_file)
    }
}


#### Process modis

# Variables for each MODIS/VIIRS product
modis_vars <- list(
    MOD09A1 = c(
        "sur_refl_b01",
        "sur_refl_b02",
        "sur_refl_b03",
        "sur_refl_b04",
        "sur_refl_b05",
        "sur_refl_b06",
        "sur_refl_b07",
        "sur_refl_qc500m",
        "sur_refl_day_of_year"
    ),
    MOD11A2 = c(
        "LST_Day_1km",
        "LST_Night_1km"
    ),
    MOD13A3 = c(
        "NDVI",
        "EVI",
        "Pixel_reliability"
    ),
    VNP13A2 = c(
        "Gap_Filled_DNB_BRDF-Corrected_NTL",
        "QF_Cloud_Mask",
        "Snow_Flag",
        "DNB_Lunar_Irradiance"
    )
)

out_dir <- "./clean_data/modis_clean"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Create a vector of all available dates
dates <- seq(as.Date("2010-01-01"), as.Date("2024-12-31"), by = "1 day")


for (product in names(modis_vars)) {
    # Only get files for this product
    product_files <- list.files(
        "./raw_data/modis",
        pattern = paste0(product, ".*\\.hdf$"),
        full.names = TRUE,
        recursive = TRUE
    )

    for (d in dates) {
        # Ensure date is in YYYY-MM-DD format
        date_str <- format(as.Date(d), "%Y-%m-%d")

        message("Processing ", var, " from ", product, " for date ", date_str)

        tryCatch(
            {
                r <- process_modis_merge(
                    path = product_files,
                    date = date_str,
                    subdataset = var,
                    fun_agg = "mean"
                )

                save_path <- file.path(
                    out_dir,
                    paste0(product, "_", var, "_", date_str, ".tif")
                )
                terra::writeRaster(r, save_path, overwrite = TRUE)
            },
            error = function(e) {
                message("Failed for ", var, " on ", date_str, ": ", e$message)
            }
        )
    }
}


#### Process nlcd

# Output base directory
base_out_dir <- "./clean_data/nlcd_clean"
dir.create(base_out_dir, recursive = TRUE, showWarnings = FALSE)

# Define products and years
products <- c(
    "Land Cover",
    "Land Cover Change",
    "Land Cover Confidence",
    "Fractional Impervious Surface",
    "Impervious Descriptor",
    "Spectral Change Day of Year"
)
years <- 2010:2023

# Loop through each product
for (prod in products) {
    # Clean product folder name
    prod_folder <- gsub(" ", "_", prod)
    prod_out_dir <- file.path(base_out_dir, prod_folder)
    dir.create(prod_out_dir, recursive = TRUE, showWarnings = FALSE)

    for (yr in years) {
        message("Processing ", prod, " for year ", yr)

        tryCatch(
            {
                # Run processing function
                result_raster <- process_nlcd(
                    path = file.path("./raw_data/nlcd", prod_folder),
                    year = yr
                )

                # Construct output file name
                out_filename <- paste0(prod_folder, "_", yr, "_processed.tif")
                out_path <- file.path(prod_out_dir, out_filename)

                # Save raster
                terra::writeRaster(result_raster, out_path, overwrite = TRUE)

                message("Saved to: ", out_path)
            },
            error = function(e) {
                warning("Skipping ", prod, " for year ", yr, ": ", e$message)
            }
        )
    }
}

#### Process prism

# Define elements and their corresponding M-codes
elements <- c(
    "ppt",
    "tmin",
    "tmax",
    "tmean",
    "tdmean",
    "vpdmin",
    "vpdmax",
    "solslope",
    "soltotal",
    "solclear",
    "soltrans"
)
m_codes <- c("M4", rep("M5", 6), rep("M3", 4))
names(m_codes) <- elements # Named vector for easy lookup

# Define time
months <- sprintf("%02d", 1:12)

# Directories
input_dir <- "./raw_data/prism/data_files"
output_dir <- "./clean_data/prism_clean"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Loop through each element and month
for (element in elements) {
    m_code <- m_codes[element] # Lookup correct M-code

    for (month in months) {
        # Construct filename using M-code
        filename <- paste0(
            "PRISM_",
            element,
            "_30yr_normal_800m",
            m_code,
            "_",
            month,
            "_bil.bil"
        )
        file_path <- file.path(input_dir, filename)

        if (!file.exists(file_path)) {
            warning("File not found: ", file_path)
            next
        }

        message("Processing ", element, " for month ", month)

        tryCatch(
            {
                prism_raster <- process_prism(
                    path = file_path,
                    element = element,
                    time = month
                )

                # Output filename
                out_filename <- paste0(element, "_", month, ".tif")
                out_path <- file.path(output_dir, out_filename)

                # Save
                terra::writeRaster(prism_raster, out_path, overwrite = TRUE)
                message("Saved to: ", out_path)
            },
            error = function(e) {
                warning("Failed to process ", file_path, ": ", e$message)
            }
        )
    }
}

#### Process terraclimate

output_dir <- "./clean_data/terraclimate_clean"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

variables = c(
    "aet",
    "def",
    "swe",
    "q",
    "soil",
    "PDSI",
    "pet",
    "ppt",
    "srad",
    "tmax",
    "tmin",
    "vap",
    "vpd",
    "ws"
)


for (var in variables) {
    message("Processing variable: ", var)

    tryCatch(
        {
            result <- process_terraclimate(
                date = c("2010-01-01", "2024-12-31"),
                variable = var,
                path = file.path("./raw_data/terraclimate/", var)
            )

            # Save the result
            out_file <- file.path(output_dir, paste0(var, "_processed.tif"))
            terra::writeRaster(result, out_file, overwrite = TRUE)

            message("Saved to: ", out_file)
        },
        error = function(e) {
            warning("Failed to process ", var, ": ", e$message)
        }
    )
}


#### Process tri

output_dir <- "./clean_data/tri_clean"
dir_create(output_dir, recurse = TRUE)

years <- 2010:2024
vars <- c(
    1, # YEAR
    12, # LATITUDE
    13, # LONGITUDE
    14, # HORIZONTAL DATUM
    20, # STANDARD FOREIGN PARENT CO NAME
    29, # SIC 6
    34, # NAICS 5
    36, # DOC_CTRL_NUM
    38, # ELEMENTAL METAL INCLUDED
    39, # TRI CHEMICAL/COMPOUND ID
    40, # CAS#
    47, # PBT
    48, # PFAS
    49, # FORM TYPE
    50, # UNIT OF MEASURE
    51, # 5.1 - FUGITIVE AIR
    52, # 5.2 - STACK AIR
    53, # 5.3 - WATER
    65, # ON-SITE RELEASE TOTAL
    88, # OFF-SITE RELEASE TOTAL
    107 # TOTAL RELEASES
)

for (yr in years) {
    message("Processing TRI data for year: ", yr)

    tryCatch(
        {
            tri_data <- process_tri(
                path = "./raw_data/tri",
                year = yr,
                variables = vars
            )

            if (!inherits(tri_data, "SpatVector")) {
                warning("No valid SpatVector returned for ", yr, "; skipping.")
                next
            }

            out_file <- file.path(
                output_dir,
                paste0("tri_", yr, "_processed.gpkg")
            )
            terra::writeVector(tri_data, out_file, overwrite = TRUE)
            message("Saved to: ", out_file)
        },
        interrupt = function(e) {
            message("Detected user interrupt; stopping after year ", yr, ".")
            break
        },
        error = function(e) {
            warning(
                "Failed to process TRI for year ",
                yr,
                ": ",
                conditionMessage(e)
            )
        }
    )
}
