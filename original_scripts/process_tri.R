.libPaths("/usr/local/lib/R/site-library")

library(amadeus)
library(dplyr)
library(tidyr)
library(archive)
library(languageserver)
library(terra)
library(lubridate)
library(fs)


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
