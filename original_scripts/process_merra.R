# apptainer shell /ddn/gs1/group/set/chords/combining_datasets/container_combining_datasets.sif

.libPaths("/usr/local/lib/R/site-library")
library(targets)
library(tarchetypes)
library(amadeus)
library(dplyr)
library(tidyr)
library(archive)
library(languageserver)
library(terra)

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
