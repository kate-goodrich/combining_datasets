# Download Parameter Elevation Regression on Independent Slopes Model (PRISM) AKA prism
# DOWNLOADED

load_prism <- function(
    save_dir = "/ddn/gs1/group/set/chords/combining_datasets/raw_data/prism",
    data_subdir = "data_files"
) {
    elements1 <- c("ppt", "tmin", "tmax", "tmean", "tdmean", "vpdmin", "vpdmax")
    elements_sol <- c("solslope", "soltotal", "solclear", "soltrans")

    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

    # Download regular elements
    for (element in elements1) {
        tryCatch(
            {
                download_prism(
                    time = "201001",
                    element = element,
                    data_type = c("ts", "normals_800", "normals"),
                    format = c("nc", "asc", "grib2"),
                    directory_to_save = save_dir,
                    acknowledgement = TRUE,
                    download = TRUE,
                    remove_command = TRUE,
                    hash = FALSE
                )
                message(paste("Downloaded PRISM element:", element))
            },
            error = function(e) {
                message(
                    "Failed to download PRISM element ",
                    element,
                    ": ",
                    e$message
                )
            }
        )
    }

    # Download solar elements
    for (element in elements_sol) {
        tryCatch(
            {
                download_prism(
                    time = "01",
                    element = element,
                    data_type = c("normals"),
                    format = c("nc", "asc", "grib2"),
                    directory_to_save = save_dir,
                    acknowledgement = TRUE,
                    download = TRUE,
                    remove_command = TRUE,
                    hash = FALSE
                )
                message(paste("Downloaded PRISM solar element:", element))
            },
            error = function(e) {
                message(
                    "Failed to download PRISM solar element ",
                    element,
                    ": ",
                    e$message
                )
            }
        )
    }

    # Unzip all .zip files
    zip_files <- list.files(save_dir, pattern = "\\.zip$", full.names = TRUE)
    extract_dir <- file.path(save_dir, data_subdir)
    dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

    for (zip_file in zip_files) {
        tryCatch(
            {
                unzip(zip_file, exdir = extract_dir)
                file.remove(zip_file)
                message(paste("Unzipped and removed:", zip_file))
            },
            error = function(e) {
                message("Failed to unzip: ", zip_file, " - ", e$message)
            }
        )
    }

    return(list(
        extracted_dir = extract_dir
    ))
}
