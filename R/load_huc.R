# Download USGS National Hydrography Dataset (NHD) AKA huc
# DOWNLOADED

load_huc <- function(
    save_dir = "/ddn/gs1/group/set/chords/combining_datasets/raw_data/huc",
    container_path = "/ddn/gs1/group/set/chords/combining_datasets/container_combining_datasets.sif"
) {
    # 1. Create download directory if needed
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

    # 2. Skip if any file already exists
    if (length(list.files(save_dir, recursive = TRUE)) > 0) {
        message(
            "Skipping HUC download and extraction - files already exist in ",
            save_dir
        )
        return(list(
            archive = NA,
            extracted_dir = save_dir
        ))
    }

    # 3. Download HUC using R wrapper
    tryCatch(
        {
            download_huc(
                region = c("Lower48", "Islands"),
                type = c("Seamless", "OceanCatchment"),
                directory_to_save = save_dir,
                acknowledgement = TRUE,
                download = TRUE,
                remove_command = TRUE,
                unzip = FALSE,
                hash = FALSE
            )
            message("HUC download complete.")
        },
        error = function(e) {
            message("Failed to download HUC data: ", e$message)
        }
    )

    # 4. Find the .7z archive to extract
    archive_file <- file.path(
        save_dir,
        "NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z"
    )
    if (!file.exists(archive_file)) {
        stop("Expected archive not found at: ", archive_file)
    }

    # 5. Build and run the Apptainer extraction command
    unzip_cmd <- c(
        "exec",
        container_path,
        "7z",
        "x",
        archive_file,
        paste0("-o", save_dir)
    )

    result <- tryCatch(
        {
            system2("apptainer", args = unzip_cmd, stdout = TRUE, stderr = TRUE)
        },
        error = function(e) {
            message("Failed to unzip HUC archive: ", e$message)
            return(NULL)
        }
    )

    # 6. Return for targets tracking
    return(list(
        archive = archive_file,
        extracted_dir = save_dir
    ))
}
