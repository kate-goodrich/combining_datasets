load_terraclimate <- function(
    save_dir = "raw_data/terraclimate",
    years = c(2010, 2024),
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
) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    downloaded_files <- character(0)

    for (var in variables) {
        # Check if any matching .nc files for this variable already exist
        existing_files <- list.files(
            save_dir,
            pattern = paste0("^", var, ".*\\.nc$"),
            full.names = TRUE
        )

        if (length(existing_files) > 0) {
            message("Skipping ", var, " - files already exist.")
            downloaded_files <- c(downloaded_files, existing_files)
            next
        }

        tryCatch(
            {
                download_terraclimate(
                    variables = var,
                    year = years,
                    directory_to_save = save_dir,
                    acknowledgement = TRUE,
                    download = TRUE,
                    remove_command = TRUE,
                    hash = FALSE
                )

                # List files for this variable after downloading
                var_files <- list.files(
                    save_dir,
                    pattern = paste0("^", var, ".*\\.nc$"),
                    full.names = TRUE
                )
                downloaded_files <- c(downloaded_files, var_files)
            },
            error = function(e) {
                message(paste("Error downloading", var, ":", e$message))
            }
        )
    }

    return(downloaded_files)
}
