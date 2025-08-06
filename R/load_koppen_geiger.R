# Load Koppen-Geiger Climate Classification AKA koppen_geiger
# DOWNLOADED

load_koppen_geiger <- function(
    save_dir = "raw_data/koppen_geiger"
) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

    # Skip if files already exist
    if (length(list.files(save_dir, recursive = TRUE)) > 0) {
        message(
            "Skipping Köppen-Geiger download - files already exist in ",
            save_dir
        )
        return(list.files(
            save_dir,
            full.names = TRUE,
            recursive = TRUE
        ))
    }

    tryCatch(
        {
            download_koppen_geiger(
                data_resolution = "0.5",
                time_period = "Present",
                directory_to_save = save_dir,
                acknowledgement = TRUE,
                download = TRUE,
                remove_command = TRUE,
                unzip = TRUE,
                remove_zip = FALSE,
                hash = FALSE
            )
        },
        error = function(e) {
            message("Failed to download Köppen-Geiger dataset: ", e$message)
        }
    )

    # Return all files downloaded and unzipped in that directory
    returned_files <- list.files(
        save_dir,
        full.names = TRUE,
        recursive = TRUE
    )

    return(returned_files)
}
