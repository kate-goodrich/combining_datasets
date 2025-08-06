# Download NOAA Hazard Mapping System Fire and Smoke Product AKA hms
# DOWNLOADED

load_hms <- function(
    save_dir = "/ddn/gs1/group/set/chords/combining_datasets/raw_data/hms",
    date_range = c("2010-01-01", "2024-12-31"),
    format = "Shapefile"
) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

    tryCatch(
        {
            download_hms(
                data_format = format,
                date = date_range,
                directory_to_save = save_dir,
                acknowledgement = TRUE,
                download = TRUE,
                remove_command = TRUE,
                unzip = TRUE,
                remove_zip = TRUE,
                hash = FALSE
            )
            message("HMS smoke data download and extraction complete.")
        },
        error = function(e) {
            message("Failed to download HMS smoke data: ", e$message)
        }
    )

    return(list(
        extracted_dir = save_dir
    ))
}
