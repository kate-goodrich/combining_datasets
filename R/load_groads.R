# Download NASA SEDAC Global Roads Open Access Data Set AKA groads
# DOWNLOADED

load_groads <- function(
    download_dir = "/ddn/gs1/group/set/chords/combining_datasets/raw_data/groads",
    unzip_dir = "/ddn/gs1/group/set/chords/combining_datasets/raw_data/groads",
    zip_filename = "groads/groads_v1_americas_shp.zip"
) {
    tryCatch(
        {
            download_groads(
                data_region = "Americas",
                data_format = "Shapefile",
                directory_to_save = download_dir,
                acknowledgement = TRUE,
                download = TRUE,
                remove_command = TRUE,
                unzip = FALSE,
                remove_zip = FALSE,
                hash = FALSE
            )
            message("Groads download complete.")
        },
        error = function(e) {
            message("Failed to download Groads: ", e$message)
        }
    )

    zip_path <- file.path(download_dir, zip_filename)

    if (file.exists(zip_path)) {
        dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)
        tryCatch(
            {
                unzip(zip_path, exdir = unzip_dir)
                message("Groads unzipped successfully.")

                # Delete the zip file
                file.remove(zip_path)
                message("Groads zip file removed.")
            },
            error = function(e) {
                message("Failed to unzip Groads: ", e$message)
            }
        )
    } else {
        message("Groads zip file not found at: ", zip_path)
    }

    return(list(
        extracted_dir = unzip_dir
    ))
}
