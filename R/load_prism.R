download_and_unzip_prism <- function(
    save_dir = "/ddn/gs1/group/set/chords/combining_datasets/raw_data/prism",
    elements = c(
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
    ),
    months = sprintf("%02d", 1:12)
) {
    # Create output directory
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

    # Check if data files already exist
    unzip_dir <- file.path(save_dir, "data_files")
    existing_files <- list.files(
        unzip_dir,
        pattern = "\\.(bil|nc|asc|grib2)$",
        full.names = TRUE
    )

    if (length(existing_files) > 0) {
        message(
            "PRISM files already exist in ",
            unzip_dir,
            ". Skipping download."
        )
        return(list(
            zip_dir = save_dir,
            extracted_dir = unzip_dir
        ))
    }

    # Loop through all elements and months
    for (element in elements) {
        for (month in months) {
            message("Downloading ", element, " for month ", month)
            tryCatch(
                {
                    download_prism(
                        time = month,
                        element = element,
                        data_type = "normals_800",
                        format = c("nc", "asc", "grib2"),
                        directory_to_save = save_dir,
                        acknowledgement = TRUE,
                        download = TRUE,
                        remove_command = TRUE,
                        hash = FALSE
                    )
                },
                error = function(e) {
                    message(
                        "Failed to download ",
                        element,
                        " for month ",
                        month,
                        ": ",
                        e$message
                    )
                }
            )
        }
    }

    # Unzip downloaded files
    zip_files <- list.files(
        save_dir,
        pattern = "\\.zip$",
        full.names = TRUE
    )
    dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)

    for (f in zip_files) {
        tryCatch(
            {
                unzip(f, exdir = unzip_dir)
            },
            error = function(e) {
                message("Failed to unzip ", f, ": ", e$message)
            }
        )
    }

    return(list(
        zip_dir = save_dir,
        extracted_dir = unzip_dir
    ))
}
