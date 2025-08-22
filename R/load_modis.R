load_modis <- function(
    dir = "/ddn/gs1/group/set/chords/combining_datasets/raw_data/modis",
    nasa_token = "YOUR_TOKEN_HERE"
) {
    available_years <- list(
        "MOD09A1" = 2010:2024,
        "MOD11A2" = 2010:2024,
        "MOD13A3" = 2010:2024,
    )

    all_downloaded_files <- list()

    for (product in names(available_years)) {
        product_dir <- file.path(dir, product)
        dir.create(product_dir, recursive = TRUE, showWarnings = FALSE)

        years <- available_years[[product]]
        year_ranges <- lapply(
            years,
            function(y) c(sprintf("%s-01-01", y), sprintf("%s-12-31", y))
        )

        for (i in seq_along(years)) {
            year <- years[i]
            year_range <- year_ranges[[i]]

            # Skip if any file with the year string exists in the product directory
            existing <- list.files(
                product_dir,
                pattern = as.character(year),
                full.names = TRUE,
                recursive = TRUE
            )
            if (length(existing) > 0) {
                message(
                    "Skipping ",
                    product,
                    " for ",
                    year,
                    " - files already exist."
                )
                next
            }

            tryCatch(
                {
                    download_modis(
                        product = product,
                        version = "61",
                        horizontal_tiles = c(7, 13),
                        vertical_tiles = c(3, 6),
                        mod06_links = NULL,
                        nasa_earth_data_token = nasa_token,
                        date = year_range,
                        directory_to_save = product_dir,
                        acknowledgement = TRUE,
                        download = TRUE,
                        remove_command = TRUE,
                        hash = FALSE
                    )
                    message(
                        "Downloaded ",
                        product,
                        " for ",
                        paste(year_range, collapse = " - ")
                    )
                },
                error = function(e) {
                    message(
                        "Failed for ",
                        product,
                        " in ",
                        paste(year_range, collapse = " - "),
                        ": ",
                        e$message
                    )
                }
            )
        }

        downloaded_files <- list.files(
            product_dir,
            full.names = TRUE,
            recursive = TRUE
        )
        all_downloaded_files[[product]] <- downloaded_files
    }

    return(all_downloaded_files)
}
