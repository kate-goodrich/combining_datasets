# Download MRLC1 Consortium National Land Cover Database (NLCD) AKA nlcd
# DOWNLOADED

load_nlcd <- function(
    base_dir = "/ddn/gs1/group/set/chords/combining_datasets/raw_data/nlcd",
    years = 2010:2023,
    products = c(
        "Land Cover",
        "Land Cover Change",
        "Land Cover Confidence",
        "Fractional Impervious Surface",
        "Impervious Descriptor",
        "Spectral Change Day of Year"
    )
) {
    dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

    for (prod in products) {
        prod_dir <- file.path(base_dir, gsub(" ", "_", prod))
        dir.create(prod_dir, recursive = TRUE, showWarnings = FALSE)

        for (yr in years) {
            tryCatch(
                {
                    download_nlcd(
                        product = prod,
                        year = yr,
                        directory_to_save = prod_dir,
                        acknowledgement = TRUE,
                        download = TRUE,
                        remove_command = TRUE,
                        unzip = TRUE,
                        remove_zip = FALSE,
                        hash = FALSE
                    )
                },
                error = function(e) {
                    message(paste(
                        "Error downloading",
                        prod,
                        "for year",
                        yr,
                        ":",
                        e$message
                    ))
                }
            )
        }
    }

    # Return list of all downloaded/unzipped files for targets to track
    downloaded_files <- list.files(
        base_dir,
        full.names = TRUE,
        recursive = TRUE
    )

    return(downloaded_files)
}
