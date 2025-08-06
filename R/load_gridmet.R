# Load Climatology Lab GridMet AKA gridmet
# DOWNLOADED

load_gridmet <- function(
    save_dir = "raw_data/gridmet",
    years = 2010:2024,
    variables = c(
        "sph",
        "pr",
        "rmin",
        "rmax",
        "srad",
        "tmmn",
        "tmmx",
        "vs",
        "th",
        "pet",
        "etr"
    )
) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

    for (year in as.character(years)) {
        for (var in variables) {
            tryCatch(
                {
                    download_gridmet(
                        variables = var,
                        year = year,
                        directory_to_save = save_dir,
                        acknowledgement = TRUE,
                        download = TRUE,
                        remove_command = TRUE,
                        hash = FALSE
                    )
                },
                error = function(e) {
                    message(paste(
                        "Error downloading",
                        var,
                        "in",
                        year,
                        ":",
                        e$message
                    ))
                }
            )
        }
    }

    # Return all downloaded files (assumed to be .nc files)
    returned_files <- list.files(
        save_dir,
        pattern = "\\.nc$",
        full.names = TRUE,
        recursive = TRUE
    )

    return(returned_files)
}
