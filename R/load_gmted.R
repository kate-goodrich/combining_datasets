# Download USGS6 Global Multi-resolution Terrain Elevation Data (GMTED2010) AKA gmted
# DOWNLOADED

load_gmted <- function(
    statistics = c(
        "Breakline Emphasis",
        "Systematic Subsample",
        "Median Statistic",
        "Minimum Statistic",
        "Mean Statistic",
        "Maximum Statistic",
        "Standard Deviation Statistic"
    ),
    resolution = "30 arc-seconds",
    save_dir = "/ddn/gs1/group/set/chords/combining_datasets/raw_data/gmted"
) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

    for (stat in statistics) {
        tryCatch(
            {
                download_gmted(
                    statistic = stat,
                    resolution = resolution,
                    directory_to_save = save_dir,
                    acknowledgement = TRUE,
                    download = TRUE,
                    remove_command = TRUE,
                    unzip = TRUE,
                    remove_zip = FALSE,
                    hash = FALSE
                )
                message("Downloaded GMTED: ", stat)
            },
            error = function(e) {
                message("Failed to download GMTED (", stat, "): ", e$message)
            }
        )
    }

    return(list(
        extracted_dir = save_dir
    ))
}
