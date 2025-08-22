load_tri <- function(
    years = 2010:2024,
    save_dir = "/ddn/gs1/group/set/chords/combining_datasets/raw_data/tri"
) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

    for (yr in years) {
        # Check if any file for the year exists
        existing <- list.files(
            save_dir,
            pattern = as.character(yr),
            full.names = TRUE,
            recursive = TRUE
        )

        if (length(existing) > 0) {
            message("Skipping TRI for year ", yr, " - files already exist.")
            next
        }

        tryCatch(
            {
                download_tri(
                    year = yr,
                    directory_to_save = save_dir,
                    acknowledgement = TRUE,
                    download = TRUE,
                    remove_command = TRUE,
                    hash = FALSE
                )
                message("Downloaded TRI data for year: ", yr)
            },
            error = function(e) {
                message(
                    "Failed to download TRI data for year ",
                    yr,
                    ": ",
                    e$message
                )
            }
        )
    }

    return(list(
        directory = save_dir
    ))
}
