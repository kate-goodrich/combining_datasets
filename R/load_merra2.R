# Download NASA Modern-Era Retrospective analysis for Research and Applications, Version 2 (MERRA-2) AKA merra2
# DOWNLOADED

load_merra2 <- function(
    save_dir = "/ddn/gs1/group/set/chords/combining_datasets/raw_data/merra2",
    products = c(
        "tavg1_2d_aer_Nx",
        "tavg1_2d_flx_Nx",
        "tavg1_2d_lfo_Nx",
        "tavg1_2d_lnd_Nx",
        "tavg1_2d_rad_Nx",
        "tavg1_2d_slv_Nx"
    ),
    start_date = "2010-01-01",
    end_date = "2024-12-31"
) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    all_downloaded_files <- list()

    for (prod in products) {
        # Skip download if files for this product already exist
        existing_files <- list.files(
            save_dir,
            pattern = prod,
            full.names = TRUE,
            recursive = TRUE
        )

        if (length(existing_files) > 0) {
            message(
                "Skipping MERRA-2 download for ",
                prod,
                " - files already exist."
            )
            all_downloaded_files[[prod]] <- existing_files
            next
        }

        tryCatch(
            {
                download_merra2(
                    collection = prod,
                    date = c(start_date, end_date),
                    directory_to_save = save_dir,
                    acknowledgement = TRUE,
                    download = TRUE,
                    remove_command = TRUE,
                    hash = FALSE
                )
                message("MERRA-2 download complete for: ", prod)
            },
            error = function(e) {
                message(
                    "Failed to download MERRA-2 product ",
                    prod,
                    ": ",
                    e$message
                )
            }
        )

        downloaded <- list.files(
            save_dir,
            pattern = prod,
            full.names = TRUE,
            recursive = TRUE
        )
        all_downloaded_files[[prod]] <- downloaded
    }

    return(all_downloaded_files)
}
