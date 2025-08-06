# Download US EPA Toxic Release Inventory (TRI) Program AKA tri
# DOWNLOADED

load_tri <- function(
    years = 2010:2024,
    save_dir = "/ddn/gs1/group/set/chords/combining_datasets/raw_data/tri"
) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

    tryCatch(
        {
            download_tri(
                year = years,
                directory_to_save = save_dir,
                acknowledgement = TRUE,
                download = TRUE,
                remove_command = TRUE,
                hash = FALSE
            )
            message("TRI download complete.")
        },
        error = function(e) {
            message("Failed to download TRI data: ", e$message)
        }
    )

    return(list(
        directory = save_dir,
    ))
}
