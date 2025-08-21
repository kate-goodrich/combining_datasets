zip_handoffs <- function(
    handoffs_dir = "handoffs",
    zipfile = "handoffs.zip"
) {
    # Ensure the directory exists
    if (!dir.exists(handoffs_dir)) {
        stop("handoffs directory does not exist: ", handoffs_dir)
    }

    # List all files (recursively, to catch subfolders)
    files <- list.files(handoffs_dir, recursive = TRUE, full.names = TRUE)

    if (length(files) == 0) {
        warning("No files found in ", handoffs_dir)
        return(invisible(NULL))
    }

    # Create the zip (overwrites if it exists)
    utils::zip(
        zipfile = zipfile,
        files = files,
        flags = "-r9Xj" # -r recursive, -9 max compression, -X omit extra attrs, -j junk paths
    )

    message("Created zip archive: ", normalizePath(zipfile))
    return(zipfile)
}
