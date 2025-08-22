zip_handoffs <- function(
    handoffs_dir = "handoffs",
    zipfile = "handoffs.zip",
    junk_paths = FALSE, # keep FALSE to avoid name collisions
    exclude_patterns = c("\\.DS_Store$", "~$", "\\.tmp$")
) {
    if (!dir.exists(handoffs_dir)) {
        stop("handoffs directory does not exist: ", handoffs_dir)
    }

    # List files recursively
    files <- list.files(
        handoffs_dir,
        recursive = TRUE,
        full.names = TRUE,
        all.files = TRUE
    )
    files <- files[file.exists(files) & !dir.exists(files)]

    # Exclude the output zip if it sits under handoffs_dir, plus common temp files
    files <- files[normalizePath(files) != normalizePath(zipfile)]
    if (length(exclude_patterns)) {
        keep <- !Reduce(
            `|`,
            lapply(exclude_patterns, function(p) grepl(p, files))
        )
        files <- files[keep]
    }

    if (!length(files)) {
        warning("No files found to zip in ", handoffs_dir)
        return(invisible(NULL))
    }

    # Choose flags: -r recursive, -9 max compression, -X omit extra attributes
    flags <- if (isTRUE(junk_paths)) "-r9Xj" else "-r9X"

    # Create zip (overwrites if it exists)
    utils::zip(
        zipfile = zipfile,
        files = files,
        flags = flags
    )

    message("Created zip archive: ", normalizePath(zipfile))
    return(zipfile)
}
