load_zcta <- function(
    year = 2024,
    save_dir = "raw_data/county_censes_zip",
    overwrite = FALSE,
    verbose = TRUE
) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

    # TIGER/Line ZCTA5 (2020 vintage) path pattern used by the Census for 2021+.
    # Example: https://www2.census.gov/geo/tiger/TIGER2024/ZCTA520/tl_2024_us_zcta520.zip
    zip_url <- sprintf(
        "https://www2.census.gov/geo/tiger/TIGER%1$d/ZCTA520/tl_%1$d_us_zcta520.zip",
        as.integer(year)
    )

    zip_path <- file.path(save_dir, sprintf("tl_%d_us_zcta520.zip", year))
    extract_dir <- file.path(save_dir, sprintf("tl_%d_us_zcta520", year))
    shp_path <- file.path(extract_dir, sprintf("tl_%d_us_zcta520.shp", year))

    # Skip if we already have an extracted shapefile and not overwriting
    if (!overwrite && file.exists(shp_path)) {
        if (isTRUE(verbose)) {
            message("[load_zcta] Using cached shapefile: ", shp_path)
        }
        return(normalizePath(shp_path))
    }

    # Download (if needed)
    if (overwrite || !file.exists(zip_path)) {
        if (isTRUE(verbose)) {
            message("[load_zcta] Downloading: ", zip_url)
        }
        utils::download.file(
            zip_url,
            destfile = zip_path,
            mode = "wb",
            quiet = !verbose
        )
    } else if (isTRUE(verbose)) {
        message("[load_zcta] Using cached ZIP: ", zip_path)
    }

    # Recreate extract dir if overwriting
    if (overwrite && dir.exists(extract_dir)) {
        unlink(extract_dir, recursive = TRUE, force = TRUE)
    }
    dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

    # Unzip
    utils::unzip(zip_path, exdir = extract_dir)

    # Find the .shp (should be exactly one)
    shp_found <- list.files(extract_dir, pattern = "\\.shp$", full.names = TRUE)
    if (length(shp_found) == 0L) {
        stop(
            "[load_zcta] No shapefile (.shp) found after unzip in: ",
            extract_dir
        )
    }
    if (length(shp_found) > 1L && isTRUE(verbose)) {
        message(
            "[load_zcta] Multiple .shp files found; using the first: ",
            shp_found[1]
        )
    }

    return(normalizePath(shp_found[1]))
}
