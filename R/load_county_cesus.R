download_counties_tracts <- function(
    year = 2024,
    scale = "500k",
    save_dir = "raw_data/county_census",
    overwrite = FALSE,
    verbose = TRUE
) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    year <- as.integer(year)

    base <- sprintf("https://www2.census.gov/geo/tiger/GENZ%d/shp", year)

    # County
    county_zip <- file.path(
        save_dir,
        sprintf("cb_%d_us_county_%s.zip", year, scale)
    )
    county_dir <- file.path(
        save_dir,
        sprintf("cb_%d_us_county_%s", year, scale)
    )
    county_shp <- file.path(
        county_dir,
        sprintf("cb_%d_us_county_%s.shp", year, scale)
    )
    county_url <- sprintf("%s/cb_%d_us_county_%s.zip", base, year, scale)

    # Tract
    tract_zip <- file.path(
        save_dir,
        sprintf("cb_%d_us_tract_%s.zip", year, scale)
    )
    tract_dir <- file.path(save_dir, sprintf("cb_%d_us_tract_%s", year, scale))
    tract_shp <- file.path(
        tract_dir,
        sprintf("cb_%d_us_tract_%s.shp", year, scale)
    )
    tract_url <- sprintf("%s/cb_%d_us_tract_%s.zip", base, year, scale)

    # Helper to download+unzip one layer
    .get <- function(url, zip_path, out_dir, shp_path) {
        if (!overwrite && file.exists(shp_path)) {
            if (isTRUE(verbose)) {
                message("[download_counties_tracts] Using cached: ", shp_path)
            }
            return(invisible(NULL))
        }
        if (overwrite || !file.exists(zip_path)) {
            if (isTRUE(verbose)) {
                message("[download_counties_tracts] Downloading: ", url)
            }
            utils::download.file(
                url,
                destfile = zip_path,
                mode = "wb",
                quiet = !verbose
            )
        } else if (isTRUE(verbose)) {
            message("[download_counties_tracts] Using cached ZIP: ", zip_path)
        }
        if (overwrite && dir.exists(out_dir)) {
            unlink(out_dir, recursive = TRUE, force = TRUE)
        }
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        utils::unzip(zip_path, exdir = out_dir)
    }

    .get(county_url, county_zip, county_dir, county_shp)
    .get(tract_url, tract_zip, tract_dir, tract_shp)

    if (!file.exists(county_shp)) {
        stop(
            "[download_counties_tracts] Missing county shapefile: ",
            county_shp
        )
    }
    if (!file.exists(tract_shp)) {
        stop("[download_counties_tracts] Missing tract shapefile: ", tract_shp)
    }

    list(
        county_shp = normalizePath(county_shp),
        tract_shp = normalizePath(tract_shp),
        year = year,
        scale = scale
    )
}
