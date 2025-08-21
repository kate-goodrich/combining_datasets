summarize_modis <- function(
    level = c("county", "tract"),
    agg = c("annual", "monthly"),
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    zone_layer = NULL,
    write_csv = NULL
) {
    level <- match.arg(level)
    agg <- match.arg(agg)

    if (is.null(zone_layer)) {
        zone_layer <- if (level == "county") "counties_500k" else "tracts_500k"
    }

    # Read zones
    zones_ll <- sf::st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid()

    # Directory for logs/output
    dir.create("summary_sets", showWarnings = FALSE, recursive = TRUE)
    dir.create("logs", showWarnings = FALSE, recursive = TRUE)
    bad_log <- file.path("logs", "modis_bad_tiles.txt")

    log_bad <- function(paths, why = "unreadable") {
        if (length(paths) == 0) {
            return(invisible(NULL))
        }
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
        lines <- sprintf("[%s] %s\t%s", timestamp, why, paths)
        cat(paste0(lines, collapse = "\n"), "\n", file = bad_log, append = TRUE)
        invisible(NULL)
    }

    is_readable_raster <- function(fp) {
        tryCatch(
            {
                r <- terra::rast(fp)
                terra::nlyr(r) >= 1
            },
            error = function(e) FALSE
        )
    }

    index_modis_files <- function(var_dir) {
        tibble(
            path = list.files(var_dir, pattern = "\\.tif$", full.names = TRUE)
        ) |>
            mutate(
                date = as.Date(str_extract(
                    basename(path),
                    "\\d{4}-\\d{2}-\\d{2}"
                )),
                year = as.integer(format(date, "%Y"))
            ) |>
            arrange(date) |>
            mutate(read_ok = purrr::map_lgl(path, is_readable_raster)) |>
            (\(df) {
                bad <- filter(df, !read_ok)
                if (nrow(bad) > 0) {
                    log_bad(bad$path, "readStart failure or not a raster")
                }
                filter(df, read_ok)
            })() |>
            select(-read_ok)
    }

    extract_means_factory <- function(zones_proj) {
        function(fp) {
            r <- terra::rast(fp)
            vals <- exactextractr::exact_extract(
                r,
                zones_proj,
                "mean",
                progress = FALSE
            )
            tibble(geoid = zones_proj$geoid, mean_raw = as.numeric(vals))
        }
    }

    safe_extract_means <- function(extract_means, fp, tries = 3, sleep0 = 0.5) {
        for (i in seq_len(tries)) {
            res <- try(extract_means(fp), silent = TRUE)
            if (!inherits(res, "try-error")) {
                return(res)
            }
            Sys.sleep(sleep0 * i)
        }
        log_bad(fp, sprintf("retry_exhausted_%dx", tries))
        tibble(geoid = character(), mean_raw = double())
    }

    summarize_modis_dir <- function(var_dir, variable_label, scale_factor) {
        files <- index_modis_files(var_dir)
        if (nrow(files) == 0) {
            return(tibble())
        }

        r0 <- terra::rast(files$path[1])
        zones_proj <- sf::st_transform(zones_ll, terra::crs(r0))
        extract_means <- extract_means_factory(zones_proj)

        daily <- files |>
            mutate(res = map(path, ~ safe_extract_means(extract_means, .x))) |>
            unnest(res) |>
            mutate(value = mean_raw * scale_factor) |>
            select(geoid, date, year, value)

        if (agg == "annual") {
            daily |>
                group_by(geoid, year) |>
                summarize(
                    value = mean(value, na.rm = TRUE),
                    .groups = "drop"
                ) |>
                mutate(variable = variable_label) |>
                relocate(variable, geoid, year, value)
        } else {
            daily |>
                mutate(month = lubridate::month(date)) |>
                group_by(geoid, year, month) |>
                summarize(
                    value = mean(value, na.rm = TRUE),
                    .groups = "drop"
                ) |>
                mutate(variable = variable_label) |>
                relocate(variable, geoid, year, month, value)
        }
    }

    # Variable metadata
    vars <- tibble::tribble(
        ~dir,
        ~label,
        ~scale,
        "clean_data/modis_clean/MOD09A1/sur_refl_b01",
        "MOD09A1_sur_refl_b01",
        1e-4,
        "clean_data/modis_clean/MOD09A1/sur_refl_b02",
        "MOD09A1_sur_refl_b02",
        1e-4,
        "clean_data/modis_clean/MOD09A1/sur_refl_b03",
        "MOD09A1_sur_refl_b03",
        1e-4,
        "clean_data/modis_clean/MOD09A1/sur_refl_b04",
        "MOD09A1_sur_refl_b04",
        1e-4,
        "clean_data/modis_clean/MOD09A1/sur_refl_b05",
        "MOD09A1_sur_refl_b05",
        1e-4,
        "clean_data/modis_clean/MOD09A1/sur_refl_b06",
        "MOD09A1_sur_refl_b06",
        1e-4,
        "clean_data/modis_clean/MOD09A1/sur_refl_b07",
        "MOD09A1_sur_refl_b07",
        1e-4,
        "clean_data/modis_clean/MOD09A1/sur_refl_day_of_year",
        "MOD09A1_sur_refl_doy",
        1.0,
        "clean_data/modis_clean/MOD11A2/LST_Day_1km",
        "MOD11A2_LST_Day_1km_K",
        0.02,
        "clean_data/modis_clean/MOD11A2/LST_Night_1km",
        "MOD11A2_LST_Night_1km_K",
        0.02,
        "clean_data/modis_clean/MOD13A3/NDVI",
        "MOD13A3_NDVI",
        1e-4,
        "clean_data/modis_clean/MOD13A3/EVI",
        "MOD13A3_EVI",
        1e-4
    )

    out <- vars |>
        mutate(tbl = pmap(list(dir, label, scale), summarize_modis_dir)) |>
        pull(tbl) |>
        list_rbind()

    if (!is.null(write_csv)) {
        readr::write_csv(out, write_csv)
    }

    message("Done. If any tiles were skipped, see: ", bad_log)
    out
}
