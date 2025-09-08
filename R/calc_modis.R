summarize_modis <- function(
    level = c("county", "tract", "zip"),
    agg = c("annual", "monthly", "overall"),
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    zone_layer = NULL,
    id_col = "geoid",
    write_csv = NULL
) {
    level <- match.arg(level)
    agg <- match.arg(agg)

    if (is.null(zone_layer)) {
        zone_layer <- switch(
            level,
            county = "counties_500k",
            tract = "tracts_500k",
            zip = "zctas_500k"
        )
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
        tibble::tibble(
            path = list.files(var_dir, pattern = "\\.tif$", full.names = TRUE)
        ) |>
            dplyr::mutate(
                date = as.Date(stringr::str_extract(
                    basename(path),
                    "\\d{4}-\\d{2}-\\d{2}"
                )),
                year = as.integer(format(date, "%Y"))
            ) |>
            dplyr::arrange(date) |>
            dplyr::mutate(read_ok = purrr::map_lgl(path, is_readable_raster)) |>
            (\(df) {
                bad <- dplyr::filter(df, !read_ok)
                if (nrow(bad) > 0) {
                    log_bad(bad$path, "readStart failure or not a raster")
                }
                dplyr::filter(df, read_ok)
            })() |>
            dplyr::select(-read_ok)
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
            tibble::tibble(
                !!id_col := zones_proj[[id_col]],
                mean_raw = as.numeric(vals)
            )
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
        tibble::tibble(
            !!id_col := vector(mode = "character"),
            mean_raw = double()
        )
    }

    summarize_modis_dir <- function(var_dir, variable_label, scale_factor) {
        files <- index_modis_files(var_dir)
        if (nrow(files) == 0) {
            return(tibble::tibble())
        }

        r0 <- terra::rast(files$path[1])
        zones_proj <- sf::st_transform(zones_ll, terra::crs(r0))
        extract_means <- extract_means_factory(zones_proj)

        daily <- files |>
            dplyr::mutate(
                res = purrr::map(path, ~ safe_extract_means(extract_means, .x))
            ) |>
            tidyr::unnest(res) |>
            dplyr::mutate(value = mean_raw * scale_factor) |>
            dplyr::select(dplyr::all_of(id_col), date, year, value)

        if (agg == "annual") {
            daily |>
                dplyr::group_by(.data[[id_col]], year) |>
                dplyr::summarise(
                    value = mean(value, na.rm = TRUE),
                    .groups = "drop"
                ) |>
                dplyr::mutate(variable = variable_label) |>
                dplyr::relocate(variable, !!rlang::sym(id_col), year, value)
        } else if (agg == "monthly") {
            daily |>
                dplyr::mutate(month = lubridate::month(date)) |>
                dplyr::group_by(.data[[id_col]], year, month) |>
                dplyr::summarise(
                    value = mean(value, na.rm = TRUE),
                    .groups = "drop"
                ) |>
                dplyr::mutate(variable = variable_label) |>
                dplyr::relocate(
                    variable,
                    !!rlang::sym(id_col),
                    year,
                    month,
                    value
                )
        } else {
            # overall: mean over all dates in 2010–2024
            daily |>
                dplyr::filter(year >= 2010, year <= 2024) |>
                dplyr::group_by(.data[[id_col]]) |>
                dplyr::summarise(
                    value = mean(value, na.rm = TRUE),
                    .groups = "drop"
                ) |>
                dplyr::mutate(variable = variable_label) |>
                dplyr::relocate(variable, !!rlang::sym(id_col), value)
        }
    }

    # Variable metadata
    vars <- tibble::tribble(
        ~dir,
        ~label,
        ~scale,
        "clean_data/modis_clean/MOD09A1/sur_refl_b01",
        "sur_refl_b01",
        1e-4,
        "clean_data/modis_clean/MOD09A1/sur_refl_b02",
        "sur_refl_b02",
        1e-4,
        "clean_data/modis_clean/MOD09A1/sur_refl_b03",
        "sur_refl_b03",
        1e-4,
        "clean_data/modis_clean/MOD09A1/sur_refl_b04",
        "sur_refl_b04",
        1e-4,
        "clean_data/modis_clean/MOD09A1/sur_refl_b05",
        "sur_refl_b05",
        1e-4,
        "clean_data/modis_clean/MOD09A1/sur_refl_b06",
        "sur_refl_b06",
        1e-4,
        "clean_data/modis_clean/MOD09A1/sur_refl_b07",
        "sur_refl_b07",
        1e-4,
        "clean_data/modis_clean/MOD11A2/LST_Day_1km",
        "LST_Day_1km_K",
        0.02,
        "clean_data/modis_clean/MOD11A2/LST_Night_1km",
        "LST_Night_1km_K",
        0.02,
        "clean_data/modis_clean/MOD13A3/NDVI",
        "NDVI",
        1e-4,
        "clean_data/modis_clean/MOD13A3/EVI",
        "EVI",
        1e-4
    )

    out <- vars |>
        dplyr::mutate(
            tbl = purrr::pmap(list(dir, label, scale), summarize_modis_dir)
        ) |>
        dplyr::pull(tbl) |>
        dplyr::bind_rows()

    if (!is.null(write_csv)) {
        readr::write_csv(out, write_csv)
    }

    message("Done. If any tiles were skipped, see: ", bad_log)
    out
}
