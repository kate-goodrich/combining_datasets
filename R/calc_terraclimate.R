summarize_terraclimate <- function(
    tif_dir = "clean_data/terraclimate_clean",
    county_gpkg = "clean_data/county_census_zip/canonical_2024.gpkg",
    level = c("county", "tract", "zip"),
    agg = c("annual", "monthly", "overall"),
    zone_layer = NULL,
    id_col = "geoid",
    write_csv = NULL,
    exclude_vars = "srad" # <— exclude these TerraClimate variables (default: srad)
) {
    # --- Match args and infer layer if needed ---
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

    # --- Load zones and ensure valid geometry ---
    zones <- sf::st_read(county_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid()
    if (!(id_col %in% names(zones))) {
        stop("id_col not found in zones layer.")
    }

    # --- Helpers ---
    std_layer_names <- function(nm) {
        out <- sub("^.*?(\\d{6})$", "\\1", nm) # prefer YYYYMM
        bad <- !grepl("^\\d{6}$", out)
        out[bad] <- sub("^.*?(\\d{4})$", "\\1", nm[bad]) # fallback YYYY
        out
    }
    layer_years <- function(r) {
        as.integer(substr(std_layer_names(names(r)), 1, 4))
    }
    annualize <- function(r) {
        yrs <- layer_years(r)
        ra <- terra::tapp(r, index = yrs, fun = mean, na.rm = TRUE)
        names(ra) <- as.character(sort(unique(yrs)))
        ra
    }
    # variable name from PATH, normalized (strip common suffixes like _processed/_clean/_YYYYMMDD)
    var_from_path <- function(p) {
        nm <- tools::file_path_sans_ext(basename(p))
        nm <- tolower(nm)
        nm <- sub("(_processed|_clean)$", "", nm)
        nm <- sub("_\\d{6}$", "", nm) # strip trailing YYYYMM if present
        nm <- sub("_\\d{4}$", "", nm) # or trailing YYYY
        nm
    }
    # variable name from RASTER names (first layer), normalized
    var_from_r <- function(r) {
        nm <- tolower(names(r)[1])
        nm <- sub("(_processed|_clean)$", "", nm)
        nm <- sub("_\\d{6}$", "", nm)
        nm <- sub("_\\d{4}$", "", nm)
        nm
    }

    # --- List all .tif files ---
    tifs <- list.files(tif_dir, pattern = "\\.tif$", full.names = TRUE)
    if (length(tifs) == 0L) {
        stop("No .tif files found in '", tif_dir, "'.")
    }
    # Early filter by filename to skip excluded variables fast
    if (length(exclude_vars)) {
        vars_inferred <- vapply(tifs, var_from_path, character(1))
        keep <- !(vars_inferred %in% tolower(exclude_vars))
        tifs <- tifs[keep]
    }
    if (length(tifs) == 0L) {
        # nothing to do after exclusions
        out <- tibble::tibble(
            !!id_col := zones[[id_col]],
            var = character(),
            year = integer(),
            month = integer(),
            value = double()
        )[0, ]
        if (!is.null(write_csv)) {
            readr::write_csv(out, write_csv)
        }
        return(out)
    }

    # --- Zonal summary for one file ---
    summarise_one <- function(tif_path, zones_ll, agg_mode) {
        r <- terra::rast(tif_path)
        vname <- var_from_r(r)

        # Defensive skip if variable is excluded (covers odd filenames/metadata)
        if (length(exclude_vars) && vname %in% tolower(exclude_vars)) {
            return(tibble::tibble(
                !!id_col := zones_ll[[id_col]],
                var = character(),
                year = integer(),
                month = integer(),
                value = double()
            )[0, ])
        }

        zones_proj <- sf::st_transform(zones_ll, terra::crs(r))

        r_use <- switch(
            agg_mode,
            "monthly" = {
                names(r) <- std_layer_names(names(r))
                r
            },
            "annual" = annualize(r),
            "overall" = annualize(r) # compute annual first; overall later
        )

        # Clamp negatives to zero
        r_use <- terra::ifel(r_use < 0, 0, r_use)

        # Area weights (m^2); one weights raster is fine for stack on same grid
        w <- terra::cellSize(r_use[[1]], unit = "m")

        # Area-weighted mean extraction
        mat <- exactextractr::exact_extract(
            r_use,
            zones_proj,
            fun = "weighted_mean",
            weights = w,
            progress = FALSE
        )
        mat <- tibble::as_tibble(mat)
        colnames(mat) <- names(r_use)

        base <- tibble::tibble(!!id_col := zones_proj[[id_col]]) |>
            dplyr::bind_cols(mat) |>
            tidyr::pivot_longer(
                -dplyr::all_of(id_col),
                names_to = if (agg_mode == "monthly") "date" else "year",
                values_to = "value"
            ) |>
            dplyr::mutate(
                var = vname,
                year = if (agg_mode == "monthly") {
                    as.integer(substr(.data$date, 1, 4))
                } else {
                    as.integer(.data$year)
                },
                month = if (agg_mode == "monthly") {
                    as.integer(substr(.data$date, 5, 6))
                } else {
                    NA_integer_
                }
            )

        if (agg_mode == "overall") {
            base |>
                dplyr::filter(year >= 2010, year <= 2024) |>
                dplyr::group_by(.data[[id_col]], var) |>
                dplyr::summarise(
                    value = mean(value, na.rm = TRUE),
                    .groups = "drop"
                ) |>
                dplyr::select(dplyr::all_of(id_col), var, value)
        } else if (agg_mode == "annual") {
            base |>
                dplyr::select(dplyr::all_of(id_col), year, var, value)
        } else {
            base |>
                dplyr::select(dplyr::all_of(id_col), year, month, var, value)
        }
    }

    # --- Run zonal summaries ---
    df <- purrr::map_dfr(tifs, summarise_one, zones_ll = zones, agg_mode = agg)

    # --- Optionally write to disk ---
    if (!is.null(write_csv)) {
        readr::write_csv(df, write_csv)
    }

    df
}
