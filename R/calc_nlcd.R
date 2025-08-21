# Robust NLCD zonal summaries (no rescaling; level dropped from final output)
# - Categorical rasters: variable = "<base>_prop_class_<code>", value = proportion [0–1]
# - Proportion rasters (e.g., Fractional Impervious Surface [%]): variable = "<base>_mean", value = area-weighted mean (native units)
# - DOY rasters: variables "<base>_doy_circ_mean" and "<base>_share_valid"
# - Monthly agg replicates annual values across months 1..12

zonal_means_from_tifs <- function(
    input_dir = "clean_data/nlcd_clean",
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    level = c("county", "tract"),
    zone_layer = NULL,
    agg = c("annual", "monthly"),
    id_col = "geoid",
    file_pattern = "_processed\\.tif$",
    write_csv = NULL
) {
    # ---------------- Helpers ----------------
    infer_var_type <- function(var) {
        v <- tolower(var)
        if (grepl("land[_-]?cover[_-]?change", v)) {
            return("categorical")
        }
        if (grepl("land[_-]?cover(?!.*confidence)", v, perl = TRUE)) {
            return("categorical")
        }
        if (grepl("impervious[_-]?descriptor", v)) {
            return("categorical")
        }
        if (grepl("fractional[_-]?impervious|impervious[_-]?fraction", v)) {
            return("proportion")
        }
        if (grepl("land[_-]?cover[_-]?confidence|\\bconfidence\\b", v)) {
            return("proportion")
        }
        if (grepl("spectral[_-]?change.*day[_-]?of[_-]?year|\\bdoy\\b", v)) {
            return("doy")
        }
        "proportion"
    }

    weighted_mean <- function(values, coverage_fraction) {
        ok <- is.finite(values) &
            is.finite(coverage_fraction) &
            coverage_fraction > 0
        if (!any(ok)) {
            return(NA_real_)
        }
        v <- values[ok]
        w <- coverage_fraction[ok]
        sum(v * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
    }

    class_props <- function(values, coverage_fraction) {
        ok <- is.finite(values) &
            is.finite(coverage_fraction) &
            coverage_fraction > 0
        if (!any(ok)) {
            return(tibble::tibble(class = integer(), prop = numeric()))
        }
        v <- as.integer(round(values[ok]))
        w <- coverage_fraction[ok]
        df <- tibble::tibble(class = v, w = w) |>
            dplyr::group_by(class) |>
            dplyr::summarise(prop = sum(w), .groups = "drop")
        df$prop <- df$prop / sum(df$prop)
        df
    }

    circular_mean_doy <- function(values, coverage_fraction, n_days = 366) {
        ok <- is.finite(values) &
            is.finite(coverage_fraction) &
            coverage_fraction > 0
        if (!any(ok)) {
            return(NA_real_)
        }
        v <- values[ok]
        w <- coverage_fraction[ok]
        theta <- 2 * pi * (v / n_days)
        C <- sum(w * cos(theta)) / sum(w)
        S <- sum(w * sin(theta)) / sum(w)
        ang <- atan2(S, C)
        if (ang < 0) {
            ang <- ang + 2 * pi
        }
        as.numeric((ang / (2 * pi)) * n_days)
    }

    parse_base_variable <- function(path) {
        base <- tools::file_path_sans_ext(basename(path))
        base <- sub("_processed$", "", base)
        base <- sub("_(19|20)\\d{2}$", "", base) # strip trailing _YYYY
        paste0("annual_", tolower(base))
    }

    parse_year <- function(path) {
        as.integer(stringr::str_extract(basename(path), "(19|20)\\d{2}"))
    }

    # ---------------- Main ----------------
    level <- match.arg(level)
    agg <- match.arg(agg)
    if (is.null(zone_layer)) {
        zone_layer <- if (level == "county") "counties_500k" else "tracts_500k"
    }

    zones <- sf::st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid()

    if (!(id_col %in% names(zones))) {
        rlang::abort(paste0(
            "id_col '",
            id_col,
            "' not found in zones: available = ",
            paste(names(zones), collapse = ", ")
        ))
    }
    zones <- dplyr::select(zones, !!rlang::sym(id_col)) |> sf::st_as_sf()

    tif_files <- list.files(
        input_dir,
        pattern = file_pattern,
        recursive = TRUE,
        full.names = TRUE
    )
    if (length(tif_files) == 0L) {
        rlang::abort(paste0(
            "No TIFFs found in ",
            input_dir,
            " matching pattern ",
            file_pattern
        ))
    }

    extract_one <- function(tif_path) {
        r <- terra::rast(tif_path)
        zones_proj <- sf::st_transform(zones, terra::crs(r))

        base_var <- parse_base_variable(tif_path)
        vtype <- infer_var_type(base_var)
        yr <- parse_year(tif_path)
        nly <- terra::nlyr(r)

        if (vtype %in% c("categorical", "doy") && nly > 1) {
            # Keep first band for these types
            r <- r[[1]]
            nly <- 1
        }

        if (vtype == "categorical") {
            res <- exactextractr::exact_extract(
                r,
                zones_proj,
                function(values, coverage_fraction) {
                    class_props(values, coverage_fraction)
                }
            )
            out <- purrr::map2_dfr(
                res,
                zones_proj[[id_col]],
                ~ dplyr::mutate(.x, !!rlang::sym(id_col) := .y)
            ) |>
                dplyr::mutate(
                    variable = paste0(base_var, "_prop_class_", class),
                    year = yr
                ) |>
                dplyr::transmute(
                    !!rlang::sym(id_col),
                    variable,
                    year,
                    value = prop
                )
            return(out)
        }

        if (vtype == "doy") {
            res_df <- exactextractr::exact_extract(
                r,
                zones_proj,
                function(values, coverage_fraction) {
                    c(
                        doy_circ_mean = circular_mean_doy(
                            values,
                            coverage_fraction
                        ),
                        share_valid = weighted_mean(
                            as.numeric(is.finite(values)),
                            coverage_fraction
                        )
                    )
                }
            )
            out <- tibble::tibble(
                !!rlang::sym(id_col) := zones_proj[[id_col]],
                year = yr
            )
            out <- dplyr::bind_cols(out, tibble::as_tibble(res_df)) |>
                tidyr::pivot_longer(
                    cols = c(doy_circ_mean, share_valid),
                    names_to = "suffix",
                    values_to = "value"
                ) |>
                dplyr::mutate(variable = paste0(base_var, "_", suffix)) |>
                dplyr::select(!!rlang::sym(id_col), variable, year, value)
            return(out)
        }

        # proportion-like
        if (nly == 1) {
            means <- exactextractr::exact_extract(
                r,
                zones_proj,
                function(values, coverage_fraction) {
                    weighted_mean(values, coverage_fraction)
                }
            )
            out <- tibble::tibble(
                !!rlang::sym(id_col) := zones_proj[[id_col]],
                variable = paste0(base_var, "_mean"),
                year = yr,
                value = as.numeric(means)
            )
            return(out)
        } else {
            # Multi-layer proportion-like raster: compute per-layer weighted mean
            lyr_names <- names(r)
            if (
                is.null(lyr_names) ||
                    any(is.na(lyr_names)) ||
                    any(lyr_names == "")
            ) {
                lyr_names <- paste0("band", seq_len(nly))
            }
            # exact_extract with a function over multi-layer rasters passes a matrix
            # (columns = layers). Return a named vector -> data.frame row.
            res_df <- exactextractr::exact_extract(
                r,
                zones_proj,
                function(values, coverage_fraction) {
                    # values is (n_cells x n_layers)
                    col_means <- apply(as.matrix(values), 2, function(col) {
                        weighted_mean(col, coverage_fraction)
                    })
                    stats::setNames(col_means, lyr_names)
                }
            )
            out <- tibble::tibble(
                !!rlang::sym(id_col) := zones_proj[[id_col]],
                year = yr
            ) |>
                dplyr::bind_cols(tibble::as_tibble(res_df)) |>
                tidyr::pivot_longer(
                    cols = dplyr::all_of(lyr_names),
                    names_to = "layer",
                    values_to = "value"
                ) |>
                dplyr::mutate(
                    variable = paste0(base_var, "_", layer, "_mean")
                ) |>
                dplyr::select(!!rlang::sym(id_col), variable, year, value)
            return(out)
        }
    }

    # Extract and stack
    raw_df <- purrr::map_dfr(tif_files, extract_one)

    # Average across multiple tiles per zone-variable-year if present
    out <- raw_df |>
        dplyr::group_by(!!rlang::sym(id_col), variable, year) |>
        dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

    # Annual vs monthly: replicate annual values across 12 months when requested
    if (agg == "annual") {
        out <- out |>
            dplyr::mutate(month = NA_integer_) |>
            dplyr::relocate(!!rlang::sym(id_col), variable, year, month, value)
    } else {
        out <- out |>
            dplyr::group_by(!!rlang::sym(id_col), variable, year) |>
            tidyr::expand(month = 1:12) |>
            dplyr::ungroup() |>
            dplyr::left_join(out, by = c(id_col, "variable", "year")) |>
            dplyr::relocate(!!rlang::sym(id_col), variable, year, month, value)
    }

    if (!is.null(write_csv)) {
        dir.create(dirname(write_csv), recursive = TRUE, showWarnings = FALSE)
        readr::write_csv(out, write_csv)
    }

    out
}
