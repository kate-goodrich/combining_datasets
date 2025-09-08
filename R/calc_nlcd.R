summarize_nlcd <- function(
    level = c("county", "tract", "zip"),
    agg = c("annual", "monthly", "overall"),
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    zone_layer = NULL,
    nlcd_dir = "clean_data/nlcd_clean",
    id_col = "geoid",
    write_csv = NULL
) {
    # Match arguments
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

    # Load zones
    zones <- sf::st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        sf::st_make_valid() |>
        dplyr::select(!!id_col := dplyr::all_of(id_col))

    # Load raster files
    tif_files <- list.files(
        nlcd_dir,
        pattern = "_processed\\.tif$",
        full.names = TRUE,
        recursive = TRUE
    )

    # Allowed land cover codes
    allowed_classes <- c(
        11,
        12,
        21,
        22,
        23,
        24,
        31,
        41,
        42,
        43,
        52,
        71,
        81,
        82,
        90,
        95
    )

    # Define metadata
    meta <- tibble::tibble(file = tif_files) |>
        dplyr::mutate(
            base = stringr::str_remove(
                basename(file),
                "_\\d{4}_processed\\.tif$"
            ),
            variable = tolower(base),
            year = as.integer(stringr::str_extract(file, "\\d{4}")),
            type = dplyr::case_when(
                base == "Fractional_Impervious_Surface" ~ "proportion",
                base == "Land_Cover_Confidence" ~ "proportion",
                base == "Land_Cover" ~ "categorical",
                TRUE ~ "skip"
            )
        ) |>
        dplyr::filter(type != "skip")

    # Initialize result list
    results_all <- vector("list", nrow(meta))

    for (i in seq_len(nrow(meta))) {
        row <- meta[i, ]
        message("Processing: ", basename(row$file))

        r <- terra::rast(row$file)[[1]]
        zones_proj <- sf::st_transform(zones, terra::crs(r))

        result <- switch(
            row$type,
            "proportion" = {
                val <- exactextractr::exact_extract(
                    r,
                    zones_proj,
                    fun = function(values, coverage_fractions) {
                        stats::weighted.mean(
                            values,
                            coverage_fractions,
                            na.rm = TRUE
                        )
                    },
                    progress = FALSE
                )
                tibble::tibble(
                    !!id_col := zones[[id_col]],
                    variable = row$variable,
                    value = val,
                    year = row$year
                )
            },
            "categorical" = {
                lc_props <- exactextractr::exact_extract(
                    r,
                    zones_proj,
                    fun = function(values, coverage_fractions) {
                        valid <- !is.na(values) & !is.na(coverage_fractions)
                        values <- values[valid]
                        coverage_fractions <- coverage_fractions[valid]
                        if (length(values) == 0) {
                            return(as.list(numeric(0)))
                        }
                        tab <- tapply(coverage_fractions, values, sum)
                        tab <- tab[names(tab) %in% allowed_classes]
                        props <- tab / sum(tab)
                        names(props) <- paste0("land_cover_", names(props))
                        as.list(props)
                    },
                    progress = FALSE
                )

                dplyr::tibble(
                    !!id_col := zones[[id_col]],
                    props = lc_props
                ) |>
                    tidyr::unnest_wider(props) |>
                    tidyr::pivot_longer(
                        -dplyr::all_of(id_col),
                        names_to = "variable",
                        values_to = "value"
                    ) |>
                    dplyr::mutate(year = row$year)
            }
        )

        results_all[[i]] <- result
    }

    # Combine results
    final <- dplyr::bind_rows(results_all)

    # Aggregate by requested grain
    if (agg == "monthly") {
        final <- final |>
            tidyr::crossing(month = 1:12) |>
            dplyr::select(dplyr::all_of(id_col), variable, value, month, year)
    } else if (agg == "annual") {
        final <- final |>
            dplyr::select(dplyr::all_of(id_col), variable, value, year)
    } else {
        # overall (mean across 2010–2024)
        final <- final |>
            dplyr::filter(year >= 2010, year <= 2024) |>
            dplyr::group_by(.data[[id_col]], variable) |>
            dplyr::summarise(
                value = mean(value, na.rm = TRUE),
                .groups = "drop"
            ) |>
            dplyr::select(dplyr::all_of(id_col), variable, value)
    }

    # Write CSV if requested
    if (!is.null(write_csv)) {
        dir.create(dirname(write_csv), showWarnings = FALSE, recursive = TRUE)
        readr::write_csv(final, write_csv)
    }

    final
}
