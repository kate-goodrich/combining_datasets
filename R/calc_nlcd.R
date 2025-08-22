summarize_nlcd <- function(
    level = c("county", "tract"),
    agg = c("annual", "monthly"),
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    zone_layer = NULL,
    nlcd_dir = "clean_data/nlcd_clean",
    write_csv = NULL
) {
    # -------------------------------
    # Load libraries
    # -------------------------------
    library(terra)
    library(sf)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(exactextractr)
    library(circular)
    library(readr)
    library(purrr)

    # -------------------------------
    # Step 1: Parse inputs
    # -------------------------------
    level <- match.arg(level)
    agg <- match.arg(agg)
    if (is.null(zone_layer)) {
        zone_layer <- if (level == "county") "counties_500k" else "tracts_500k"
    }

    # -------------------------------
    # Step 2: Read geometries
    # -------------------------------
    zones <- st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        st_make_valid() |>
        select(geoid)

    # -------------------------------
    # Step 3: List & categorize rasters
    # -------------------------------
    tif_files <- list.files(
        nlcd_dir,
        pattern = "_processed\\.tif$",
        full.names = TRUE,
        recursive = TRUE
    )

    meta <- tibble(file = tif_files) |>
        mutate(
            variable = str_extract(basename(file), "^[^_]+(?:_[^_]+)*"),
            year = as.integer(str_extract(file, "\\d{4}")),
            type = case_when(
                str_detect(
                    file,
                    "Fractional_Impervious_Surface|Land_Cover_Confidence"
                ) ~
                    "proportion",
                str_detect(file, "Land_Cover") ~ "categorical",
                str_detect(file, "Spectral_Change_Day_of_Year") ~ "doy",
                TRUE ~ "skip"
            )
        ) |>
        filter(type != "skip")

    # -------------------------------
    # Step 4: Summarize all rasters
    # -------------------------------
    results_all <- vector("list", nrow(meta))

    for (i in seq_len(nrow(meta))) {
        row <- meta[i, ]
        message("Processing: ", basename(row$file))
        r <- rast(row$file)[[1]]
        zones_proj <- st_transform(zones, crs(r))

        result <- switch(
            row$type,
            "proportion" = {
                values <- exact_extract(
                    r,
                    zones_proj,
                    function(val, cov) weighted.mean(val, cov, na.rm = TRUE),
                    progress = FALSE
                )
                tibble(
                    geoid = zones$geoid,
                    variable = row$variable,
                    value = values,
                    year = row$year
                )
            },
            "categorical" = {
                summary_list <- exact_extract(
                    r,
                    zones_proj,
                    function(val, cov) {
                        valid <- !is.na(val) & !is.na(cov)
                        val <- val[valid]
                        cov <- cov[valid]
                        if (length(val) == 0) {
                            return(list())
                        }
                        tab <- tapply(cov, val, sum)
                        props <- tab / sum(tab)
                        out <- setNames(
                            as.list(as.numeric(props)),
                            paste0(row$variable, "_", names(props))
                        )
                        list(out)
                    },
                    progress = FALSE
                )
                bind_cols(geoid = zones$geoid, summary_list) |>
                    pivot_longer(
                        -geoid,
                        names_to = "variable",
                        values_to = "value"
                    ) |>
                    mutate(year = row$year)
            },
            "doy" = {
                values <- exact_extract(
                    r,
                    zones_proj,
                    function(val, cov) {
                        val <- val[!is.na(val)]
                        cov <- cov[!is.na(val)]
                        if (length(val) == 0 || length(cov) == 0) {
                            return(NA_real_)
                        }
                        circ <- circular(
                            val * 2 * pi / 365,
                            units = "radians",
                            template = "none"
                        )
                        as.numeric(mean.circular(circ)) %%
                            (2 * pi) *
                            365 /
                            (2 * pi)
                    },
                    progress = FALSE
                )
                tibble(
                    geoid = zones$geoid,
                    variable = paste0(row$variable, "_doy_circ_mean"),
                    value = values,
                    year = row$year
                )
            }
        )

        results_all[[i]] <- result
    }

    # -------------------------------
    # Step 5: Combine and format
    # -------------------------------
    final <- bind_rows(results_all)

    if (agg == "monthly") {
        final <- final |>
            crossing(month = 1:12) |>
            select(geoid, variable, value, month, year)
    } else {
        final <- final |>
            select(geoid, variable, value, year)
    }

    # -------------------------------
    # Step 6: Write to CSV if requested
    # -------------------------------
    if (!is.null(write_csv)) {
        dir.create(dirname(write_csv), recursive = TRUE, showWarnings = FALSE)
        write_csv(final, write_csv)
    }

    return(final)
}
