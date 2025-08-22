# apptainer shell /ddn/gs1/group/set/chords/combining_datasets/container_combining_datasets.sif
# Load libraries

# Load packages
# -------------------------------
# Load libraries
# -------------------------------
# Load libraries
.libPaths("/usr/local/lib/R/site-library")
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(exactextractr)
library(circular)
library(readr)

summarize_nlcd <- function(
    level = c("county", "tract"),
    agg = c("annual", "monthly"),
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    zone_layer = NULL,
    nlcd_dir = "clean_data/nlcd_clean",
    write_csv = NULL
) {
    # Load required libraries
    library(terra)
    library(sf)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(exactextractr)
    library(readr)
    library(purrr)

    # Match arguments
    level <- match.arg(level)
    agg <- match.arg(agg)
    if (is.null(zone_layer)) {
        zone_layer <- if (level == "county") "counties_500k" else "tracts_500k"
    }

    # Load zones
    zones <- st_read(zones_gpkg, layer = zone_layer, quiet = TRUE) |>
        st_make_valid() |>
        select(geoid)

    # Find input rasters
    tif_files <- list.files(
        nlcd_dir,
        pattern = "_processed\\.tif$",
        full.names = TRUE,
        recursive = TRUE
    )

    # Extract metadata and filter
    meta <- tibble(file = tif_files) |>
        mutate(
            base = str_remove(basename(file), "_\\d{4}_processed\\.tif$"),
            variable = tolower(base),
            year = as.integer(str_extract(file, "\\d{4}")),
            type = case_when(
                str_detect(
                    base,
                    "Fractional_Impervious_Surface|Land_Cover_Confidence"
                ) ~
                    "proportion",
                str_detect(base, "Land_Cover$") ~ "categorical",
                TRUE ~ "skip"
            )
        ) |>
        filter(type != "skip")

    # Land cover classes to keep
    allowed_lc_classes <- c(
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

    # Run zonal summaries
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
                    variable = paste0(row$variable, "_mean"),
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
                            return(as.list(numeric(0)))
                        }
                        tab <- tapply(cov, val, sum)
                        props <- tab / sum(tab)
                        props <- props[names(props) %in% allowed_lc_classes]
                        names(props) <- paste0("land_cover_", names(props))
                        as.list(props)
                    },
                    progress = FALSE
                )

                bind_cols(geoid = zones$geoid, summary_list) |>
                    unnest_wider(cols = -geoid) |>
                    pivot_longer(
                        cols = -geoid,
                        names_to = "variable",
                        values_to = "value"
                    ) |>
                    mutate(year = row$year)
            }
        )

        results_all[[i]] <- result
    }

    # Combine and format
    final <- bind_rows(results_all)

    if (agg == "monthly") {
        final <- final |>
            crossing(month = 1:12) |>
            select(geoid, variable, value, month, year)
    } else {
        final <- final |>
            select(geoid, variable, value, year)
    }

    # Write output
    if (!is.null(write_csv)) {
        dir.create(dirname(write_csv), recursive = TRUE, showWarnings = FALSE)
        write_csv(final, write_csv)
    }

    return(final)
}


summarize_nlcd(
    level = c("county"),
    agg = c("annual"),
    zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    zone_layer = NULL,
    nlcd_dir = "test_data/nlcd_clean",
    write_csv = NULL
)
