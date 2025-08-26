build_exposure_long_streamed <- function(
    agg = c("annual", "monthly"),
    level = c("county", "tract"),
    input_dir = "summary_sets",
    dataset_dir = NULL # on-disk Arrow dataset (partitioned)
) {
    agg <- match.arg(agg)
    level <- match.arg(level)

    if (is.null(dataset_dir)) {
        dataset_dir <- file.path(
            "handoffs",
            "long_dataset",
            paste0(agg, "_", level)
        )
    }
    dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)

    # Files to include (always include normals + statics)
    pattern <- sprintf(
        "^(%s_%s|normal_%s|static_%s).*\\.csv$",
        agg,
        level,
        level,
        level
    )
    csv_files <- list.files(input_dir, pattern = pattern, full.names = TRUE)
    if (length(csv_files) == 0) {
        stop(sprintf(
            "No files matched pattern in '%s': %s",
            input_dir,
            pattern
        ))
    }

    suppressPackageStartupMessages({
        library(readr)
        library(dplyr)
        library(stringr)
        library(arrow)
    })

    normalize_one <- function(file, is_monthly) {
        df <- readr::read_csv(file, show_col_types = FALSE)
        nm <- names(df)
        ln <- tolower(nm)
        names(df) <- ln

        # Standardize names where possible
        if ("var" %in% ln && !("variable" %in% ln)) {
            df <- dplyr::rename(df, variable = "var")
        }
        if (!("variable" %in% names(df))) {
            stop("Missing 'variable' column in ", basename(file))
        }

        # value column (pick the first present)
        candidates <- c("value", "annual_mean", "mean", "val")
        have <- candidates[candidates %in% names(df)]
        if (length(have) == 0) {
            stop("Missing a value column in ", basename(file))
        }
        if ("value" %in% have) {
            # ok
        } else {
            df <- dplyr::rename(df, value = have[1])
        }

        # geoid
        if (!("geoid" %in% names(df))) {
            if ("geoid10" %in% names(df)) {
                df <- dplyr::rename(df, geoid = "geoid10")
            } else if ("geoid_county" %in% names(df)) {
                df <- dplyr::rename(df, geoid = "geoid_county")
            } else {
                stop("Missing geoid column in ", basename(file))
            }
        }

        file_name <- basename(file)

        # year
        if (!("year" %in% names(df))) {
            df <- df %>%
                mutate(
                    year = dplyr::case_when(
                        str_detect(file_name, "^normal_") ~ "normal",
                        str_detect(file_name, "^static_") ~ "static",
                        TRUE ~ NA_character_
                    )
                )
        } else {
            df$year <- as.character(df$year)
        }

        # month for monthly agg
        if (is_monthly) {
            if (!("month" %in% names(df))) {
                df$month <- NA_integer_
            } else {
                df$month <- suppressWarnings(as.integer(df$month))
            }
        } else {
            if ("month" %in% names(df)) {
                df$month <- NULL
            }
        }

        df %>%
            mutate(
                variable = variable %>%
                    tolower() %>%
                    str_remove("_\\d{4}$") %>%
                    str_remove("_clean$")
            ) %>%
            select(geoid, variable, value, dplyr::any_of("month"), year)
    }

    is_monthly <- identical(agg, "monthly")

    # Stream: write each file into a partitioned Arrow dataset
    for (f in csv_files) {
        chunk <- normalize_one(f, is_monthly)
        # Ensure types are consistent
        chunk$geoid <- as.character(chunk$geoid)
        chunk$variable <- as.character(chunk$variable)
        chunk$year <- as.character(chunk$year)
        if (is_monthly && "month" %in% names(chunk)) {
            chunk$month <- as.integer(chunk$month)
        }

        arrow::write_dataset(
            chunk,
            path = dataset_dir,
            format = "parquet",
            partitioning = c(if (is_monthly) "month", "year"),
            existing_data_behavior = "overwrite"
        )
    }

    dataset_dir
}
