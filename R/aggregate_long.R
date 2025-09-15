build_exposure_long_streamed <- function(
    agg = c("annual", "monthly"),
    level = c("county", "tract", "zip"),
    input_dir = "summary_sets",
    handoff_dir = "handoffs"
) {
    agg <- match.arg(agg)
    level <- match.arg(level)

    # --- Set up directories ---
    long_dir <- file.path(handoff_dir, paste0(level, "_", agg, "_long"))
    wide_dir <- file.path(handoff_dir, paste0(level, "_", agg, "_wide"))
    dir.create(long_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(wide_dir, recursive = TRUE, showWarnings = FALSE)

    # --- File matching (always include normal + static) ---
    pattern <- sprintf(
        "^(%1$s_%2$s|normal_.*_%2$s|static_%2$s).*\\.csv$",
        agg,
        level
    )
    csv_files <- list.files(input_dir, pattern = pattern, full.names = TRUE)
    if (length(csv_files) == 0) {
        stop(sprintf(
            "No files matched in '%s' with pattern: %s",
            input_dir,
            pattern
        ))
    }

    suppressPackageStartupMessages({
        library(readr)
        library(dplyr)
        library(stringr)
        library(arrow)
        library(tidyr)
        library(purrr)
    })

    # --- Normalization helper ---
    normalize_one <- function(file, is_monthly) {
        df <- readr::read_csv(file, show_col_types = FALSE)
        names(df) <- tolower(names(df))

        # variable
        if ("var" %in% names(df)) {
            df <- rename(df, variable = var)
        }
        if (!"variable" %in% names(df)) {
            stop("Missing 'variable' column in ", basename(file))
        }

        # value
        candidates <- c("value", "annual_mean", "mean", "val")
        have <- candidates[candidates %in% names(df)]
        if (length(have) == 0) {
            stop("Missing value column in ", basename(file))
        }
        if (have[1] != "value") {
            df <- rename(df, value = !!have[1])
        }

        # geoid (all summary sets already provide it)
        if (!"geoid" %in% names(df)) {
            stop("Missing geoid column in ", basename(file))
        }

        file_name <- basename(file)

        # year
        if (!"year" %in% names(df)) {
            df <- mutate(
                df,
                year = case_when(
                    str_detect(file_name, "^normal_") ~ "normal",
                    str_detect(file_name, "^static_") ~ "static",
                    TRUE ~ NA_character_
                )
            )
        } else {
            df$year <- as.character(df$year)
        }

        # month handling
        if (is_monthly) {
            if (!"month" %in% names(df)) {
                df$month <- NA_integer_
            } else {
                df$month <- suppressWarnings(as.integer(df$month))
            }
        } else {
            if ("month" %in% names(df)) df$month <- NULL
        }

        df %>%
            mutate(
                variable = variable %>%
                    tolower() %>%
                    str_remove("_\\d{4}$") %>% # drop trailing year in var name if present
                    str_remove("_clean$"),
                value = if_else(
                    str_starts(variable, "land_cover_") & is.na(value),
                    0,
                    value
                )
            ) %>%
            select(geoid, variable, value, any_of("month"), year)
    }

    is_monthly <- identical(agg, "monthly")

    # --- Collect all normalized chunks ---
    all_data <- purrr::map_dfr(
        csv_files,
        normalize_one,
        is_monthly = is_monthly
    )

    # Drop annual-normals from monthly builds
    if (is_monthly) {
        all_data <- all_data %>% filter(!(year == "normal" & is.na(month)))
    }

    # --- Write long outputs ---
    long_csv <- file.path(long_dir, paste0(level, "_", agg, ".csv"))
    long_parq <- file.path(long_dir, paste0(level, "_", agg, ".parquet"))
    readr::write_csv(all_data, long_csv)
    arrow::write_parquet(all_data, long_parq)

    # Helper: pivot a chunk to wide (one row per geoid, columns = variables)
    to_wide <- function(df) {
        df %>%
            group_by(geoid, variable) %>%
            summarise(value = dplyr::first(value), .groups = "drop") %>%
            tidyr::pivot_wider(names_from = variable, values_from = value) %>%
            arrange(geoid)
    }

    # --- Write wide outputs (CSV + Parquet) ---
    if (is_monthly) {
        all_data %>%
            mutate(year = as.character(year)) %>%
            group_split(year, month) %>%
            purrr::walk(function(chunk) {
                y <- unique(chunk$year)
                m <- unique(chunk$month)
                if (length(y) != 1 || length(m) != 1) {
                    stop("Unexpected grouping")
                }

                # base filename (no extension)
                base <- if (y == "normal") {
                    sprintf("%s_normal_%02d", level, m)
                } else if (y == "static") {
                    if (is.na(m)) {
                        sprintf("%s_static", level)
                    } else {
                        sprintf("%s_static_%02d", level, m)
                    }
                } else {
                    sprintf("%s_%s_%02d", level, y, m)
                }

                fn_csv <- file.path(wide_dir, paste0(base, ".csv"))
                fn_parq <- file.path(wide_dir, paste0(base, ".parquet"))

                chunk_wide <- to_wide(chunk)
                readr::write_csv(chunk_wide, fn_csv)
                arrow::write_parquet(chunk_wide, fn_parq)
            })
    } else {
        all_data %>%
            mutate(year = as.character(year)) %>%
            group_split(year) %>%
            purrr::walk(function(chunk) {
                y <- unique(chunk$year)
                if (length(y) != 1) {
                    stop("Unexpected grouping")
                }

                # base filename (no extension)
                base <- if (y == "normal") {
                    paste0(level, "_normal")
                } else if (y == "static") {
                    paste0(level, "_static")
                } else {
                    sprintf("%s_%s", level, y)
                }

                fn_csv <- file.path(wide_dir, paste0(base, ".csv"))
                fn_parq <- file.path(wide_dir, paste0(base, ".parquet"))

                chunk_wide <- to_wide(chunk)
                readr::write_csv(chunk_wide, fn_csv)
                arrow::write_parquet(chunk_wide, fn_parq)
            })
    }

    list(long_csv = long_csv, long_parquet = long_parq, wide_dir = wide_dir)
}
