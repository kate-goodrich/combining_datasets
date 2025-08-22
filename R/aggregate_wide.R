build_exposure_wide_layers <- function(
    data,
    agg = c("annual", "monthly"),
    level = c("county", "tract"),
    output_dir = NULL,
    fill_koppen_zero = TRUE
) {
    agg <- match.arg(agg)
    level <- match.arg(level)

    stopifnot(is.data.frame(data))
    required_cols <- c("geoid", "variable", "value", "year")
    if (agg == "monthly") {
        required_cols <- c(required_cols, "month")
    }
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols)) {
        stop(
            "Missing required columns in `data`: ",
            paste(missing_cols, collapse = ", ")
        )
    }

    if (is.null(output_dir)) {
        output_dir <- file.path("handoffs", paste0(level, "_", agg, "_layers"))
    }
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    suppressPackageStartupMessages({
        library(dplyr)
        library(tidyr)
        library(stringr)
        library(purrr)
        library(readr)
    })

    # ---- 1) Deduplicate within keys ----
    data <- data %>% mutate(year = as.character(.data$year))
    if (agg == "annual") {
        data_dedup <- data %>%
            group_by(.data$geoid, .data$year, .data$variable) %>%
            summarise(value = mean(.data$value, na.rm = TRUE), .groups = "drop")
    } else {
        data_dedup <- data %>%
            mutate(month = suppressWarnings(as.integer(.data$month))) %>%
            group_by(.data$geoid, .data$year, .data$month, .data$variable) %>%
            summarise(value = mean(.data$value, na.rm = TRUE), .groups = "drop")
    }

    # ---- 2) Split static/normal vs numeric years ----
    is_numeric_year <- function(x) {
        !is.na(suppressWarnings(as.integer(x))) & str_detect(x, "^[0-9]{4}$")
    }

    static_long <- data_dedup %>%
        filter(!is_numeric_year(.data$year)) %>%
        mutate(
            year = if_else(
                str_detect(.data$year, "(?i)normal"),
                "normal",
                "static"
            )
        )

    dynamic_long <- data_dedup %>% filter(is_numeric_year(.data$year))

    # ---- 3) Static wide (one file) ----
    written_files <- character(0)
    if (nrow(static_long) > 0) {
        static_wide <- static_long %>%
            {
                if (agg == "monthly") {
                    select(., .data$geoid, .data$variable, .data$value)
                } else {
                    select(., .data$geoid, .data$variable, .data$value)
                }
            } %>%
            group_by(.data$geoid, .data$variable) %>%
            summarise(
                value = mean(.data$value, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            pivot_wider(
                id_cols = .data$geoid,
                names_from = .data$variable,
                values_from = .data$value
            ) %>%
            arrange(.data$geoid)

        if (fill_koppen_zero) {
            static_wide <- static_wide %>%
                mutate(across(starts_with("koppen_"), ~ replace_na(.x, 0)))
        }

        static_path <- file.path(output_dir, paste0(level, "_static.csv"))
        write_csv(static_wide, static_path)
        written_files <- c(written_files, static_path)
    }

    # ---- 4) Dynamic wide (per year or per year-month) ----
    if (nrow(dynamic_long) > 0) {
        if (agg == "annual") {
            years <- dynamic_long %>%
                distinct(.data$year) %>%
                arrange(.data$year) %>%
                pull(.data$year)
            files <- map_chr(years, function(y) {
                yr_wide <- dynamic_long %>%
                    filter(.data$year == y) %>%
                    select(.data$geoid, .data$variable, .data$value) %>%
                    group_by(.data$geoid, .data$variable) %>%
                    summarise(
                        value = mean(.data$value, na.rm = TRUE),
                        .groups = "drop"
                    ) %>%
                    pivot_wider(
                        id_cols = .data$geoid,
                        names_from = .data$variable,
                        values_from = .data$value
                    ) %>%
                    arrange(.data$geoid)

                if (fill_koppen_zero) {
                    yr_wide <- yr_wide %>%
                        mutate(across(
                            starts_with("koppen_"),
                            ~ replace_na(.x, 0)
                        ))
                }

                out_path <- file.path(output_dir, paste0(level, "_", y, ".csv"))
                write_csv(yr_wide, out_path)
                out_path
            })
            written_files <- c(written_files, files)
        } else {
            # monthly
            # keep valid months or NA (if any slipped in)
            dynamic_long <- dynamic_long %>%
                mutate(month = ifelse(month %in% 1:12, month, NA_integer_))
            ym <- dynamic_long %>%
                filter(!is.na(.data$month)) %>%
                distinct(.data$year, .data$month) %>%
                arrange(.data$year, .data$month)

            files <- pmap_chr(ym, function(year, month) {
                mm <- sprintf("%02d", as.integer(month))
                ym_wide <- dynamic_long %>%
                    filter(.data$year == year, .data$month == month) %>%
                    select(.data$geoid, .data$variable, .data$value) %>%
                    group_by(.data$geoid, .data$variable) %>%
                    summarise(
                        value = mean(.data$value, na.rm = TRUE),
                        .groups = "drop"
                    ) %>%
                    pivot_wider(
                        id_cols = .data$geoid,
                        names_from = .data$variable,
                        values_from = .data$value
                    ) %>%
                    arrange(.data$geoid)

                if (fill_koppen_zero) {
                    ym_wide <- ym_wide %>%
                        mutate(across(
                            starts_with("koppen_"),
                            ~ replace_na(.x, 0)
                        ))
                }

                out_path <- file.path(
                    output_dir,
                    paste0(level, "_", year, "_", mm, ".csv")
                )
                write_csv(ym_wide, out_path)
                out_path
            })
            written_files <- c(written_files, files)
        }
    }

    unique(written_files)
}
