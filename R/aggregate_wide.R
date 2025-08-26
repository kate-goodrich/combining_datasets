build_exposure_wide_layers_from_dataset <- function(
    dataset_dir,
    agg = c("annual", "monthly"),
    level = c("county", "tract"),
    output_dir = NULL,
    fill_koppen_zero = TRUE
) {
    agg <- match.arg(agg)
    level <- match.arg(level)

    suppressPackageStartupMessages({
        library(arrow)
        library(dplyr)
        library(tidyr)
        library(stringr)
        library(readr)
    })

    if (is.null(output_dir)) {
        output_dir <- file.path("handoffs", paste0(level, "_", agg, "_layers"))
    }
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    ds <- arrow::open_dataset(dataset_dir, format = "parquet")

    # Helper to coerce and dedup within keys, but only for a subset we collect
    dedup_frame <- function(tbl, monthly = FALSE) {
        if (monthly) {
            tbl %>%
                group_by(geoid, year, month, variable) %>%
                summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
        } else {
            tbl %>%
                group_by(geoid, year, variable) %>%
                summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
        }
    }

    written <- character(0)

    # ---------- Static/normal (one file) ----------
    static_tbl <- ds %>%
        filter(!(stringr::str_detect(year, "^\\d{4}$"))) %>%
        select(geoid, variable, value, year)

    if (static_tbl %>% head(1) %>% collect() %>% nrow() > 0) {
        static_df <- static_tbl %>% collect() %>% dedup_frame(monthly = FALSE)

        # Collapse multiple static/normal sources to a single row per geoid/variable
        static_wide <- static_df %>%
            group_by(geoid, variable) %>%
            summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
            tidyr::pivot_wider(
                id_cols = geoid,
                names_from = variable,
                values_from = value
            ) %>%
            arrange(geoid)

        if (fill_koppen_zero) {
            kop_cols <- grep("^koppen_", names(static_wide), value = TRUE)
            if (length(kop_cols)) {
                static_wide[kop_cols] <- lapply(
                    static_wide[kop_cols],
                    function(x) {
                        x[is.na(x)] <- 0
                        x
                    }
                )
            }
        }

        p <- file.path(output_dir, paste0(level, "_static.csv"))
        readr::write_csv(static_wide, p)
        written <- c(written, p)
    }

    # ---------- Dynamic ----------
    dyn_keys <- ds %>%
        filter(stringr::str_detect(year, "^\\d{4}$")) %>%
        select(year, dplyr::any_of("month")) %>%
        distinct()

    dyn_keys_df <- dyn_keys %>% collect()

    if (nrow(dyn_keys_df)) {
        if (agg == "annual") {
            years <- sort(unique(dyn_keys_df$year))
            for (y in years) {
                yr_tbl <- ds %>%
                    filter(year == y) %>%
                    select(geoid, variable, value, year)

                yr_df <- yr_tbl %>% collect() %>% dedup_frame(monthly = FALSE)

                yr_wide <- yr_df %>%
                    select(geoid, variable, value) %>%
                    tidyr::pivot_wider(
                        id_cols = geoid,
                        names_from = variable,
                        values_from = value
                    ) %>%
                    arrange(geoid)

                if (fill_koppen_zero) {
                    kop_cols <- grep("^koppen_", names(yr_wide), value = TRUE)
                    if (length(kop_cols)) {
                        yr_wide[kop_cols] <- lapply(
                            yr_wide[kop_cols],
                            function(x) {
                                x[is.na(x)] <- 0
                                x
                            }
                        )
                    }
                }

                out_path <- file.path(output_dir, paste0(level, "_", y, ".csv"))
                readr::write_csv(yr_wide, out_path)
                written <- c(written, out_path)
            }
        } else {
            # monthly
            dyn_keys_df <- dyn_keys_df %>% filter(!is.na(month))
            dyn_keys_df$month <- as.integer(dyn_keys_df$month)
            dyn_keys_df <- dyn_keys_df %>% arrange(year, month)

            for (i in seq_len(nrow(dyn_keys_df))) {
                y <- dyn_keys_df$year[i]
                m <- dyn_keys_df$month[i]
                mm <- sprintf("%02d", m)

                ym_tbl <- ds %>%
                    filter(year == y, month == m) %>%
                    select(geoid, variable, value, year, month)

                ym_df <- ym_tbl %>% collect() %>% dedup_frame(monthly = TRUE)

                ym_wide <- ym_df %>%
                    select(geoid, variable, value) %>%
                    tidyr::pivot_wider(
                        id_cols = geoid,
                        names_from = variable,
                        values_from = value
                    ) %>%
                    arrange(geoid)

                if (fill_koppen_zero) {
                    kop_cols <- grep("^koppen_", names(ym_wide), value = TRUE)
                    if (length(kop_cols)) {
                        ym_wide[kop_cols] <- lapply(
                            ym_wide[kop_cols],
                            function(x) {
                                x[is.na(x)] <- 0
                                x
                            }
                        )
                    }
                }

                out_path <- file.path(
                    output_dir,
                    paste0(level, "_", y, "_", mm, ".csv")
                )
                readr::write_csv(ym_wide, out_path)
                written <- c(written, out_path)
            }
        }
    }

    unique(written)
}
