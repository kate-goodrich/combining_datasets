build_exposure_overall_long_streamed <- function(
    input_dir = "summary_sets",
    handoff_dir = "handoffs"
) {
    suppressPackageStartupMessages({
        library(readr)
        library(dplyr)
        library(stringr)
        library(arrow)
        library(tidyr)
        library(purrr)
    })

    # --- Set up output directory (long form only) ---
    long_dir <- file.path(handoff_dir, "zip_overall_long")
    dir.create(long_dir, recursive = TRUE, showWarnings = FALSE)

    # --- Match only 'overall_zip*' and 'static_zip*' CSVs ---
    pattern <- "^(overall_zip|static_zip).*\\.csv$"
    csv_files <- list.files(input_dir, pattern = pattern, full.names = TRUE)

    if (length(csv_files) == 0) {
        stop(sprintf(
            "No files matched in '%s' with pattern: %s",
            input_dir,
            pattern
        ))
    }

    # --- Normalization helper ---
    normalize_one <- function(file) {
        df <- readr::read_csv(file, show_col_types = FALSE)
        names(df) <- tolower(names(df))

        # geoid check
        if (!"geoid" %in% names(df)) {
            stop("Missing 'geoid' column in ", basename(file))
        }

        # variable name standardization
        if ("var" %in% names(df)) {
            df <- rename(df, variable = var)
        }
        if (!"variable" %in% names(df)) {
            stop("Missing 'variable' (or 'var') column in ", basename(file))
        }

        # pick the value column
        val_candidates <- c("value", "annual_mean", "mean", "val")
        have <- val_candidates[val_candidates %in% names(df)]
        if (length(have) == 0) {
            stop("Missing value column in ", basename(file))
        }
        if (have[1] != "value") {
            df <- rename(df, value = !!have[1])
        }

        # drop/handle agg if present
        if ("agg" %in% names(df)) {
            # keep only 'overall' if other values slipped in; then drop the column
            if (!all(is.na(df$agg) | df$agg == "overall")) {
                df <- df %>% filter(agg == "overall")
            }
            df$agg <- NULL
        }

        # clean variable names a bit to match your other pipelines
        df %>%
            mutate(
                variable = variable %>%
                    tolower() %>%
                    str_remove("_\\d{4}$") %>% # drop trailing year if embedded
                    str_remove("_clean$"),
                value = if_else(
                    str_starts(variable, "land_cover_") & is.na(value),
                    0,
                    value
                )
            ) %>%
            select(geoid, variable, value)
    }

    # --- Read & combine all files ---
    all_overall <- purrr::map_dfr(csv_files, normalize_one)

    # --- Write long outputs (CSV + Parquet) ---
    long_csv <- file.path(long_dir, "zip_overall.csv")
    long_parq <- file.path(long_dir, "zip_overall.parquet")

    readr::write_csv(all_overall, long_csv)
    arrow::write_parquet(all_overall, long_parq)

    list(
        long_csv = long_csv,
        long_parquet = long_parq,
        n_files_combined = length(csv_files),
        n_rows = nrow(all_overall),
        n_variables = dplyr::n_distinct(all_overall$variable),
        n_geoids = dplyr::n_distinct(all_overall$geoid)
    )
}
