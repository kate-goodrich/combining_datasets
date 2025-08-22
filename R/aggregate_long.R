build_exposure_long <- function(
    agg = c("annual", "monthly"),
    level = c("county", "tract"),
    input_dir = "summary_sets",
    output_dir = NULL,
    write_csv = NULL,
    write_parquet = NULL
) {
    agg <- match.arg(agg)
    level <- match.arg(level)

    # Defaults dependent on agg/level
    if (is.null(output_dir)) {
        output_dir <- file.path("handoffs", paste0(agg, "_", level, "_long"))
    }
    if (is.null(write_csv)) {
        write_csv <- file.path(output_dir, paste0(agg, "_", level, ".csv"))
    }
    if (is.null(write_parquet)) {
        write_parquet <- file.path(
            output_dir,
            paste0(agg, "_", level, ".parquet")
        )
    }

    # Build a pattern that always includes:
    #   - requested agg_<level> (annual_*/monthly_*)
    #   - normal_<level> (normals go into both agg types)
    #   - static_<level> (statics go into both agg types)
    # This ensures statics and normals are included regardless of agg.
    pattern <- sprintf(
        "^(%s_%s|normal_%s|static_%s).*\\.csv$",
        agg,
        level,
        level,
        level
    )

    # ---- helper: process a single file into the long schema ----
    process_file <- function(file, is_monthly_output) {
        df <- readr::read_csv(file, show_col_types = FALSE)
        file_name <- basename(file)
        file_base <- tools::file_path_sans_ext(file_name)

        # normalize column names
        df <- dplyr::rename_with(df, ~ stringr::str_to_lower(.x))

        # align to expected names
        df <- dplyr::rename(
            df,
            variable = tidyselect::any_of(c("var", "variable")),
            value = tidyselect::any_of(c(
                "value",
                "annual_mean",
                "mean",
                "val"
            )),
            geoid = tidyselect::any_of(c("geoid", "geoid10", "geoid_county"))
        )

        # year handling
        if (!("year" %in% names(df))) {
            df <- dplyr::mutate(
                df,
                year = dplyr::case_when(
                    stringr::str_detect(file_name, "^normal_") ~ "normal",
                    stringr::str_detect(file_name, "^static_") ~ "static",
                    TRUE ~ NA_character_
                )
            )
        }

        # month handling for monthly outputs:
        # - if the output is monthly and column 'month' is missing, add NA (statics/annualized normals)
        # - coerce to integer when present and valid
        if (is_monthly_output) {
            if (!("month" %in% names(df))) {
                df$month <- NA_integer_
            } else {
                # try to coerce cleanly
                df$month <- suppressWarnings(as.integer(df$month))
            }
        }

        # finalize columns
        if (is_monthly_output) {
            df |>
                dplyr::mutate(
                    year = as.character(.data$year),
                    variable = .data$variable |>
                        stringr::str_to_lower() |>
                        stringr::str_remove("_\\d{4}$") |>
                        stringr::str_remove("_clean$")
                ) |>
                dplyr::select("geoid", "variable", "value", "month", "year") |>
                dplyr::mutate(source = file_base)
        } else {
            df |>
                dplyr::mutate(
                    year = as.character(.data$year),
                    variable = .data$variable |>
                        stringr::str_to_lower() |>
                        stringr::str_remove("_\\d{4}$") |>
                        stringr::str_remove("_clean$")
                ) |>
                dplyr::select("geoid", "variable", "value", "year") |>
                dplyr::mutate(source = file_base)
        }
    }

    # ---- find files ----
    csv_files <- list.files(input_dir, pattern = pattern, full.names = TRUE)
    if (length(csv_files) == 0) {
        stop(sprintf(
            "No files matched pattern in '%s': %s",
            input_dir,
            pattern
        ))
    }

    # ---- read/process/combine ----
    is_monthly <- identical(agg, "monthly")
    out <- purrr::map_dfr(
        csv_files,
        process_file,
        is_monthly_output = is_monthly
    )

    # Basic sanity: month range for monthly outputs
    if (is_monthly) {
        out$month <- ifelse(
            !is.na(out$month) & out$month %in% 1:12,
            out$month,
            out$month
        )
    }

    # ---- write outputs (optional) ----
    if (!is.null(write_csv)) {
        dir.create(dirname(write_csv), recursive = TRUE, showWarnings = FALSE)
        readr::write_csv(out, write_csv)
    }
    if (!is.null(write_parquet)) {
        dir.create(
            dirname(write_parquet),
            recursive = TRUE,
            showWarnings = FALSE
        )
        arrow::write_parquet(out, write_parquet)
    }

    out
}
