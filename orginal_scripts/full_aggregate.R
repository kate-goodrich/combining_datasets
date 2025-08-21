# FULLY AGGREGATING EVERYTHING

# need long parquet for annual_county, annual_tract, monthly_county, monthly_tract
# need annual_county series, monthly_county series, monthly_county series, monthly_tract

# annual_county parquet

library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(tools)
library(arrow)
library(tidyr)

# Match any files containing these prefixes
csv_files <- list.files(
    "summary_sets",
    pattern = "^(annual_county|normal_county|static_county).*\\.csv$",
    full.names = TRUE
)

# Helper to read, standardize column names, and add 'year' if missing
process_file <- function(file) {
    df <- read_csv(file, show_col_types = FALSE)
    file_name <- basename(file)
    file_base <- file_path_sans_ext(file_name)

    # Normalize column names
    df <- df |> rename_with(~ str_to_lower(.x))

    df <- df |>
        rename(
            variable = any_of(c("var", "variable")),
            value = any_of(c("value", "annual_mean"))
        )

    if (!"year" %in% names(df)) {
        df <- df |>
            mutate(
                year = if (str_detect(file_name, "normal_county")) {
                    "normal"
                } else {
                    "static"
                }
            )
    }

    df |>
        mutate(year = as.character(year)) |> # <- force to character
        select(geoid, variable, value, year) |>
        mutate(source = file_base)
}

# Read, standardize, and combine
all_data <- map_dfr(csv_files, process_file)


all_data <- all_data |>
    mutate(
        variable = variable |>
            str_to_lower() |> # 1. lowercase
            str_remove("_\\d{4}$") |> # 2. remove year suffix (4-digit years)
            str_remove("_clean$") # 3. remove trailing "_clean"
    )


dir.create("handoffs", recursive = TRUE, showWarnings = FALSE)

write_csv(all_data, "handoffs/county_annual_long/county_annual.csv")
write_parquet(all_data, "handoffs/county_annual_long/county_annual.parquet")


# annual_county series

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

# Ensure output folder exists
dir.create(
    "handoffs/county_annual_layers",
    recursive = TRUE,
    showWarnings = FALSE
)

# ---- 1) Deduplicate within (geoid, year, variable) before widening ----
# If multiple rows exist for the same key, take the mean (deterministic & safe).
all_data_dedup <- all_data %>%
    mutate(year = as.character(year)) %>%
    group_by(geoid, year, variable) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

# ---- 2) Split static/normal vs numeric years ----
is_numeric_year <- function(x) {
    !is.na(suppressWarnings(as.integer(x))) & str_detect(x, "^[0-9]{4}$")
}

static_long <- all_data_dedup %>%
    filter(!is_numeric_year(year)) %>%
    mutate(
        year = if_else(
            str_detect(year, "(?i)normal"), # case-insensitive match
            "normal",
            "static"
        )
    )


dynamic_long <- all_data_dedup %>%
    filter(is_numeric_year(year))

# ---- 3) Pivot wider: variables become columns ----
# Static/normal -> one file
if (nrow(static_long) > 0) {
    static_wide <- static_long %>%
        select(geoid, variable, value) %>% # 'year' no longer needed here
        group_by(geoid, variable) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(
            id_cols = geoid,
            names_from = variable,
            values_from = value
        ) %>%
        arrange(geoid)

    # Replace NA with 0 for any columns starting with "koppen_"
    static_wide <- static_wide %>%
        mutate(across(starts_with("koppen_"), ~ replace_na(.x, 0)))

    write_csv(static_wide, "handoffs/county_annual_layers/county_static.csv")
}

# ---- 4) Per-year wide CSVs ----
years <- dynamic_long %>%
    distinct(year) %>%
    arrange(year) %>%
    pull(year)

walk(years, function(y) {
    yr_wide <- dynamic_long %>%
        filter(year == y) %>%
        select(geoid, variable, value) %>%
        group_by(geoid, variable) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(
            id_cols = geoid,
            names_from = variable,
            values_from = value
        ) %>%
        arrange(geoid)

    out_path <- file.path(
        "handoffs",
        "county_annual_layers",
        paste0("county_", y, ".csv")
    )
    write_csv(yr_wide, out_path)
})
