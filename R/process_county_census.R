build_canonical_census <- function(
    in_gpkg,
    out_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    county_layer = "cb_2024_us_county_500k",
    tract_layer = "cb_2024_us_tract_500k",
    territory_statefps = c("60", "66", "69", "72", "78"),
    make_valid = FALSE,
    overwrite_layers = TRUE,
    quiet = TRUE
) {
    stopifnot(file.exists(in_gpkg))
    dir.create(dirname(out_gpkg), recursive = TRUE, showWarnings = FALSE)

    counties <- sf::st_read(in_gpkg, layer = county_layer, quiet = quiet)
    tracts <- sf::st_read(in_gpkg, layer = tract_layer, quiet = quiet)

    # Standardize names to lower-case so select() works regardless of source case
    names(counties) <- tolower(names(counties))
    names(tracts) <- tolower(names(tracts))

    # Filter out territories and pick canonical columns
    counties_us <- counties |>
        dplyr::filter(!statefp %in% territory_statefps) |>
        dplyr::select(dplyr::any_of(c(
            "geoid",
            "statefp",
            "countyfp",
            "name",
            "stusps",
            "state_name",
            "geom"
        )))

    tracts_us <- tracts |>
        dplyr::filter(!statefp %in% territory_statefps) |>
        dplyr::select(dplyr::any_of(c(
            "geoid",
            "statefp",
            "countyfp",
            "tractce",
            "name",
            "geom"
        )))

    if (make_valid) {
        counties_us <- sf::st_make_valid(counties_us)
        tracts_us <- sf::st_make_valid(tracts_us)
    }

    # Write layers (idempotent with delete_layer = TRUE)
    sf::st_write(
        counties_us,
        out_gpkg,
        layer = "counties_500k",
        delete_layer = overwrite_layers,
        quiet = quiet
    )
    sf::st_write(
        tracts_us,
        out_gpkg,
        layer = "tracts_500k",
        delete_layer = overwrite_layers,
        quiet = quiet
    )

    normalizePath(out_gpkg, mustWork = FALSE)
}
