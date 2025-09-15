# =============================================================================
# Canonicalize counties & tracts from either a GPKG or direct shapefile paths
# =============================================================================
build_canonical_census <- function(
    # EITHER provide in_gpkg + layer names ...
    in_gpkg = NULL,
    county_layer = "cb_2024_us_county_500k",
    tract_layer = "cb_2024_us_tract_500k",
    # ... OR provide direct shapefile paths (from download_counties_tracts)
    county_path = NULL,
    tract_path = NULL,

    out_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    territory_statefps = c("60", "66", "69", "72", "78"),
    make_valid = FALSE,
    overwrite_layers = TRUE,
    quiet = TRUE
) {
    # ---- Resolve inputs ----
    use_gpkg <- !is.null(in_gpkg)
    if (use_gpkg) {
        stopifnot(file.exists(in_gpkg))
    } else {
        if (is.null(county_path) || is.null(tract_path)) {
            stop(
                "Provide either 'in_gpkg' OR both 'county_path' and 'tract_path'."
            )
        }
        if (!file.exists(county_path)) {
            stop("county_path not found: ", county_path)
        }
        if (!file.exists(tract_path)) stop("tract_path not found: ", tract_path)
    }

    dir.create(dirname(out_gpkg), recursive = TRUE, showWarnings = FALSE)

    # ---- Read inputs ----
    if (use_gpkg) {
        counties <- sf::st_read(in_gpkg, layer = county_layer, quiet = quiet)
        tracts <- sf::st_read(in_gpkg, layer = tract_layer, quiet = quiet)
    } else {
        counties <- sf::read_sf(county_path)
        tracts <- sf::read_sf(tract_path)
    }

    # ---- Normalize names ----
    names(counties) <- tolower(names(counties))
    names(tracts) <- tolower(names(tracts))

    # ---- Filter out territories & select canonical columns ----
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

    # ---- Optionally fix geometries ----
    if (isTRUE(make_valid)) {
        counties_us <- sf::st_make_valid(counties_us)
        tracts_us <- sf::st_make_valid(tracts_us)
    }

    # ---- Write layers (idempotent with delete_layer) ----
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
