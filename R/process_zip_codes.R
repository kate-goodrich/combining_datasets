clean_zcta <- function(
    in_shp,
    out_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    out_layer = "zctas_500k",
    crs_out = 5070,
    make_valid = TRUE,
    verbose = TRUE
) {
    if (!file.exists(in_shp)) {
        stop("[clean_zcta] Input shapefile does not exist: ", in_shp)
    }
    out_dir <- dirname(out_gpkg)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    if (isTRUE(verbose)) {
        message("[clean_zcta] Reading ZCTA shapefile: ", in_shp)
    }
    zcta <- sf::read_sf(in_shp)

    # Pick an ID column and rename to 'geoid'
    id_candidates <- intersect(names(zcta), c("GEOID20", "GEOID", "ZCTA5CE20"))
    if (length(id_candidates) == 0L) {
        stop(
            "[clean_zcta] No expected ZCTA ID column found (GEOID20/GEOID/ZCTA5CE20)."
        )
    }
    id_col <- id_candidates[1]
    zcta <- zcta[, c(id_col, setdiff(names(zcta), id_col))]
    names(zcta)[1] <- "geoid"
    zcta$geoid <- as.character(zcta$geoid)

    # Fix invalid geometries (optional)
    if (isTRUE(make_valid)) {
        if (requireNamespace("lwgeom", quietly = TRUE)) {
            if (isTRUE(verbose)) {
                message(
                    "[clean_zcta] Fixing invalid geometries with lwgeom::st_make_valid()"
                )
            }
            zcta <- lwgeom::st_make_valid(zcta)
        } else if (isTRUE(verbose)) {
            message(
                "[clean_zcta] Skipping st_make_valid(): 'lwgeom' not installed."
            )
        }
    }

    # Transform to EPSG:5070 (NAD83 / Conus Albers)
    if (isTRUE(verbose)) {
        message("[clean_zcta] Transforming CRS to EPSG:", crs_out)
    }
    zcta <- sf::st_transform(zcta, crs_out)

    # Append to GPKG as 'out_layer' (create new layer if it doesn't exist)
    append_flag <- FALSE
    if (file.exists(out_gpkg)) {
        existing_layers <- tryCatch(
            sf::st_layers(out_gpkg)$name,
            error = function(e) character(0)
        )
        append_flag <- out_layer %in% existing_layers
    }

    if (isTRUE(verbose)) {
        msg <- if (append_flag) "Appending" else "Creating"
        message(sprintf(
            "[clean_zcta] %s layer '%s' in %s",
            msg,
            out_layer,
            out_gpkg
        ))
    }

    sf::st_write(
        obj = zcta,
        dsn = out_gpkg,
        layer = out_layer,
        append = append_flag,
        quiet = TRUE
    )

    invisible(normalizePath(out_gpkg))
}
