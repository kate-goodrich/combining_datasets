clean_zcta <- function(
    in_shp,
    layer = NULL, # new: optionally specify a GPKG layer
    out_gpkg = "clean_data/county_census/canonical_2024.gpkg",
    out_layer = "zctas_500k",
    crs_out = 4326, # default to lon/lat
    also_save_5070 = FALSE, # optionally save Albers copy
    make_valid = TRUE,
    verbose = TRUE
) {
    if (!file.exists(in_shp)) {
        stop("[clean_zcta] Input file does not exist: ", in_shp)
    }
    out_dir <- dirname(out_gpkg)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    msg <- function(...) if (isTRUE(verbose)) message(...)

    pad_zip5 <- function(x) {
        x <- gsub("\\D", "", as.character(x))
        n <- nchar(x)
        if (length(n) && any(n < 5)) {
            x[n < 5] <- paste0(strrep("0", 5 - n[n < 5]), x[n < 5])
        }
        x
    }

    # ---- Read source data ----
    if (is.null(layer)) {
        msg("[clean_zcta] Reading: ", in_shp)
        zcta <- sf::read_sf(in_shp)
    } else {
        msg("[clean_zcta] Reading: ", in_shp, " (layer=", layer, ")")
        zcta <- sf::read_sf(in_shp, layer = layer)
    }

    # ---- Standardize GEOID ----
    id_candidates <- intersect(names(zcta), c("GEOID20", "GEOID", "ZCTA5CE20"))
    if (!length(id_candidates)) {
        stop(
            "[clean_zcta] No expected ZCTA ID column found (GEOID20/GEOID/ZCTA5CE20)."
        )
    }
    id_col <- id_candidates[1]
    zcta <- zcta[, c(id_col, setdiff(names(zcta), id_col))]
    names(zcta)[1] <- "geoid"
    zcta$geoid <- pad_zip5(zcta$geoid)

    # ---- Fix geometries ----
    if (isTRUE(make_valid)) {
        msg("[clean_zcta] Repairing geometries with sf::st_make_valid()")
        zcta <- tryCatch(
            sf::st_make_valid(zcta),
            error = function(e) {
                msg(
                    "[clean_zcta] st_make_valid() failed: ",
                    conditionMessage(e),
                    " — trying buffer(0)"
                )
                # fallback; temporarily ensure planar ops
                old_s2 <- sf::sf_use_s2()
                sf::sf_use_s2(FALSE)
                on.exit(sf::sf_use_s2(old_s2), add = TRUE)
                zcta$geometry <- sf::st_buffer(sf::st_geometry(zcta), 0)
                zcta
            }
        )
    }
    zcta <- sf::st_zm(zcta, drop = TRUE)
    zcta <- zcta[!sf::st_is_empty(zcta), , drop = FALSE]

    # ---- Reproject ----
    msg("[clean_zcta] Transforming CRS to EPSG:", crs_out)
    zcta_out <- sf::st_transform(zcta, crs_out)

    # ---- Write main layer ----
    existing_layers <- tryCatch(
        sf::st_layers(out_gpkg)$name,
        error = function(e) character(0)
    )
    append_flag <- out_layer %in% existing_layers
    msg(sprintf(
        "[clean_zcta] %s layer '%s' in %s",
        if (append_flag) "Appending to" else "Creating",
        out_layer,
        out_gpkg
    ))
    sf::st_write(
        zcta_out,
        dsn = out_gpkg,
        layer = out_layer,
        append = append_flag,
        quiet = TRUE
    )

    # ---- Optionally save 5070 copy ----
    if (isTRUE(also_save_5070)) {
        layer_5070 <- paste0(out_layer, "_epsg5070")
        msg("[clean_zcta] Also writing EPSG:5070 layer as '", layer_5070, "'")
        zcta_5070 <- sf::st_transform(zcta_out, 5070)
        append_flag_5070 <- layer_5070 %in% existing_layers
        sf::st_write(
            zcta_5070,
            dsn = out_gpkg,
            layer = layer_5070,
            append = append_flag_5070,
            quiet = TRUE
        )
    }

    invisible(normalizePath(out_gpkg))
}
