# ---- Library paths & packages ----
.libPaths("/usr/local/lib/R/site-library")

library(targets)
library(tarchetypes)
library(amadeus)
library(dplyr)
library(tidyr)
library(archive)
library(languageserver)
library(units)
library(sf)
library(lwgeom)
library(lubridate)
library(terra)
library(exactextractr)
library(stringr)
library(purrr)
library(readr)

# ---- Source local R helpers ----
purrr::walk(list.files("R", full.names = TRUE, pattern = "\\.R$"), source)

# ---- Global constants ----
YEARS <- 2010:2024
MONTHS <- 1:12
CENSUS_GPKG <- "clean_data/county_census/canonical_2024.gpkg"
LAYER <- list(county = "counties_500k", tract = "tracts_500k")

# ---- targets options ----
tar_option_set(
    packages = c(
        "targets",
        "tarchetypes",
        "amadeus",
        "dplyr",
        "tidyr",
        "archive",
        "languageserver",
        "terra",
        "purrr",
        "stringr",
        "tibble",
        "units",
        "sf",
        "lwgeom",
        "lubridate",
        "exactextractr",
        "readr"
    ),
    error = "continue", # continue on error
    memory = "transient", # use transient memory
    library = "/usr/local/lib/R/site-library",
    format = "rds" # default storage format
)


# =========================================
# Pipeline
# =========================================
list(
    # ======================
    # LOAD (raw downloads)
    # ======================

    # All download functions are idempotent - delete the file if you want it to redo

    tar_target(gmted, load_gmted(save_dir = "raw_data/gmted")),
    tar_target(gridmet, load_gridmet(save_dir = "raw_data/gridmet")),
    tar_target(groads, load_groads()),
    tar_target(hms, load_hms(save_dir = "raw_data/hms")),
    tar_target(
        huc,
        load_huc(
            save_dir = "raw_data/huc",
            container_path = "raw_data/huc/NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z"
        )
    ),
    tar_target(
        koppen_geiger,
        load_koppen_geiger(save_dir = "raw_data/koppen_geiger")
    ),
    tar_target(merra2, load_merra2(save_dir = "raw_data/merra2")),
    tar_target(modis, load_modis(dir = "raw_data/modis")),
    tar_target(nlcd, load_nlcd(base_dir = "raw_data/nlcd")),
    tar_target(prism, download_and_unzip_prism(save_dir = "raw_data/prism")),
    tar_target(
        terraclimate,
        load_terraclimate(save_dir = "raw_data/terraclimate")
    ),
    tar_target(tri, load_tri(save_dir = "raw_data/tri")),

    # ======================
    # PROCESS (clean)
    # ======================

    tar_target(
        gmted_outputs,
        clean_gmted(),
        format = "file"
    ),

    tar_target(
        gridmet_files,
        clean_gridmet(),
        format = "file"
    ),

    tar_target(
        groads_files,
        clean_groads(),
        format = "file"
    ),

    tar_target(
        hms_files,
        clean_hms(years = YEARS, months = MONTHS, overwrite = TRUE),
        format = "file"
    ),

    tar_target(
        huc_files,
        clean_huc_layers(),
        format = "file"
    ),

    tar_target(
        kg_files_out,
        clean_koppen_geiger(),
        format = "file"
    ),

    tar_target(
        merra2_files, # single target
        clean_merra2(
            years = 2011:2024,
            raw_dir = "./raw_data/merra2",
            out_dir = "./clean_data/merra2_clean",
            use_collection_by_year = TRUE,
            overwrite = TRUE,
            verbose = TRUE
        ),
        format = "file"
    ),

    tar_target(
        modis_clean_log,
        build_modis_clean(
            modis_vars = list(
                MOD09A1 = c(
                    "sur_refl_b01",
                    "sur_refl_b02",
                    "sur_refl_b03",
                    "sur_refl_b04",
                    "sur_refl_b05",
                    "sur_refl_b06",
                    "sur_refl_b07",
                    "sur_refl_day_of_year"
                ),
                MOD11A2 = c("LST_Day_1km", "LST_Night_1km"),
                MOD13A3 = c("NDVI", "EVI")
            ),
            raw_dir = "raw_data/modis",
            out_dir = "clean_data/modis_clean",
            start_date = "2010-01-01",
            end_date = "2024-12-31",
            fun_agg = "mean",
            overwrite = TRUE,
            verbose = TRUE
        ),
        format = "rds"
    ),

    tar_target(
        nlcd_files,
        clean_nlcd(),
        format = "file"
    ),

    tar_target(
        prism_files,
        clean_prism_normals(),
        format = "file"
    ),

    tar_target(
        terraclimate_files,
        clean_terraclimate(),
        format = "file"
    ),

    tar_target(
        tri_files,
        clean_tri(),
        format = "file"
    ),

    # ======================
    # LOADING TRACTS AND COUNTIES
    # ======================

    tar_target(
        canonical_gpkg,
        build_canonical_census(
            in_gpkg = "raw_data/county_census/cb_2024_us_all_500k.gpkg",
            out_gpkg = CENSUS_GPKG,
            make_valid = TRUE
        ),
        format = "file"
    ),

    tar_target(
        counties_500k,
        sf::st_read(canonical_gpkg, layer = LAYER$county, quiet = TRUE)
    ),

    tar_target(
        tracts_500k,
        sf::st_read(canonical_gpkg, layer = LAYER$tract, quiet = TRUE)
    ),

    # ======================
    # CALCULATE
    # ======================

    # ---- GMTED (static) ----
    tar_target(
        gmted_static_county,
        static_zonal_summary(
            tif_dir = "clean_data/gmted_clean",
            level = "county",
            zones_gpkg = CENSUS_GPKG,
            zone_layer = LAYER$county,
            write_csv = "summary_sets/static_county_gmted.csv"
        )
    ),
    tar_target(
        gmted_static_tract,
        static_zonal_summary(
            tif_dir = "clean_data/gmted_clean",
            level = "tract",
            zones_gpkg = CENSUS_GPKG,
            zone_layer = LAYER$tract,
            write_csv = "summary_sets/static_tract_gmted.csv"
        )
    ),

    # ---- GRIDMET ----
    tar_target(
        gridmet_county_annual,
        gridmet_zonal(
            tif_dir = "clean_data/gridmet_clean",
            level = "county",
            zones_gpkg = CENSUS_GPKG,
            aggregate_to = "annual",
            chunk_size = 365,
            write_csv = "summary_sets/annual_county_gridmet.csv"
        )
    ),
    tar_target(
        gridmet_county_monthly,
        gridmet_zonal(
            tif_dir = "clean_data/gridmet_clean",
            level = "county",
            zones_gpkg = CENSUS_GPKG,
            aggregate_to = "monthly",
            chunk_size = 365,
            write_csv = "summary_sets/monthly_county_gridmet.csv"
        )
    ),
    tar_target(
        gridmet_tract_annual,
        gridmet_zonal(
            tif_dir = "clean_data/gridmet_clean",
            level = "tract",
            zones_gpkg = CENSUS_GPKG,
            zone_layer = LAYER$tract, # optional if your gpkg has this name
            aggregate_to = "annual",
            chunk_size = 365,
            write_csv = "summary_sets/annual_tract_gridmet.csv"
        )
    ),
    tar_target(
        gridmet_tract_monthly,
        gridmet_zonal(
            tif_dir = "clean_data/gridmet_clean",
            level = "tract",
            zones_gpkg = CENSUS_GPKG,
            zone_layer = LAYER$tract, # optional if your gpkg has this name
            aggregate_to = "monthly",
            chunk_size = 365,
            write_csv = "summary_sets/monthly_tract_gridmet.csv"
        )
    ),

    # ---- GROADS (static) ----
    tar_target(
        groads_county_long,
        road_density_zonal(
            roads_gpkg = "clean_data/groads_clean/groads_clean.gpkg",
            zones_gpkg = CENSUS_GPKG,
            level = "county",
            write_csv = "summary_sets/static_county_groads.csv"
        )
    ),
    tar_target(
        groads_tract_long,
        road_density_zonal(
            roads_gpkg = "clean_data/groads_clean/groads_clean.gpkg",
            zones_gpkg = CENSUS_GPKG,
            level = "tract",
            write_csv = "summary_sets/static_tract_groads.csv"
        )
    ),

    # ---- HMS ----
    tar_target(
        hms_county_annual,
        hms_fire_exposure(
            hms_dir = "clean_data/hms_clean",
            level = "county",
            agg = "annual",
            write_csv = "summary_sets/hms_county_annual.csv"
        )
    ),
    tar_target(
        hms_county_monthly,
        hms_fire_exposure(
            hms_dir = "clean_data/hms_clean",
            level = "county",
            agg = "monthly",
            write_csv = "summary_sets/hms_county_monthly.csv"
        )
    ),
    tar_target(
        hms_tract_annual,
        hms_fire_exposure(
            hms_dir = "clean_data/hms_clean",
            level = "tract",
            agg = "annual",
            write_csv = "summary_sets/hms_tract_annual.csv"
        )
    ),
    tar_target(
        hms_tract_monthly,
        hms_fire_exposure(
            hms_dir = "clean_data/hms_clean",
            level = "tract",
            agg = "monthly",
            write_csv = "summary_sets/hms_tract_monthly.csv"
        )
    ),

    # ---- HUC (static) ----
    tar_target(
        county_huc2,
        huc2_from_huc12_gpkg(
            huc12_gpkg = "/ddn/gs1/group/set/chords/combining_datasets/clean_data/huc_clean/HUC12.gpkg",
            zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            level = "county",
            id_col = "geoid",
            write_csv = "/ddn/gs1/group/set/chords/combining_datasets/summary_sets/static_county_huc.csv"
        ),
        format = "parquet" # optional; use if you want parquet instead of rds
    ),

    # Tract-level HUC2 coverage
    tar_target(
        tract_huc2,
        huc2_from_huc12_gpkg(
            huc12_gpkg = "/ddn/gs1/group/set/chords/combining_datasets/clean_data/huc_clean/HUC12.gpkg",
            zones_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            level = "tract",
            id_col = "geoid",
            write_csv = "/ddn/gs1/group/set/chords/combining_datasets/summary_sets/static_tract_huc.csv"
        )
    ),

    # ---- Köppen–Geiger (static) ----
    tar_target(
        koppen_county_long,
        koppen_geiger_summary(
            level = "county",
            write_csv = "summary_sets/static_county_koppen_geiger.csv"
        )
    ),
    tar_target(
        koppen_tract_long,
        koppen_geiger_summary(
            level = "tract",
            write_csv = "summary_sets/static_tract_koppen_geiger.csv"
        )
    ),

    # ---- MERRA-2 ----
    tar_target(
        merra2_county_annual,
        merra2_summary(
            tif_dir = "clean_data/merra2_clean",
            zones_gpkg = CENSUS_GPKG,
            level = "county",
            agg = "annual",
            write_csv = "summary_sets/annual_county_merra2.csv"
        ),
        format = "rds"
    ),
    tar_target(
        merra2_county_monthly,
        merra2_summary(
            tif_dir = "clean_data/merra2_clean",
            zones_gpkg = CENSUS_GPKG,
            level = "county",
            agg = "monthly",
            write_csv = "summary_sets/monthly_county_merra2.csv"
        ),
        format = "rds"
    ),
    tar_target(
        merra2_tract_annual,
        merra2_summary(
            tif_dir = "clean_data/merra2_clean",
            zones_gpkg = CENSUS_GPKG,
            level = "tract",
            agg = "annual",
            write_csv = "summary_sets/annual_tract_merra2.csv"
        ),
        format = "rds"
    ),
    tar_target(
        merra2_tract_monthly,
        merra2_summary(
            tif_dir = "clean_data/merra2_clean",
            zones_gpkg = CENSUS_GPKG,
            level = "tract",
            agg = "monthly",
            write_csv = "summary_sets/monthly_tract_merra2.csv"
        ),
        format = "rds"
    ),

    # ---- MODIS ----
    tar_target(
        modis_county_annual,
        summarize_modis(
            level = "county",
            agg = "annual",
            zones_gpkg = CENSUS_GPKG,
            write_csv = "summary_sets/annual_county_modis.csv"
        ),
        format = "qs"
    ),
    tar_target(
        modis_county_monthly,
        summarize_modis(
            level = "county",
            agg = "monthly",
            zones_gpkg = CENSUS_GPKG,
            write_csv = "summary_sets/monthly_county_modis.csv"
        ),
        format = "qs"
    ),
    tar_target(
        modis_tract_annual,
        summarize_modis(
            level = "tract",
            agg = "annual",
            zones_gpkg = CENSUS_GPKG,
            write_csv = "summary_sets/annual_tract_modis.csv"
        ),
        format = "qs"
    ),
    tar_target(
        modis_tract_monthly,
        summarize_modis(
            level = "tract",
            agg = "monthly",
            zones_gpkg = CENSUS_GPKG,
            write_csv = "summary_sets/monthly_tract_modis.csv"
        ),
        format = "qs"
    ),

    # === NLCD Summaries ===

    # 1. County-level, Annual
    tar_target(
        county_nlcd_annual,
        summarize_nlcd(
            level = "county",
            agg = "annual",
            write_csv = "summary_sets/annual_county_nlcd.csv"
        ),
        format = "rds"
    ),

    # 2. County-level, Monthly
    tar_target(
        county_nlcd_monthly,
        summarize_nlcd(
            level = "county",
            agg = "monthly",
            write_csv = "summary_sets/monthly_county_nlcd.csv"
        ),
        format = "rds"
    ),

    # 3. Tract-level, Annual
    tar_target(
        tract_nlcd_annual,
        summarize_nlcd(
            level = "tract",
            agg = "annual",
            write_csv = "summary_sets/annual_tract_nlcd.csv"
        ),
        format = "rds"
    ),

    # 4. Tract-level, Monthly
    tar_target(
        tract_nlcd_monthly,
        summarize_nlcd(
            level = "tract",
            agg = "monthly",
            write_csv = "summary_sets/monthly_tract_nlcd.csv"
        ),
        format = "rds"
    ),

    # ---- PRISM normals ----
    # County monthly normals (keeps 12 months)
    tar_target(
        prism_county_monthly_normals,
        prism_normals_from_tifs(
            input_dir = "clean_data/prism_clean",
            zones_gpkg = CENSUS_GPKG,
            level = "county",
            agg = "monthly",
            id_col = "geoid",
            write_csv = "summary_sets/normal_monthly_county_prism.csv"
        )
    ),
    tar_target(
        prism_county_annual_normals,
        prism_normals_from_tifs(
            input_dir = "clean_data/prism_clean",
            zones_gpkg = CENSUS_GPKG,
            level = "county",
            agg = "annual",
            id_col = "geoid",
            write_csv = "summary_sets/normal_annual_county_prism.csv"
        )
    ),
    # Tract monthly normals (keeps 12 months)
    tar_target(
        prism_tract_monthly_normals,
        prism_normals_from_tifs(
            input_dir = "clean_data/prism_clean",
            zones_gpkg = CENSUS_GPKG,
            level = "tract",
            agg = "monthly",
            id_col = "geoid",
            write_csv = "summary_sets/normal_monthly_tract_prism.csv"
        )
    ),
    # Tract annual normals (mean of months 1..12)
    tar_target(
        prism_tract_annual_normals,
        prism_normals_from_tifs(
            input_dir = "clean_data/prism_clean",
            zones_gpkg = CENSUS_GPKG,
            level = "tract",
            agg = "annual",
            id_col = "geoid",
            write_csv = "summary_sets/normal_annual_tract_prism.csv"
        )
    ),

    # ---- TerraClimate ----
    tar_target(
        terraclimate_county_annual,
        summarize_terraclimate(
            tif_dir = "clean_data/terraclimate_clean",
            county_gpkg = CENSUS_GPKG,
            level = "county",
            agg = "annual",
            write_csv = "summary_sets/annual_county_terraclimate.csv"
        ),
        format = "file"
    ),
    tar_target(
        terraclimate_county_monthly,
        summarize_terraclimate(
            tif_dir = "clean_data/terraclimate_clean",
            county_gpkg = CENSUS_GPKG,
            level = "county",
            agg = "monthly",
            write_csv = "summary_sets/monthly_county_terraclimate.csv"
        ),
        format = "file"
    ),
    tar_target(
        terraclimate_tract_annual,
        summarize_terraclimate(
            tif_dir = "clean_data/terraclimate_clean",
            county_gpkg = CENSUS_GPKG,
            level = "tract",
            agg = "annual",
            write_csv = "summary_sets/annual_tract_terraclimate.csv"
        ),
        format = "file"
    ),
    tar_target(
        terraclimate_tract_monthly,
        summarize_terraclimate(
            tif_dir = "clean_data/terraclimate_clean",
            county_gpkg = CENSUS_GPKG,
            level = "tract",
            agg = "monthly",
            write_csv = "summary_sets/monthly_tract_terraclimate.csv"
        ),
        format = "file"
    ),

    # ---- TRI ----
    # COUNTY ANNUAL
    tar_target(
        tri_county_annual,
        summarise_tri_air_totals(
            tri_dir = "clean_data/tri_clean",
            county_gpkg = CENSUS_GPKG,
            level = "county",
            agg = "annual"
        )
    ),
    tar_target(
        tri_county_annual_csv,
        {
            out_path <- "summary_sets/annual_county_tri.csv"
            dir.create(
                dirname(out_path),
                showWarnings = FALSE,
                recursive = TRUE
            )
            readr::write_csv(tri_county_annual, out_path)
            out_path
        },
        format = "file"
    ),

    # COUNTY MONTHLY
    tar_target(
        tri_county_monthly,
        summarise_tri_air_totals(
            tri_dir = "clean_data/tri_clean",
            county_gpkg = CENSUS_GPKG,
            level = "county",
            agg = "monthly"
        )
    ),
    tar_target(
        tri_county_monthly_csv,
        {
            out_path <- "summary_sets/monthly_county_tri.csv"
            dir.create(
                dirname(out_path),
                showWarnings = FALSE,
                recursive = TRUE
            )
            readr::write_csv(tri_county_monthly, out_path)
            out_path
        },
        format = "file"
    ),

    # TRACT ANNUAL
    tar_target(
        tri_tract_annual,
        summarise_tri_air_totals(
            tri_dir = "clean_data/tri_clean",
            county_gpkg = CENSUS_GPKG,
            level = "tract",
            agg = "annual"
        )
    ),
    tar_target(
        tri_tract_annual_csv,
        {
            out_path <- "summary_sets/annual_tract_tri.csv"
            dir.create(
                dirname(out_path),
                showWarnings = FALSE,
                recursive = TRUE
            )
            readr::write_csv(tri_tract_annual, out_path)
            out_path
        },
        format = "file"
    ),

    # TRACT MONTHLY
    tar_target(
        tri_tract_monthly,
        summarise_tri_air_totals(
            tri_dir = "clean_data/tri_clean",
            county_gpkg = CENSUS_GPKG,
            level = "tract",
            agg = "monthly"
        )
    ),
    tar_target(
        tri_tract_monthly_csv,
        {
            out_path <- "summary_sets/monthly_tract_tri.csv"
            dir.create(
                dirname(out_path),
                showWarnings = FALSE,
                recursive = TRUE
            )
            readr::write_csv(tri_tract_monthly, out_path)
            out_path
        },
        format = "file"
    ),

    # ======================
    # AGGREGATE
    # ======================

    # County - Annual
    tar_target(
        county_annual,
        build_exposure_long_streamed(
            agg = "annual",
            level = "county",
            input_dir = "summary_sets",
            handoff_dir = "handoffs"
        ),
        format = "file" # output is files on disk
    ),

    # County - Monthly
    tar_target(
        county_monthly,
        build_exposure_long_streamed(
            agg = "monthly",
            level = "county",
            input_dir = "summary_sets",
            handoff_dir = "handoffs"
        ),
        format = "file"
    ),

    # Tract - Annual
    tar_target(
        tract_annual,
        build_exposure_long_streamed(
            agg = "annual",
            level = "tract",
            input_dir = "summary_sets",
            handoff_dir = "handoffs"
        ),
        format = "file"
    ),

    # Tract - Monthly
    tar_target(
        tract_monthly,
        build_exposure_long_streamed(
            agg = "monthly",
            level = "tract",
            input_dir = "summary_sets",
            handoff_dir = "handoffs"
        ),
        format = "file"
    ),

    # ---- Bundle all handoffs into a zip ----
    tar_target(
        handoffs_zip,
        {
            # run after all handoffs are created
            zip_handoffs(
                handoffs_dir = "handoffs",
                zipfile = "handoffs.zip",
                junk_paths = FALSE
            )
        },
        format = "file" # so targets tracks the zip
    ),

    # FIGURES
    # huc

    tar_target(
        county_annual_ds,
        arrow::open_dataset("handoffs/county_annual_long/county_annual.parquet")
    ),
    tar_target(
        county_monthly_ds,
        arrow::open_dataset(
            "handoffs/county_monthly_long/county_monthly.parquet"
        )
    ),
    tar_target(
        tract_annual_ds,
        arrow::open_dataset("handoffs/tract_annual_long/tract_annual.parquet")
    ),
    tar_target(
        tract_monthly_ds,
        arrow::open_dataset("handoffs/tract_monthly_long/tract_monthly.parquet")
    ),

    # GIF outputs (tracked as files)
    tar_target(
        gif_county_annual,
        animate_hms_smoke(
            data = county_annual_ds,
            level = "county",
            agg = "annual",
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            out_gif = "figures/county_annual_hms_smoke.gif",
            include_alaska = TRUE,
            include_hawaii = FALSE
        ),
        format = "file"
    ),
    tar_target(
        gif_county_monthly,
        animate_hms_smoke(
            data = county_monthly_ds,
            level = "county",
            agg = "monthly",
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            out_gif = "figures/county_monthly_hms_smoke.gif",
            include_alaska = TRUE,
            include_hawaii = FALSE
        ),
        format = "file"
    ),
    tar_target(
        gif_tract_annual,
        animate_hms_smoke(
            data = tract_annual_ds,
            level = "tract",
            agg = "annual",
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            out_gif = "figures/tract_annual_hms_smoke.gif",
            include_alaska = TRUE,
            include_hawaii = FALSE
        ),
        format = "file"
    ),
    tar_target(
        gif_tract_monthly,
        animate_hms_smoke(
            data = tract_monthly_ds,
            level = "tract",
            agg = "monthly",
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            out_gif = "figures/tract_monthly_hms_smoke.gif",
            include_alaska = TRUE,
            include_hawaii = FALSE
        ),
        format = "file"
    ),

    tar_target(
        gif_params,
        {
            # per-variable settings (keep AK flags/bboxes as shown)
            var_cfg <- tibble::tibble(
                var = c(
                    "dusmass25",
                    "rmax",
                    "tmin",
                    "annual_total_air_lb_per_km2",
                    "evi"
                ),
                include_alaska = c(TRUE, FALSE, TRUE, TRUE, FALSE),
                include_hawaii = c(FALSE, FALSE, FALSE, FALSE, FALSE),
                bbox_xmin = c(-170, -125, -170, -170, -125),
                bbox_xmax = c(-60, -66, -60, -60, -66),
                bbox_ymin = c(18, 24, 18, 18, 24),
                bbox_ymax = c(72, 50, 72, 72, 50),
                legend_title = list(
                    expression("Dust (µg·m"^-3 * ")"),
                    "Rmax",
                    expression(T[min] * " (°C)"),
                    expression("TRI air emissions per area (lb·km"^-2 * ")"),
                    "EVI"
                ),
                trans = c(
                    "log10",
                    "identity",
                    "identity",
                    "identity",
                    "identity"
                ),
                value_fun = list(
                    function(x) x * 1e9, # dusmass25
                    NULL, # rmax
                    NULL, # tmin
                    NULL, # tri
                    NULL # evi
                )
            )

            combos <- tidyr::crossing(
                var_cfg,
                tibble::tibble(level = c("county", "tract")),
                tibble::tibble(agg = c("annual", "monthly"))
            )

            combos |>
                dplyr::mutate(
                    bbox = Map(c, bbox_xmin, bbox_xmax, bbox_ymin, bbox_ymax),
                    out_path = sprintf("figures/%s_%s_%s.gif", var, level, agg),
                    title = sprintf(
                        "%s %s %s — {current_frame}",
                        tools::toTitleCase(level),
                        agg,
                        var
                    ),
                    palette = "viridis",
                    direction = dplyr::if_else(
                        var == "annual_total_air_lb_per_km2",
                        -1L,
                        1L
                    )
                )
        }
    ),

    # Build all GIFs (each as its own file target)
    tarchetypes::tar_pmap(
        name = gif,
        command = animate_geo_gif(
            var = ..1$var,
            level = ..1$level,
            agg = ..1$agg,
            include_alaska = ..1$include_alaska,
            include_hawaii = ..1$include_hawaii,
            bbox = unlist(..1$bbox),
            legend_title = ..1$legend_title[[1]],
            palette = ..1$palette,
            direction = ..1$direction,
            trans = ..1$trans,
            value_fun = ..1$value_fun[[1]],
            out_path = ds(..1$out_path),
            title = ..1$title
        ),
        values = gif_params,
        format = "file"
    ), # ---- County NLCD ----
    tar_target(
        panel_nlcd_county,
        plot_proportion_panel(
            vars = c("land_cover_24", "land_cover_42", "land_cover_82"),
            pretty_names = c(
                land_cover_24 = "Developed, High Intensity",
                land_cover_42 = "Evergreen Forest",
                land_cover_82 = "Cultivated Crops"
            ),
            high_colors = setNames(
                rep(NA_character_, 3),
                c("land_cover_24", "land_cover_42", "land_cover_82")
            ),
            target_year = 2021,
            outfile = "figures/county_annual_nlcd.png",
            dataset = county_annual,
            level = "county",
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            layer_counties = "counties_500k",
            include_alaska = FALSE,
            include_hawaii = FALSE,
            bbox = c(-125, -66, 24, 50)
        ),
        format = "file"
    ),

    # ---- County Köppen ----
    tar_target(
        panel_koppen_county,
        plot_proportion_panel(
            vars = c("koppen_14", "koppen_7", "koppen_26"),
            pretty_names = c(
                koppen_14 = "Cfa — Temperate, no dry season, hot summer",
                koppen_7 = "BSk — Arid, steppe, cold",
                koppen_26 = "Dfb — Cold, no dry season, warm summer"
            ),
            high_colors = setNames(
                rep(NA_character_, 3),
                c("koppen_14", "koppen_7", "koppen_26")
            ),
            target_year = "static",
            outfile = "figures/county_static_koppen.png",
            dataset = county_annual,
            level = "county",
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            layer_counties = "counties_500k",
            include_alaska = TRUE,
            include_hawaii = FALSE,
            bbox = c(-170, -60, 18, 72)
        ),
        format = "file"
    ),

    # ---- County HUC2 ----
    tar_target(
        panel_huc2_county,
        plot_proportion_panel(
            vars = c(
                "prop_cover_huc2_03",
                "prop_cover_huc2_16",
                "prop_cover_huc2_01"
            ),
            pretty_names = c(
                prop_cover_huc2_03 = "HUC2 03 Proportion",
                prop_cover_huc2_16 = "HUC2 16 Proportion",
                prop_cover_huc2_01 = "HUC2 01 Proportion"
            ),
            high_colors = setNames(
                rep(NA_character_, 3),
                c(
                    "prop_cover_huc2_03",
                    "prop_cover_huc2_16",
                    "prop_cover_huc2_01"
                )
            ),
            target_year = "static",
            outfile = "figures/county_static_huc2_panel.png",
            dataset = county_annual,
            level = "county",
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            layer_counties = "counties_500k",
            include_alaska = TRUE,
            include_hawaii = FALSE,
            bbox = c(-170, -60, 18, 72)
        ),
        format = "file"
    ),

    # ---- Tract NLCD ----
    tar_target(
        panel_nlcd_tract,
        plot_proportion_panel(
            vars = c("land_cover_24", "land_cover_42", "land_cover_82"),
            pretty_names = c(
                land_cover_24 = "Developed, High Intensity",
                land_cover_42 = "Evergreen Forest",
                land_cover_82 = "Cultivated Crops"
            ),
            high_colors = setNames(
                rep(NA_character_, 3),
                c("land_cover_24", "land_cover_42", "land_cover_82")
            ),
            target_year = 2021,
            outfile = "figures/tract_annual_nlcd.png",
            dataset = tract_annual,
            level = "tract",
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            layer_counties = "tracts_500k",
            include_alaska = FALSE,
            include_hawaii = FALSE,
            bbox = c(-125, -66, 24, 50)
        ),
        format = "file"
    ),

    # ---- Tract Köppen ----
    tar_target(
        panel_koppen_tract,
        plot_proportion_panel(
            vars = c("koppen_14", "koppen_7", "koppen_26"),
            pretty_names = c(
                koppen_14 = "Cfa — Temperate, no dry season, hot summer",
                koppen_7 = "BSk — Arid, steppe, cold",
                koppen_26 = "Dfb — Cold, no dry season, warm summer"
            ),
            high_colors = setNames(
                rep(NA_character_, 3),
                c("koppen_14", "koppen_7", "koppen_26")
            ),
            target_year = "static",
            outfile = "figures/tract_static_koppen.png",
            dataset = tract_annual,
            level = "tract",
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            layer_counties = "tracts_500k",
            include_alaska = TRUE,
            include_hawaii = FALSE,
            bbox = c(-170, -60, 18, 72)
        ),
        format = "file"
    ),

    # ---- Tract HUC2 ----
    tar_target(
        panel_huc2_tract,
        plot_proportion_panel(
            vars = c(
                "prop_cover_huc2_03",
                "prop_cover_huc2_16",
                "prop_cover_huc2_01"
            ),
            pretty_names = c(
                prop_cover_huc2_03 = "HUC2 03 Proportion",
                prop_cover_huc2_16 = "HUC2 16 Proportion",
                prop_cover_huc2_01 = "HUC2 01 Proportion"
            ),
            high_colors = setNames(
                rep(NA_character_, 3),
                c(
                    "prop_cover_huc2_03",
                    "prop_cover_huc2_16",
                    "prop_cover_huc2_01"
                )
            ),
            target_year = "static",
            outfile = "figures/tract_static_huc2_panel.png",
            dataset = tract_annual,
            level = "tract",
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            layer_counties = "tracts_500k",
            include_alaska = TRUE,
            include_hawaii = FALSE,
            bbox = c(-170, -60, 18, 72)
        ),
        format = "file"
    ),
    tar_target(
        county_static_mn30_grd_png,
        plot_static_map(
            var = "mn30_grd",
            level = "county",
            include_alaska = TRUE,
            include_hawaii = FALSE,
            legend_title = "Elevation (m)",
            palette = "viridis",
            direction = -1,
            bbox = c(-170, -60, 18, 72),
            title = "County static GMTED Mean Elevation",
            out_file = "figures/county_static_mn30_grd.png",
            dataset = county_annual_ds,
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            trans = "identity"
        ),
        format = "file"
    ),

    # County: Road density (viridis, log10)
    tar_target(
        county_static_roads_png,
        plot_static_map(
            var = "road_density_km_per_km2",
            level = "county",
            include_alaska = TRUE,
            include_hawaii = FALSE,
            legend_title = "Road Density (km/km²)",
            palette = "viridis",
            direction = -1,
            bbox = c(-170, -60, 18, 72),
            title = "County static Road Density (log10)",
            out_file = "figures/county_static_road_density_km_per_km2.png",
            dataset = county_annual_ds,
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            trans = "log10"
        ),
        format = "file"
    ),

    # Tract: GMTED mean elevation (viridis, identity)
    tar_target(
        tract_static_mn30_grd_png,
        plot_static_map(
            var = "mn30_grd",
            level = "tract",
            include_alaska = TRUE,
            include_hawaii = FALSE,
            legend_title = "Elevation (m)",
            palette = "viridis",
            direction = -1,
            bbox = c(-170, -60, 18, 72),
            title = "Tract static GMTED Mean Elevation",
            out_file = "figures/tract_static_mn30_grd.png",
            dataset = tract_annual_ds,
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            trans = "identity"
        ),
        format = "file"
    ),

    # Tract: Road density (viridis, log10)
    tar_target(
        tract_static_roads_png,
        plot_static_map(
            var = "road_density_km_per_km2",
            level = "tract",
            include_alaska = TRUE,
            include_hawaii = FALSE,
            legend_title = "Road Density (km/km²)",
            palette = "viridis",
            direction = -1,
            bbox = c(-170, -60, 18, 72),
            title = "Tract static Road Density (log10)",
            out_file = "figures/tract_static_road_density_km_per_km2.png",
            dataset = tract_annual_ds,
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            trans = "log10"
        ),
        format = "file"
    ),

    # County: PRISM SolTotal normals (log10)
    tar_target(
        county_normal_soltotal_png,
        plot_normal_map(
            var = "soltotal",
            level = "county",
            include_alaska = FALSE,
            include_hawaii = FALSE,
            legend_title = "Solar Radiation (MJ/m²/day)",
            palette = "viridis",
            limits = c(0.001, 20),
            title = "County normal PRISM SolTotal",
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            dataset = county_annual,
            trans = "identity" # linear scale
        ),
        format = "file"
    ),

    # Tract: PRISM SolTotal normals (log10)
    tar_target(
        tract_normal_soltotal_png,
        plot_normal_map(
            var = "soltotal",
            level = "tract",
            include_alaska = FALSE,
            include_hawaii = FALSE,
            legend_title = "Solar Radiation (MJ/m²/day)",
            palette = "viridis",
            bbox = c(-140, -60, 18, 72),
            limits = c(0.001, 20),
            title = "Tract normal PRISM SolTotal",
            geoms_gpkg = "clean_data/county_census/canonical_2024.gpkg",
            dataset = tract_annual,
            trans = "identity" # linear scale
        ),
        format = "file"
    )
)
