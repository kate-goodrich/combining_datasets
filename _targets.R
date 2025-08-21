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
                    "sur_refl_qc500m",
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
        huc_county_summary,
        huc_overlap_summary(
            gpkg_dir = "clean_data/huc_clean",
            zones_gpkg = CENSUS_GPKG,
            level = "county",
            write_csv = "summary_sets/static_county_huc.csv"
        )
    ),
    tar_target(
        huc_tract_summary,
        huc_overlap_summary(
            gpkg_dir = "clean_data/huc_clean",
            zones_gpkg = CENSUS_GPKG,
            level = "tract",
            zone_layer = LAYER$tract, # set if your gpkg uses this layer name
            write_csv = "summary_sets/static_tract_huc.csv"
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

    # ---- NLCD ----
    tar_target(
        nlcd_county_annual,
        zonal_means_from_tifs(
            input_dir = "clean_data/nlcd_clean",
            zones_gpkg = CENSUS_GPKG,
            level = "county",
            agg = "annual",
            id_col = "geoid",
            file_pattern = "_processed\\.tif$",
            write_csv = "summary_sets/annual_county_nlcd.csv"
        )
    ),
    tar_target(
        nlcd_county_monthly,
        zonal_means_from_tifs(
            input_dir = "clean_data/nlcd_clean",
            zones_gpkg = CENSUS_GPKG,
            level = "county",
            agg = "monthly",
            id_col = "geoid",
            file_pattern = "_processed\\.tif$",
            write_csv = "summary_sets/monthly_county_nlcd.csv"
        )
    ),
    tar_target(
        nlcd_tract_annual,
        zonal_means_from_tifs(
            input_dir = "clean_data/nlcd_clean",
            zones_gpkg = CENSUS_GPKG,
            level = "tract",
            agg = "annual",
            id_col = "geoid",
            file_pattern = "_processed\\.tif$",
            write_csv = "summary_sets/annual_tract_nlcd.csv"
        )
    ),
    tar_target(
        nlcd_tract_monthly,
        zonal_means_from_tifs(
            input_dir = "clean_data/nlcd_clean",
            zones_gpkg = CENSUS_GPKG,
            level = "tract",
            agg = "monthly",
            id_col = "geoid",
            file_pattern = "_processed\\.tif$",
            write_csv = "summary_sets/monthly_tract_nlcd.csv"
        )
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

    # 1) Long-form builders (produce CSV + Parquet AND return tibble)
    tar_target(
        county_annual_long,
        build_exposure_long(
            agg = "annual",
            level = "county",
            write_csv = "handoffs/county_annual_long/county_annual.csv",
            write_parquet = "handoffs/county_annual_long/county_annual.parquet"
        )
    ),
    tar_target(
        county_monthly_long,
        build_exposure_long(
            agg = "monthly",
            level = "county",
            write_csv = "handoffs/county_monthly_long/county_monthly.csv",
            write_parquet = "handoffs/county_monthly_long/county_monthly.parquet"
        )
    ),

    # 2) Wide-layer exporters (return file paths, so mark as files)
    tar_target(
        county_annual_layers,
        build_exposure_wide_layers(
            data = county_annual_long,
            agg = "annual",
            level = "county",
            output_dir = "handoffs/county_annual_wide"
        ),
        format = "file"
    ),
    tar_target(
        county_monthly_layers,
        build_exposure_wide_layers(
            data = county_monthly_long,
            agg = "monthly",
            level = "county",
            output_dir = "handoffs/county_monthly_wide"
        ),
        format = "file"
    ),

    # Repeat for tract
    tar_target(
        tract_annual_long,
        build_exposure_long(
            agg = "annual",
            level = "tract",
            write_csv = "handoffs/tract_annual_long/tract_annual.csv",
            write_parquet = "handoffs/tract_annual_long/tract_annual.parquet"
        )
    ),
    tar_target(
        tract_monthly_long,
        build_exposure_long(
            agg = "monthly",
            level = "tract",
            write_csv = "handoffs/tract_monthly_long/tract_monthly.csv",
            write_parquet = "handoffs/tract_monthly_long/tract_monthly.parquet"
        )
    ),
    tar_target(
        tract_annual_layers,
        build_exposure_wide_layers(
            data = tract_annual_long,
            agg = "annual",
            level = "tract",
            output_dir = "handoffs/tract_annual_wide"
        ),
        format = "file"
    ),
    tar_target(
        tract_monthly_layers,
        build_exposure_wide_layers(
            data = tract_monthly_long,
            agg = "monthly",
            level = "tract",
            output_dir = "handoffs/tract_monthly_wide"
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
                zipfile = "handoffs.zip"
            )
        },
        format = "file" # so targets tracks the zip
    )
)
