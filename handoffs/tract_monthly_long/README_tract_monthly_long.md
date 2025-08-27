# Dataset: Long-Form Census Tract Monthly Environmental Health Dataset

**File Name:** `tract_monthly.parquet` 

**Last Updated:** 2025-08-29 

**Format:** Apache Parquet

## Description

This dataset includes monthly summaries of environmental and climate health variables for all U.S. census tracts from 2010 to 2024.

## Usage

This dataset is designed to support geospatial epidemiological analyses and environmental exposure mapping by providing relevant covariates at the tract-month level.


## Structure

Each row represents a single environmental or climate variable for a given census tract and year.

| Column Name      | Type     | Description                                           |
|------------------|----------|-------------------------------------------------------|
| `geoid`          | string   | GEOID for census tract                                |
| `variable`       | string   | Name of the environmental variable                    |
| `value`          | numeric  | Area-weighted mean value                              |
| `year`           | integer  | Year                                                  |
| `month`          | integer  | Month                                                 |

## Source

This file was generated using data from:

- Climatology Lab TerraClimate (abbreviated to "terraclimate")
- Climatology Lab GridMet (abbreviated to "gridmet")
- Koppen-Geiger Climate Classification (abbreviated to "koppen_geiger")
- MRLC1 Consortium National Land Cover Database (abbreviated to "nlcd")
- NASA2 Moderate Resolution Imaging Spectroradiometer (abbriviated to "modis")
- NASA Modern-Era Retrospective analysis for Research and Applications, Version 2 (abbreviated to "merra2)
- NASA SEDAC Global Roads Open Access Data Set (abbriveated to "groads")
- NOAA Hazard Mapping System Fire and Smoke Product (abbreviated to "hms")
- Parameter Elevation Regression on Independent Slopes Model (abbreviated to "prism")
- US EPA Toxic Release Inventory (TRI) Program (abbreviated to "tri")
- USGS6 Global Multi-resolution Terrain Elevation Data (GMTED2010) (abbreviated to "gmted")
- USGS National Hydrography Dataset (NHD) (abbreviated to "huc")
- Cartographic Boundary GeoPackage (abbreviated to "boundaries")


## Processing Notes

- All datasets were downloaded and processed using `amadeus` in R. 
- Tract and county geometries are from `canonical_2024.gpkg`.
- Area-weighted means were calculated using `exactextractr` in R, applied to county and tract geometries.
- Coverage: 2010–2024 for all dynamic datasets, except merra2 (2010 excluded due to metadata inconsistencies), and nlcd (2024 not yet available).
- MODIS scale factors (e.g., reflectance ×0.0001, EVI ×0.0001) were applied prior to aggregation
- TRI point source emissions were summed by tract; additional per-area and +4 km buffer summaries included.
- Monthly PRISM normals represent 1991–2020 climatological averages (static).
- NCD and TRI data are taken at the annual level but repeated monthly for harmonization with other datasets.
- Geographic coverage spans the 48 contiguous U.S. states. Alaska and Hawaii are excluded from gridMET, NLCD, PRISM, and MODIS here.


## Covariate Descriptions

| Variable                       | Source        | Type    | Description                                           | Units          |
|--------------------------------|---------------|---------|-------------------------------------------------------|----------------|
| etr                            | gridmet       | dynamic | Reference alfalfa evaportranspiration                 | mm/day         |
| pr                             | gridmet       | dynamic | Precipitation                                         | mm/day         |
| rmax                           | gridmet       | dynamic | Maximum Near-Surface Relative Humidity                | %              |
| rmin                           | gridmet       | dynamic | Minimum Near-Surface Relative Humidity                | %              |
| sph                            | gridmet       | dynamic | Near-Surface Specific Humidity                        | kg/kg          |
| srad                           | gridmet       | dynamic | Surface Downwelling Solar Radiation                   | W/m²           |
| th                             | gridmet       | dynamic | Wind direction at 10 m                                | degrees        |
| tmmn                           | gridmet       | dynamic | Daily minimum Near-Surface Air Temperature            | K              |
| tmmx                           | gridmet       | dynamic | Daily maximum Near-Surface Air Temperature            | K              |
| vs                             | gridmet       | dynamic | Wind speed at 10 m                                    | m/s            |
| prop_light_coverage            | hms           | dynamic | Proportion of light wilfire smoke cover               | proportion     |
| prop_med_coverage              | hms           | dynamic | Proportion of medium wilfire smoke cover              | proportion     |
| prop_heavy_coverage            | hms           | dynamic | Proportion of heavy wilfire smoke cover               | proportion     |
| albedo                         | merra2        | dynamic | Surface albedo (shortwave reflectance)                | unitless       |
| bcsmass                        | merra2        | dynamic | Black carbon surface mass concentration               | kg m⁻³         |
| cldtot                         | merra2        | dynamic | Total cloud fraction                                  | fraction       |
| dusmass25                      | merra2        | dynamic | Dust surface mass concentration, particles <2.5 μm    | kg m⁻³         |
| evap                           | merra2        | dynamic | Evaporation from turbulence                           | kg m⁻² s⁻¹     |
| grn                            | merra2        | dynamic | Greenness index (fraction of green vegetation)        | proportion     |
| gwetroot                       | merra2        | dynamic | Root zone soil wetness                                | proportion     |
| lai                            | merra2        | dynamic | Leaf area index                                       | dimensionless  |
| lwgab                          | merra2        | dynamic | Surface absorbed longwave radiation                   | W m⁻²          |
| pblh                           | merra2        | dynamic | Planetary boundary layer height                       | m              |
| precsno                        | merra2        | dynamic | Snowfall precipitation                                | kg m⁻² s⁻¹     |
| prectotcorr                    | merra2        | dynamic | Bias corrected total precipitation                    | kg m⁻² s⁻¹     |
| ps                             | merra2        | dynamic | Surface pressure                                      | Pa             |
| qv2m                           | merra2        | dynamic | Specific humidity at 2 m                              | kg/kg          |
| slp                            | merra2        | dynamic | Sea level pressure                                    | Pa             |
| t2mdew                         | merra2        | dynamic | Dew point temperature at 2 m                          | K              |
| totexttau                      | merra2        | dynamic | Total aerosol optical thickness                       | unitless       |
| ts                             | merra2        | dynamic | Surface skin temperature                              | K              |
| u10m                           | merra2        | dynamic | Zonal wind speed at 10 m (east-west)                  | m/s            |
| z0m                            | merra2        | dynamic | Surface roughness length                              | m              |
| aet                            | terraclimate  | dynamic | Actual evapotranspiration                             | mm/month       |
| def                            | terraclimate  | dynamic | Climatic water deficit                                | mm/month       |
| pdsi                           | terraclimate  | dynamic | Palmer drought severity index                         | index          |
| ppt                            | terraclimate  | dynamic | Precipitation                                         | mm/month       |
| soil                           | terraclimate  | dynamic | Soil moisture                                         | mm/month       |
| swe                            | terraclimate  | dynamic | Snow water equivalent                                 | mm/month       |
| tmax                           | terraclimate  | dynamic | Maximum temperature                                   | °C             |
| tmin                           | terraclimate  | dynamic | Minimum temperature                                   | °C             |
| vap                            | terraclimate  | dynamic | Vapor pressure                                        | kPa            |
| vpd                            | terraclimate  | dynamic | Vapor pressure deficit                                | kPa            |
| ws                             | terraclimate  | dynamic | Wind speed                                            | m/s            |
| pet                            | terraclimate  | dynamic | Monthly potential evapotranspiration                  | mm/month       |
| total_air_lb                   | tri           | dynamic | Total air emissions (fugitive and stack)              | lb             |
| total_air_lb_per_km2           | tri           | dynamic | Total air emissions per area                          | lb/km²         |
| total_air_lb_plus4km           | tri           | dynamic | Total air emissions plus 4 km buffer                  | lb             |
| total_fugitive_air_lb          | tri           | dynamic | Fugitive air emissions                                | lb             |
| total_fugitive_air_lb_per_km2  | tri           | dynamic | Fugitive air emissions per area                       | lb/km²         |
| total_fugitive_air_lb_plus4km  | tri           | dynamic | Fugitive air emissions plus 4 km buffer               | lb             |
| total_stack_air_lb             | tri           | dynamic | Stack (point-source) air emissions                    | lb             |
| total_stack_air_lb_per_km2     | tri           | dynamic | Stack emissions per area                              | lb/km²         |
| total_stack_air_lb_plus4km     | tri           | dynamic | Stack emissions plus 4 km buffer                      | lb             |
| solclear                       | prism         | normal  | Solar radiation under clear sky                       | MJ/m²/day      |
| solslope                       | prism         | normal  | Solar shortwave radiation sloped surface              | MJ/m²/day      |
| soltotal                       | prism         | normal  | Solar shortwave radiation horizontal surface          | MJ/m²/day      |
| soltrans                       | prism         | normal  | Atmospheric transmittance                             | fraction       |
| tdmean                         | prism         | normal  | Mean daily dew point temperature                      | °C             |
| tmean                          | prism         | normal  | Mean temperature                                      | °C             |
| vpdmax                         | prism         | normal  | Maximum vapor pressure deficit                        | hPa            |
| vpdmin                         | prism         | normal  | Minimum vapor pressure deficit                        | hPa            |
| be30_grd                       | gmted         | static  | Breakline emphasis (30-arc seconds)                   | m              |
| ds30_grd                       | gmted         | static  | Systematic subsample (30-arc seconds)                 | m              |
| md30_grd                       | gmted         | static  | Median statistic (30-arc seconds)                     | m              |
| mi30_grd                       | gmted         | static  | Minimum statistic (30-arc seconds)                    | m              |
| mn30_grd                       | gmted         | static  | Mean statistic (30-arc seconds)                       | m              |
| mx30_grd                       | gmted         | static  | Maximum statistic (30-arc seconds)                    | m              |
| sd30_grd                       | gmted         | static  | Standard deviation (30-arc seconds)                   | m              |
| area_km2                       | boundaries    | static  | Area of county or census tract                        | km²            |
| road_density_km_per_km2        | groads        | static  | Road density                                          | km/km²         |
| total_road_km                  | groads        | static  | Total length of roads                                 | km             |
| prop_cover_burnaddwaterbody    | huc           | static  | Proportion of zone covered by BurnAdd waterbody       | proportion     |
| prop_cover_catchment           | huc           | static  | Proportion of zone covered by catchment               | proportion     |
| prop_cover_catchmentsp         | huc           | static  | Proportion of zone covered by small catchments        | proportion     |
| prop_cover_huc12               | huc           | static  | Proportion of zone covered by HUC12                   | proportion     |
| prop_cover_landsea             | huc           | static  | Proportion of zone covered by land-sea boundary       | proportion     |
| prop_cover_nhdarea             | huc           | static  | Proportion of zone covered by NHD area                | proportion     |
| prop_cover_nhdwaterbody        | huc           | static  | Proportion of zone covered by NHD waterbody           | proportion     |
| koppen_confidence              | koppen_geiger | static  | Confidence metric for Koppen_Geiger zone coverage     | %              |
| koppen_1                       | koppen_geiger | static  | Coverage Af – Tropical, rainforest                    | proportion     |
| koppen_2                       | koppen_geiger | static  | Coverage Am – Tropical, monsoon                       | proportion     |
| koppen_3                       | koppen_geiger | static  | Coverage Aw – Tropical, savannah                      | proportion     |
| koppen_4                       | koppen_geiger | static  | Coverage BWh – Arid, desert, hot                      | proportion     |
| koppen_5                       | koppen_geiger | static  | Coverage BWk – Arid, desert, cold                     | proportion     |
| koppen_6                       | koppen_geiger | static  | Coverage BSh – Arid, steppe, hot                      | proportion     |
| koppen_7                       | koppen_geiger | static  | Coverage BSk – Arid, steppe, cold                     | proportion     |
| koppen_8                       | koppen_geiger | static  | Coverage Csa – Temperate, dry summer, hot summer      | proportion     |
| koppen_9                       | koppen_geiger | static  | Coverage Csb – Temperate, dry summer, warm summer     | proportion     |
| koppen_10                      | koppen_geiger | static  | Coverage Csc – Temperate, dry summer, cold summer     | proportion     |
| koppen_11                      | koppen_geiger | static  | Coverage Cwa – Temperate, dry winter, hot summer      | proportion     |
| koppen_12                      | koppen_geiger | static  | Coverage Cwb – Temperate, dry winter, warm summer     | proportion     |
| koppen_13                      | koppen_geiger | static  | Coverage Cwc – Temperate, dry winter, cold summer     | proportion     |
| koppen_14                      | koppen_geiger | static  | Coverage Cfa – Temperate, no dry season, hot summer   | proportion     |
| koppen_15                      | koppen_geiger | static  | Coverage Cfb – Temperate, no dry season, warm summer  | proportion     |
| koppen_16                      | koppen_geiger | static  | Coverage Cfc – Temperate, no dry season, cold summer  | proportion     |
| koppen_17                      | koppen_geiger | static  | Coverage Dsa – Cold, dry summer, hot summer           | proportion     |
| koppen_18                      | koppen_geiger | static  | Coverage Dsb – Cold, dry summer, warm summer          | proportion     |
| koppen_19                      | koppen_geiger | static  | Coverage Dsc – Cold, dry summer, cold summer          | proportion     |
| koppen_20                      | koppen_geiger | static  | Coverage Dsd – Cold, dry summer, very cold winter     | proportion     |
| koppen_21                      | koppen_geiger | static  | Coverage Dwa – Cold, dry winter, hot summer           | proportion     |
| koppen_22                      | koppen_geiger | static  | Coverage Dwb – Cold, dry winter, warm summer          | proportion     |
| koppen_23                      | koppen_geiger | static  | Coverage Dwc – Cold, dry winter, cold summer          | proportion     |
| koppen_24                      | koppen_geiger | static  | Coverage Dwd – Cold, dry winter, very cold winter     | proportion     |
| koppen_25                      | koppen_geiger | static  | Coverage Dfa – Cold, no dry season, hot summer        | proportion     |
| koppen_26                      | koppen_geiger | static  | Coverage Dfb – Cold, no dry season, warm summer       | proportion     |
| koppen_27                      | koppen_geiger | static  | Coverage Dfc – Cold, no dry season, cold summer       | proportion     |
| koppen_28                      | koppen_geiger | static  | Coverage Dfd – Cold, no dry season, very cold winter  | proportion     |
| koppen_29                      | koppen_geiger | static  | Coverage ET – Polar, tundra                           | proportion     |
| koppen_30                      | koppen_geiger | static  | Coverage EF – Polar, frost                            | proportion     |
| sur_refl_b01                   | modis         | dynamic | Surface reflectance for band 1 (scale factor 0.0001)  | unitless       |
| sur_refl_b02                   | modis         | dynamic | Surface reflectance for band 2 (scale factor 0.0001)  | unitless       |
| sur_refl_b03                   | modis         | dynamic | Surface reflectance for band 3 (scale factor 0.0001)  | unitless       |
| sur_refl_b04                   | modis         | dynamic | Surface reflectance for band 4 (scale factor 0.0001)  | unitless       |
| sur_refl_b05                   | modis         | dynamic | Surface reflectance for band 5 (scale factor 0.0001)  | unitless       |
| sur_refl_b06                   | modis         | dynamic | Surface reflectance for band 6 (scale factor 0.0001)  | unitless       |
| sur_refl_b07                   | modis         | dynamic | Surface reflectance for band 7 (scale factor 0.0001)  | unitless       |
| lST_day_1km                    | modis         | dynamic | Daytime land surface temperature                      | K              |
| lST_night_1km                  | modis         | dynamic | Nighttime land surface temperature                    | K              |
| ndvi                           | modis         | dynamic | Normalized Difference Vegetation Index                | index (–1 to 1)|
| evi                            | modis         | dynamic | Enhanced Vegetation Index (scale factor 0.0001)       | index (–1 to 1)|
| fractional_impervious_surface  | nlcd          | dynamic | Proportion covered by inpervious sufaces              | %              |
| land_cover_confidence          | nlcd          | dynamic | Confidence score indicating model certainty           | %              |
| land_cover_11                  | nlcd          | dynamic | Open Water                                            | proportion     |
| land_cover_12                  | nlcd          | dynamic | Perennial Ice/Snow                                    | proportion     |
| land_cover_21                  | nlcd          | dynamic | Developed, Open Space                                 | proportion     |
| land_cover_22                  | nlcd          | dynamic | Developed, Low Intensity                              | proportion     |
| land_cover_23                  | nlcd          | dynamic | Developed, Medium Intensity                           | proportion     |
| land_cover_24                  | nlcd          | dynamic | Developed, High Intensity                             | proportion     |
| land_cover_31                  | nlcd          | dynamic | Barren Land (Rock/Sand/Clay)                          | proportion     |
| land_cover_41                  | nlcd          | dynamic | Deciduous Forest                                      | proportion     |
| land_cover_42                  | nlcd          | dynamic | Evergreen Forest                                      | proportion     |
| land_cover_43                  | nlcd          | dynamic | Mixed Forest                                          | proportion     |
| land_cover_52                  | nlcd          | dynamic | Shrub/Scrub                                           | proportion     |
| land_cover_71                  | nlcd          | dynamic | Grasslands/Herbaceous                                 | proportion     |
| land_cover_81                  | nlcd          | dynamic | Pasture/Hay                                           | proportion     |
| land_cover_82                  | nlcd          | dynamic | Cultivated Crops                                      | proportion     |
| land_cover_90                  | nlcd          | dynamic | Woody Wetlands                                        | proportion     |
| land_cover_95                  | nlcd          | dynamic | Emergent Herbaceous Wetlands                          | proportion     |



