Loading Raw Data from Amadeus
================

`{r}# Load necessary libraries library(amadeus) library(dplyr) library(tidyr) library(archive) library(languageserver)`

\`\`\`{r}# Load Zip Files

\#If there is a zip step, i just load the zip files for now since I’m,
having issues with some functions unzipping. Will mannually unzip next

# Load Climatology Lab TerraClimate AKA terraclimate

\# DOWNLOADED

variables \<- c( “aet”, “def”, “swe”, “q”, “soil”, “PDSI”, “pet”, “ppt”,
“srad”, “tmax”, “tmin”, “vap”, “vpd”, “ws” )

for (var in variables) { download_terraclimate( variables = var, year =
c(2010, 2024), directory_to_save =
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/terraclimate”,
acknowledgement = TRUE, download = TRUE, remove_command = FALSE, hash =
FALSE ) }

# Load Climatology Lab GridMet AKA gridmet

# 404 not found for all years

variables \<- c( “sph”, “pr”, “rmin”, “rmax”, “srad”, “tmmn”, “tmmx”,
“vs”, “th”, “pdsi”, “pet”, “etr”, “ERC”, “BI”, “FM100”, “FM1000” )

years \<- c(“2010”, “2011”, “2012”, “2013”, “2014”, “2015”, “2016”,
“2017”, “2018”, “2019”, “2020”, “2021”, “2022”, “2023”, “2024”)

for (year in years) { for (var in variables) { tryCatch({
download_gridmet( variables = var, year = year, directory_to_save =
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/gridmet”,
acknowledgement = TRUE, download = TRUE, remove_command = FALSE, hash =
FALSE ) }, error = function(e) { message(paste(“Error for”, var, “in”,
year, “:”, e\$message)) }) } }

# Thuis one is not working - 404 not found

# Load Koppen-Geiger Climate Classification AKA koppen_geiger

## Weird time period - ask Kyle

\# Download MRLC1 Consortium National Land Cover Database (NLCD) AKA
nlcd \# DOWNLOADING ZIP FILES

\#commenting this out for now bc when i trie to redo the function it
overwrote the files. re- insert after nlcd runs \### products \<- c(
\### “Land Cover”, \### “Land Cover Change”, \### “Land Cover
Confidence”, \### “Fractional Impervious Surface”, \### “Impervious
Descriptor”, \### “Spectral Change Day of Year” \### )

products \<- c(“Land Cover”)

base_dir \<-
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/nlcd”

for (prod in products) { prod_dir \<- file.path(base_dir, gsub(” “,”\_“,
prod)) if (!dir.exists(prod_dir)) dir.create(prod_dir)

for (yr in 2010:2023) { download_nlcd( product = prod, year = yr,
directory_to_save = prod_dir, acknowledgement = TRUE, download = TRUE,
remove_command = FALSE, unzip = TRUE, remove_zip = FALSE, hash = FALSE )
} }

\# Download NASA2 Moderate Resolution Imaging Spectroradiometer (MODIS)
AKA modis \# NOT YET DOWNLOADED - TRY LATER

\# DO NOT RESTART IF IT ONLY GOES PART THROUGH - IT WILL OVERWRITE THE
FILES AND YOU WILL HAVE TO START OVER

years \<- list( c(“2010-01-01”, “2010-12-31”), c(“2011-01-01”,
“2011-12-31”), c(“2012-01-01”, “2012-12-31”), c(“2013-01-01”,
“2013-12-31”), c(“2014-01-01”, “2014-12-31”), c(“2015-01-01”,
“2015-12-31”), c(“2016-01-01”, “2016-12-31”), c(“2017-01-01”,
“2017-12-31”), c(“2018-01-01”, “2018-12-31”), c(“2019-01-01”,
“2019-12-31”), c(“2020-01-01”, “2020-12-31”), c(“2021-01-01”,
“2021-12-31”), c(“2022-01-01”, “2022-12-31”), c(“2023-01-01”,
“2023-12-31”), c(“2024-01-01”, “2024-12-31”) )

products \<- c(“MOD09A1”, “MYD09A1”, “MOD09GQ”, “MOD11A2”, “MYD11A2”,
“MOD13A3”, “MYD13A3”, “VNP13A2”)

nasa_token \<-
“eyJ0eXAiOiJKV1QiLCJvcmlnaW4iOiJFYXJ0aGRhdGEgTG9naW4iLCJzaWciOiJlZGxqd3RwdWJrZXlfb3BzIiwiYWxnIjoiUlMyNTYifQ.eyJ0eXBlIjoiVXNlciIsInVpZCI6InBvZ3Vla2wiLCJleHAiOjE3NTkwNjc2NTAsImlhdCI6MTc1Mzg4MzY1MCwiaXNzIjoiaHR0cHM6Ly91cnMuZWFydGhkYXRhLm5hc2EuZ292IiwiaWRlbnRpdHlfcHJvdmlkZXIiOiJlZGxfb3BzIiwiYWNyIjoiZWRsIiwiYXNzdXJhbmNlX2xldmVsIjozfQ.OhGSopQgCj-A4dEYlxpCdaQ6EMTNq3EOLEQgN0twuPq5Ds2RFyEjyjvK6wrgjIhlure8VQV6s9VueHG8JHTBu9_JZBdZQt7yJcG76PTVnulP79HWZMpkx5zjy8D54LBrNoRmbFw_EyihOfeJOLOzcCZF1OzAiRqTQtOV_MO7UHVNUDXxEHugM0RK0oTbLVnxRTuwwpL3rHbeHxsa-BmZ3VqRGqdylWKmt4Av8f5ln9j112uSBLwy0ObanmZf43rjzokIbNyGB4HzSUcXAWZ8taKJm2y_AB53NGPYtA7zB2Kqr0TIDGTBeKuG6tXFp0BrgoCyZgreCNx5CdNvvC7Fxg”

for (product in products) { \# create a subfolder for each product
product_dir \<-
file.path(“/ddn/gs1/group/set/chords/combining_datasets/raw_data/modis”,
product) dir.create(product_dir, recursive = TRUE, showWarnings = FALSE)

for (year_range in years) { tryCatch({ download_modis( product =
product, version = “61”, horizontal_tiles = c(7, 13), vertical_tiles =
c(3, 6), mod06_links = NULL, nasa_earth_data_token = nasa_token, date =
year_range, \# each year is a start/end vector like c(“2010-01-01”,
“2010-12-31”) directory_to_save = product_dir, \# save into the
product-specific folder acknowledgement = TRUE, download = TRUE,
remove_command = FALSE, hash = FALSE ) }, error = function(e) {
message(paste(“Failed for”, product, “in”, paste(year_range, collapse =
” - “),”:“, e\$message)) }) } }

\# Download NASA Modern-Era Retrospective analysis for Research and
Applications, Version 2 (MERRA-2) AKA merra2 \# STILL LOADING - having
some connectivity issues

products \<- c( “statD_2d_slv_Nx”, “tavg1_2d_adg_Nx”, “tavg1_2d_aer_Nx”,
“tavg1_2d_chm_Nx”, “tavg1_2d_csp_Nx”, “tavg1_2d_flx_Nx”,
“tavg1_2d_lfo_Nx”, “tavg1_2d_lnd_Nx”, “tavg1_2d_rad_Nx”,
“tavg1_2d_slv_Nx” )

for (prod in products) { download_merra2( collection = prod, date =
c(“2010-01-01”, “2024-12-31”), directory_to_save =
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/merra2”,
acknowledgement = TRUE, download = TRUE, remove_command = FALSE, hash =
FALSE ) }

# Download NASA SEDAC3 UN WPP-Adjusted Population Density AKA population

\# “
<p>
The requested URL was not found on this server.
</p>

”  
\# zip files load but then say ther file was not found

years \<- c(“2010”, “2015”, “2020”)

for (year in years) { download_population( data_resolution = “60
minute”, data_format = “netCDF”, year = year, directory_to_save =
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/zip_files”,
acknowledgement = TRUE, download = TRUE, remove_command = FALSE, unzip =
FALSE, remove_zip = FALSE, hash = FALSE ) }

# Download NASA SEDAC Global Roads Open Access Data Set AKA groads

# DOWNLOADED

download_groads( data_region = c(“Americas”), data_format =
c(“Shapefile”), directory_to_save =
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/zip_files”,
acknowledgement = TRUE, download = TRUE, remove_command = FALSE, unzip =
FALSE, remove_zip = FALSE, hash = FALSE )

# Unzip groads file

unzip(“/ddn/gs1/group/set/chords/combining_datasets/raw_data/zip_files/zip_files/groads_v1_americas_shp.zip”,
exdir = “/ddn/gs1/group/set/chords/combining_datasets/raw_data/groads”)

# Download NASA Goddard Earth Observing System Composition Forcasting (GEOS-CF) AKA geos

# 404 not found - also looks like this is all atmospheric data, not land surface data

products \<- c(“aqc_tavg_1hr_g1440x721_v1”, “chm_tavg_1hr_g1440x721_v1”,
“met_tavg_1hr_g1440x721_x1”, “xgc_tavg_1hr_g1440x721_x1”,
“chm_inst_1hr_g1440x721_p23”, “met_inst_1hr_g1440x721_p23”)

    for (prod in products) {
      download_geos(
        collection = prod,
        date = c("2010-01-01", "2024-12-31"),
        directory_to_save = "/ddn/gs1/group/set/chords/combining_datasets/raw_data",
        acknowledgement = TRUE,
        download = TRUE,
        remove_command = FALSE,
        hash = FALSE
      )
    }

# Download NOAA Hazard Mapping System Fire and Smoke Product AKA hms

# DOWNLOADED

download_hms( data_format = “Shapefile”, date = c(“2010-01-01”,
“2024-12-31”), directory_to_save =
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/hms”,
acknowledgement = TRUE, download = TRUE, remove_command = FALSE, unzip =
TRUE, remove_zip = TRUE, hash = FALSE )

# Loading NOAA NCEP4 North American Regional Reanalysis (NARR) AKA narr

# “selected variable is not available” for all surface variables

variables \<- c( “VIS”, “HPBL”, “PRES”, “PRESN”, “TMP”, “POT”, “CNWAT”,
“WEASD”, “SNOWC”, “SNOD”, “APCP”, “APCPN”, “ACPCP”, “DSWRF”, “DLWRF”,
“SHTFL”, “LHTFL”, “GFLUX”, “SNOHF”, “EVP”, “PEVAP”, “SFEXC”, “CD”,
“FRICV”, “VEG”, “CCOND”, “RCS”, “RCT”, “RCQ”, “RCSOL”, “CAPE”, “CIN”,
“ALBDO”, “UFLX”, “VFLX”, “SOILW”, “SOILL”, “TSOIL”, “MSTAV”, “SOILM”,
“VIS”,“LCDC”,“TCDC”,“SFCR”,“VGTYP”,“SOTYP”,“SLTYP”,“RSMIN”,“RLYRS”,“WILT”,“SMREF”,“NLAT”,“ELON”,“LAND”,“SNFALB”,“MXSALB”
)

for (var in variables) { tryCatch({ message(paste0(“▶ Starting download
for:”, var))

    download_narr(
      variables = var,
      year = c(2010, 2024),
      directory_to_save = "/ddn/gs1/group/set/chords/combining_datasets/raw_data/narr",
      acknowledgement = TRUE,
      download = TRUE,
      remove_command = FALSE,
      hash = FALSE
    )

    message(paste0("Finished: ", var))

}, error = function(e) { message(paste0(“Error for”, var, “:”,
e\$message)) }) }

# Download OpenGeoHub Foundation OpenLandMap aka ?? is there a function here? ask Kyle

# Download Parameter Elevation Regression on Independent Slopes Model (PRISM) AKA prism

# DOWNLOADED SOL ONLY tried to download more that 2x - it got mad. try thurs

elements1 \<- c(“ppt”, “tmin”, “tmax”, “tmean”, “tdmean”, “vpdmin”,
“vpdmax”)

elements_sol \<- c(“solslope”, “soltotal”, “solclear”, “soltrans”)

for (element in elements1) { download_prism( time = “201001”,  
element = element, data_type = c(“ts”, “normals_800”, “normals”), format
= c(“nc”, “asc”, “grib2”), directory_to_save =
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/prism”,
acknowledgement = TRUE, download = TRUE, remove_command = FALSE, hash =
FALSE ) }

for (element in elements_sol) { download_prism( time = “01”,  
element = element, data_type = c(“normals”), format = c(“nc”, “asc”,
“grib2”), directory_to_save =
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/prism”,
acknowledgement = TRUE, download = TRUE, remove_command = FALSE, hash =
FALSE ) }

\# unzip prism files

zips \<-
list.files(“/ddn/gs1/group/set/chords/combining_datasets/raw_data/prism”,
pattern = “\\.zip\$”, full.names = TRUE)
dir.create(“/ddn/gs1/group/set/chords/combining_datasets/raw_data/prism/data_files”,
showWarnings = FALSE, recursive = TRUE) for (f in zips) unzip(f, exdir =
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/prism/data_files”)

# Download US EPA5 Air Data Pre-Generated Data Files

# Don’t do this one - bad data

# Download US EPA Ecoregions AKA ecoregions

# 404 not found.

download_ecoregion( epa_certificate_path =
system.file(“extdata/cacert_gaftp_epa.pem”, package = “amadeus”),
certificate_url =
“<http://cacerts.digicert.com/DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt>”,
directory_to_save =
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/ecoregions”,
acknowledgement = TRUE, download = TRUE, remove_command = FALSE, unzip =
FALSE, remove_zip = FALSE, hash = FALSE )

# Download US EPA National Emissions Inventory (NEI) AKA nei

# 404 not found - data only available for 2017 and 2020

download_nei( epa_certificate_path =
system.file(“extdata/cacert_gaftp_epa.pem”, package = “amadeus”),
certificate_url =
“<http://cacerts.digicert.com/DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt>”,
year = c(2010L, 2024L), directory_to_save =
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/nei”,
acknowledgement = TRUE, download = TRUE, remove_command = FALSE, unzip =
TRUE, hash = FALSE )

# Download US EPA Toxic Release Inventory (TRI) Program AKA tri

# DOWNLOADED

download_tri( year = c(2010L, 2024L), directory_to_save =
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/tri”,
acknowledgement = TRUE, download = TRUE, remove_command = FALSE, hash =
FALSE )

# Download USGS6 Global Multi-resolution Terrain Elevation Data (GMTED2010) AKA gmted

# DOWNLOADED

statistic \<- c(“Breakline Emphasis”, “Systematic Subsample”, “Median
Statistic”, “Minimum Statistic”, “Mean Statistic”, “Maximum Statistic”,
“Standard Deviation Statistic”)

for (stat in statistic) { download_gmted( statistic = stat, resolution =
c(“30 arc-seconds”), directory_to_save =
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/gmted”,
acknowledgement = TRUE, download = TRUE, remove_command = FALSE, unzip =
TRUE, remove_zip = FALSE, hash = FALSE ) }

# Download USGS National Hydrography Dataset (NHD) AKA huc

# DOWNLOADED ZIP ONLY - need 7 - zip program to unzip

download_huc( region = c(“Lower48”, “Islands”), type = c(“Seamless”,
“OceanCatchment”), directory_to_save =
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/huc”,
acknowledgement = TRUE, download = TRUE, remove_command = FALSE, unzip =
FALSE, hash = FALSE )

# unzip huc files

archive_extract(
“/ddn/gs1/group/set/chords/combining_datasets/raw_data/huc/NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z”,
dir = “/ddn/gs1/group/set/chords/combining_datasets/raw_data/huc” )

\`\`\`

\`\`\` {r} \# Unzip the downloaded zip files
