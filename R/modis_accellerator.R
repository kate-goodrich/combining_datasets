library(amadeus)
library(dplyr)
library(tidyr)
library(archive)
library(languageserver)


# Download NASA2 Moderate Resolution Imaging Spectroradiometer (MODIS) AKA modis
# NOT YET DOWNLOADED - TRY LATER

# DO NOT RESTART IF IT ONLY GOES PART THROUGH - IT WILL OVERWRITE THE FILES AND YOU WILL HAVE TO START OVER

years <- list(
    c("2012-01-01", "2012-12-31"),
    c("2013-01-01", "2013-12-31"),
    c("2014-01-01", "2014-12-31"),
    c("2015-01-01", "2015-12-31"),
    c("2016-01-01", "2016-12-31"),
    c("2017-01-01", "2017-12-31"),
    c("2018-01-01", "2018-12-31"),
    c("2019-01-01", "2019-12-31"),
    c("2020-01-01", "2020-12-31"),
    c("2021-01-01", "2021-12-31"),
    c("2022-01-01", "2022-12-31"),
    c("2023-01-01", "2023-12-31"),
    c("2024-01-01", "2024-12-31")
)


products <- c("MOD09A1")
products <- c("MOD11A2")
products <- c("VNP46A2")

nasa_token <- "eyJ0eXAiOiJKV1QiLCJvcmlnaW4iOiJFYXJ0aGRhdGEgTG9naW4iLCJzaWciOiJlZGxqd3RwdWJrZXlfb3BzIiwiYWxnIjoiUlMyNTYifQ.eyJ0eXBlIjoiVXNlciIsInVpZCI6InBvZ3Vla2wiLCJleHAiOjE3NTkwNjc2NTAsImlhdCI6MTc1Mzg4MzY1MCwiaXNzIjoiaHR0cHM6Ly91cnMuZWFydGhkYXRhLm5hc2EuZ292IiwiaWRlbnRpdHlfcHJvdmlkZXIiOiJlZGxfb3BzIiwiYWNyIjoiZWRsIiwiYXNzdXJhbmNlX2xldmVsIjozfQ.OhGSopQgCj-A4dEYlxpCdaQ6EMTNq3EOLEQgN0twuPq5Ds2RFyEjyjvK6wrgjIhlure8VQV6s9VueHG8JHTBu9_JZBdZQt7yJcG76PTVnulP79HWZMpkx5zjy8D54LBrNoRmbFw_EyihOfeJOLOzcCZF1OzAiRqTQtOV_MO7UHVNUDXxEHugM0RK0oTbLVnxRTuwwpL3rHbeHxsa-BmZ3VqRGqdylWKmt4Av8f5ln9j112uSBLwy0ObanmZf43rjzokIbNyGB4HzSUcXAWZ8taKJm2y_AB53NGPYtA7zB2Kqr0TIDGTBeKuG6tXFp0BrgoCyZgreCNx5CdNvvC7Fxg"


for (product in products) {
    # create a subfolder for each product
    product_dir <- file.path(
        "/ddn/gs1/group/set/chords/combining_datasets/raw_data/modis",
        product
    )
    dir.create(product_dir, recursive = TRUE, showWarnings = FALSE)

    for (year_range in years) {
        tryCatch(
            {
                download_modis(
                    product = product,
                    version = "61",
                    horizontal_tiles = c(7, 13),
                    vertical_tiles = c(3, 6),
                    mod06_links = NULL,
                    nasa_earth_data_token = nasa_token,
                    date = year_range,
                    directory_to_save = product_dir, # save into the product-specific folder
                    acknowledgement = TRUE,
                    download = TRUE,
                    remove_command = FALSE,
                    hash = FALSE
                )
            },
            error = function(e) {
                message(paste(
                    "Failed for",
                    product,
                    "in",
                    paste(year_range, collapse = " - "),
                    ":",
                    e$message
                ))
            }
        )
    }
}
