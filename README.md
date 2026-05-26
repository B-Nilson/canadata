
<!-- README.md is generated from README.Rmd. Please edit that file -->

# canadata

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/canadata)](https://CRAN.R-project.org/package=canadata)
<!-- badges: end -->

The goal of canadata (“Canada” + “data”) is to provide standardized
Canadian spatial datasets for use in R.

The majority of data comes effectively unaltered from open data sources
such as OpenStreetMap (thanks to {osmdata}) and Canada’s Federal
OpenData platform. Where possible, links to related datasets will be
added (such as what Province/Territory, or forecast zone a community is
in).

All polygon layers have been smoothed as minimally as possible for file
size requirements while trying to maintain spatial accuracy. As a
result, more complex boundaries may be smoother than in reality causing
potential edge case issues. Please [submit and
issue](https://github.com/B-Nilson/canadata/issues/new) if you encounter
a problem that could be resolved be less smoothing.

All point layers have been saved as non-sf objects (lat/lng columns
instead of a POINT geometry), and require converting to sf before
performing spatial operations (see \#converting-point-layers)

## Installation

You can install the development version of canadata like so:

``` r
# install.packages("pak")
pak::pak("B-Nilson/canadata")
```

## Datasets

The primary datasets made available are:

``` r
library(canadata)

provinces_and_territories |> sf::st_drop_geometry() |> head(13)
#>    abbreviation                   name_en                   name_fr is_province
#> 1            BC          British Columbia      Colombie-Britannique        TRUE
#> 2            AB                   Alberta                   Alberta        TRUE
#> 3            SK              Saskatchewan              Saskatchewan        TRUE
#> 4            MB                  Manitoba                  Manitoba        TRUE
#> 5            ON                   Ontario                   Ontario        TRUE
#> 6            QC                    Quebec                    Québec        TRUE
#> 7            NB             New Brunswick           Nouvelle-Écosse        TRUE
#> 8            NS               Nova Scotia         Nouveau-Brunswick        TRUE
#> 9            NL Newfoundland and Labrador   Terre-Neuve-et-Labrador        TRUE
#> 10           PE      Prince Edward Island     île du Prince-Édouard        TRUE
#> 11           YT                     Yukon                     Yukon       FALSE
#> 12           NT     Northwest Territories Territoires du Nord-Ouest       FALSE
#> 13           NU                   Nunavut                   Nunavut       FALSE
forecast_zones |> sf::st_drop_geometry() |> head(10)
#>    prov_terrs                 name_en                      name_fr   perimeter
#> 1          BC        Greater Victoria               Grand Victoria 148.70 [km]
#> 2          BC   Southern Gulf Islands            sud des îles Gulf 203.48 [km]
#> 3          BC Manning - Skagit Valley   Manning - vallée de Skagit 201.28 [km]
#> 4          BC                Boundary                     Boundary 263.45 [km]
#> 5          BC         Metro Vancouver              Metro Vancouver 258.94 [km]
#> 6          BC           West Kootenay               Kootenay Ouest 288.45 [km]
#> 7          BC Inland Vancouver Island île de Vancouver - Intérieur 630.59 [km]
#> 8          BC   West Vancouver Island       île de Vancouver Ouest 810.60 [km]
#> 9          BC             Similkameen                  Similkameen 291.84 [km]
#> 10         BC           Fraser Valley             vallée du Fraser 396.42 [km]
#>              area
#> 1   1369.6 [km^2]
#> 2   1924.0 [km^2]
#> 3   1851.2 [km^2]
#> 4   4067.6 [km^2]
#> 5   3269.8 [km^2]
#> 6   4810.6 [km^2]
#> 7   9319.1 [km^2]
#> 8  14846.0 [km^2]
#> 9   3968.1 [km^2]
#> 10  6149.9 [km^2]
indigenous_lands |> sf::st_drop_geometry() |> head(10)
#>              type                                 name_en
#> 1  Indian Reserve                              COLE BAY 3
#> 2  Indian Reserve                       SENANUS ISLAND 10
#> 3  Indian Reserve (DEADMAN'S ISLAND) HALKETT ISLAND NO. 2
#> 4  Indian Reserve                           BARE ISLAND 9
#> 5  Indian Reserve                          TWIN ISLAND 10
#> 6  Indian Reserve                          HATCH POINT 12
#> 7  Indian Reserve                      DISCOVERY ISLAND 3
#> 8  Indian Reserve                             UNION BAY 4
#> 9  Indian Reserve                           GOLDSTREAM 13
#> 10 Indian Reserve                              MALAHAT 11
#>                             name_fr prov_terrs       fcst_zones
#> 1                    COLE BAY NO  3         BC Greater Victoria
#> 2              SENANUS ISLAND NO 10         BC Greater Victoria
#> 3  (DEADMAN'S) HALKETT ISLAND NO. 2         BC Greater Victoria
#> 4                 BARE ISLAND NO  9         BC Greater Victoria
#> 5                 TWIN ISLAND NO 10         BC Greater Victoria
#> 6                HATCH POINT NO  12         BC Greater Victoria
#> 7            DISCOVERY ISLAND NO  3         BC Greater Victoria
#> 8                    UNION BAY NO 4         BC Greater Victoria
#> 9                 GOLDSTREAM NO  13         BC Greater Victoria
#> 10                    MALAHAT NO 11         BC Greater Victoria
communities |> sf::st_drop_geometry() |> head(10)
#>              name type prov_terr             fcst_zone       lng      lat
#> 1      Abbotsford city        BC         Fraser Valley -122.3295 49.05212
#> 2         Burnaby city        BC       Metro Vancouver -122.9725 49.24338
#> 3  Campbell River city        BC East Vancouver Island -125.2442 50.02307
#> 4       Castlegar city        BC         West Kootenay -117.6636 49.31617
#> 5      Chilliwack city        BC         Fraser Valley -121.9526 49.17098
#> 6         Colwood city        BC      Greater Victoria -123.4940 48.42341
#> 7       Coquitlam city        BC       Metro Vancouver -122.7933 49.28430
#> 8       Courtenay city        BC East Vancouver Island -124.9955 49.68941
#> 9       Cranbrook city        BC         East Kootenay -115.7673 49.51075
#> 10   Dawson Creek city        BC      B.C. Peace River -120.2364 55.76053
indigenous_communities |> sf::st_drop_geometry() |> head(10)
#>                             name          type prov_terr             fcst_zone
#> 1        ?Akisq'nuk First Nation First Nations        BC         East Kootenay
#> 2         ?Esdilagh First Nation First Nations        BC               Cariboo
#> 3                          ?aqam First Nations        BC         East Kootenay
#> 4                     Adams Lake First Nations        BC               Shuswap
#> 5                       Ahousaht First Nations        BC West Vancouver Island
#> 6                     Aitchelitz First Nations        BC         Fraser Valley
#> 7                       Ashcroft First Nations        BC        South Thompson
#> 8                    Beecher Bay First Nations        BC West Vancouver Island
#> 9                 Binche Whut'en First Nations        BC      Stuart - Nechako
#> 10 Blueberry River First Nations First Nations        BC      B.C. Peace River
#>          lng      lat
#> 1  -115.9640 50.45850
#> 2  -122.4968 52.56285
#> 3  -115.7545 49.58350
#> 4  -119.7015 50.82869
#> 5  -126.0561 49.27806
#> 6  -121.9869 49.15042
#> 7  -121.3225 50.72039
#> 8  -123.6083 48.33757
#> 9  -124.4942 54.56625
#> 10 -121.1124 56.70405
gridded_2016_population |> sf::st_drop_geometry() |> head(10)
#>        lat       lng prov_terrs
#> 1  48.4255 -123.6381         BC
#> 2  48.6248 -123.6104         BC
#> 3  48.3420 -123.5887         BC
#> 4  48.5414 -123.5610         BC
#> 5  48.7408 -123.5329         BC
#> 6  48.4580 -123.5116         BC
#> 7  48.6574 -123.4835         BC
#> 8  48.3745 -123.4625         BC
#> 9  48.5739 -123.4341         BC
#> 10 48.4904 -123.3850         BC
#>                                                        fcst_zones
#> 1  Greater Victoria,Inland Vancouver Island,West Vancouver Island
#> 2                          Greater Victoria,East Vancouver Island
#> 3                          Greater Victoria,West Vancouver Island
#> 4                          Greater Victoria,East Vancouver Island
#> 5    Greater Victoria,Southern Gulf Islands,East Vancouver Island
#> 6                                                Greater Victoria
#> 7    Greater Victoria,Southern Gulf Islands,East Vancouver Island
#> 8                                                Greater Victoria
#> 9                                                Greater Victoria
#> 10                                               Greater Victoria
#>    total_land_area total_population rural_population
#> 1     100.4 [km^2]             2035             1750
#> 2      94.6 [km^2]             9475             2770
#> 3     100.5 [km^2]             3435             3435
#> 4     100.2 [km^2]             1905             1905
#> 5     100.2 [km^2]             3105             1270
#> 6      99.0 [km^2]            56425             2070
#> 7     100.3 [km^2]            12795             1100
#> 8     100.5 [km^2]             4800             1145
#> 9     100.1 [km^2]            24715             4010
#> 10     97.4 [km^2]           116745             3110
```

## Converting Point Layers

All point layers have been saved as non-sf objects (lat/lng columns
instead of a POINT geometry) with a CRS of `"WGS84"`, and require
converting to sf before performing spatial operations:

``` r
library(canadata)

communities |>
  sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")

indigenous_communities |>
  sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")

gridded_2016_population |>
  sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")
```
