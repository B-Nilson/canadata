#' Gridded 2016 Canada Census of Population
#'
#' Grid centeroids and associated population data from the 2016 Canada Census.
#'
#' The original datasource uses an irregular polygon-based grid from their modelling system,
#' however this results in large files due to each gridcell being its own polygon.
#' This dataset uses the grid centroid instead, resulting in a much smaller file.
#'
#' Prior to finding the centroids, the original gridcells are marked with all provinces/territories they overlap with.
#' If a cell covers multiple provinces/territories, it is marked with all of them (seperated by commas).
#'
#' For further information on the original data, see [here](https://agriculture.canada.ca/atlas/data_donnees/griddedPopulationCanada10km/supportdocument_documentdesupport/en/ISO_19131_Population_of_Canada_10km_Gridded.pdf).
#'
#' @format ## `gridded_2016_population`
#' A tibble data frame with 14,780 rows and 6 columns:
#' \describe{
#'   \item{lng}{Longitude of the centroid of the grid cell. CRS: WGS84}
#'   \item{lat}{Latitude of the centroid of the grid cell. CRS: WGS84}
#'   \item{prov_terrs}{Abbreviation of province/territory the grid cell is located in, or multiple (seperated by commas) if it covers multiple provinces/territories.}
#'   \item{fcst_zones}{English name of the forecast zone the grid cell is located in, or multiple (seperated by commas) if it covers multiple. 19 points are not in any forecast zone, so they are assigned `NA`. Character.}
#'   \item{total_land_area}{Total land area covered by the original grid cell. Original name: TOT_LND_AREA_SQKM. Units: km^2}
#'   \item{total_population}{The sum of rural (Original column name: RU_POP2A) and urban (Original column name: UR_POP2A) population.}
#'   \item{rural_population}{Total rural population. Original column name: RU_POP2A. From data source: total rural non-institutional and institutional residents falling within the BIOMASS cell, 100% Census data.}
#'   ...
#' }
#' @source <https://agriculture.canada.ca/atlas/data_donnees/griddedPopulationCanada10km/data_donnees/geoJSON/griddedPopulationCanada10km_2016.geojson>
"gridded_2016_population"

#' Canadian provinces from OpenStreetMap
#'
#' Locations, names, and abbreviations of all provinces and territories in Canada from OpenStreetMap.
#'
#' Polygons were simplified using `rmapshaper::ms_simplify()` to reduce file size,
#' resulting in less details for some borders.
#'
#' @format ## `provinces_and_territories`
#' An `sf` tibble data frame with 13 rows and 5 columns:
#' \describe{
#'   \item{abbreviation}{Abbreviation of province or territory. Factor.}
#'   \item{name_en}{English name of province or territory. Factor.}
#'   \item{name_fr}{French name of province or territory. Factor.}
#'   \item{is_province}{TRUE if province, FALSE if territory. Logical.}
#'   \item{geometry}{Province/territory boundary polygons. `sf` sfc_GEOMETRY.}
#'   ...
#' }
#' @source <https://openstreetmap.org>
"provinces_and_territories"

#' Canadian Communities from OpenStreetMap
#'
#' All cities, towns, villages and hamlets in Canada from OpenStreetMap.
#'
#' @format ## `communities`
#' A tibble data frame with 11413 rows and 6 columns:
#' \describe{
#'   \item{name}{Name of community. Character.}
#'   \item{type}{Type of community. Either "city", "town", "village", or "hamlet". Factor.}
#'   \item{prov_terr}{Abbreviation of province or territory. Factor.}
#'   \item{fcst_zone}{English name of forecast zone. 53 communities are not in any forecast zone, so they are assigned `NA`. Factor.}
#'   \item{lng}{Longitude of the community. CRS: WGS84}
#'   \item{lat}{Latitude of the community. CRS: WGS84}
#'   ...
#' }
#' @source <https://openstreetmap.org>
"communities"

#' First Nations and Inuit Communities in Canada
#'
#' Names and locations of all First Nations and Inuit communities in Canada from OpenData Canada.
#'
#' @format ## `indigenous_communities`
#' A tibble data frame with 685 rows and 6 columns:
#' \describe{
#'   \item{name}{Name of community. Character.}
#'   \item{type}{Type of community. Either "First Nation", "Inuit", or "First Nations, Inuit". Factor.}
#'   \item{prov_terr}{Abbreviation of province or territory. Factor.}
#'   \item{fcst_zone}{English name of forecast zone. Two communities are not in any forecast zone, so they are assigned `NA`. Factor.}
#'   \item{lng}{Longitude of the community. CRS: WGS84}
#'   \item{lat}{Latitude of the community. CRS: WGS84}
#'   ...
#' }
#' @source <https://open.canada.ca/data/en/dataset/b6567c5c-8339-4055-99fa-63f92114d9e4>, <https://open.canada.ca/data/en/dataset/2bcf34b5-4e9a-431b-9e43-1eace6c873bd>
"indigenous_communities"

#' Aboriginal Lands of Canada Legislative Boundaries
#'
#' Locations and names of all Aboriginal land legislative boundaries in Canada from OpenData Canada.
#'
#' Polygons were simplified using `rmapshaper::ms_simplify()` to reduce file size,
#' resulting in less details for some borders.
#'
#' @format ## `indigenous_lands`
#' An `sf` tibble data frame with 13 rows and 5 columns:
#' \describe{
#'   \item{type}{Boundary type. Either "Indian Land", "Indian Reserve", or "Land Claim".  Factor.}
#'   \item{name_en}{English name of area. Character.}
#'   \item{name_fr}{French name of area. Character.}
#'   \item{prov_terrs}{Abbreviation of province/territory the area is located in, or multiple (seperated by commas) if it covers multiple provinces/territories.}
#'   \item{fcst_zones}{English name of the forecast zone the area is located in, or multiple (seperated by commas) if it covers multiple. 8 polygons are not in any forecast zone, so they are assigned `NA`. Character.}
#'   \item{geometry}{Indigenous lands legislative boundary polygons. `sf` sfc_GEOMETRY.}
#'   ...
#' }
#' @source <https://open.canada.ca/data/en/dataset/522b07b9-78e2-4819-b736-ad9208eb1067>
"indigenous_lands"

#' Canadian Public Forecast Zones
#'
#' The Public Standard Forecast Zones layer is a collection of public program
#' forecast location zone polygons that represents bounded measurable locations
#' at the Public program Standard level.
#' The public program standard level is used in most
#' forecasts, warnings, watches, advisories and special weather statements.
#'
#' Polygons were simplified using `rmapshaper::ms_simplify(keep_shapes = TRUE)` to reduce file size,
#' resulting in less details for some borders.
#'
#' @format ## `forecast_zones`
#' An `sf` tibble data frame with 13 rows and 5 columns:
#' \describe{
#'   \item{prov_terr}{Abbreviation of province(s)/territory(ies) the zone covers, seperated by commas where more than 1. Character.}
#'   \item{name_en}{English name of the zone. Factor.}
#'   \item{name_fr}{French name of the zone. Factor.}
#'   \item{perimeter}{Perimeter of the zone. Units: km.}
#'   \item{area}{Area covered by the zone. Units: km^2.}
#'   \item{geometry}{Forecast zone boundary polygons. `sf` sfc_GEOMETRY.}
#'   ...
#' }
#' @source <https://api.weather.gc.ca/collections/public-standard-forecast-zones>
"forecast_zones"
