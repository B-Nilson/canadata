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
#' A data frame with 14,780 rows and 6 columns:
#' \describe{
#'   \item{total_land_area}{Total land area covered by the original grid cell. Original name: TOT_LND_AREA_SQKM. Units: km^2}
#'   \item{total_population}{The sum of rural (Original column name: RU_POP2A) and urban (Original column name: UR_POP2A) population.}
#'   \item{rural_population}{Total rural population. Original column name: RU_POP2A. From data source: total rural non-institutional and institutional residents falling within the BIOMASS cell, 100% Census data.}
#'   \item{prov_terr}{The province/territory the grid cell is located in, or multiple (seperated by commas) if it covers multiple provinces/territories.}
#'   \item{lng}{Longitude of the centroid of the grid cell. CRS: WGS84}
#'   \item{lat}{Latitude of the centroid of the grid cell. CRS: WGS84}
#'   ...
#' }
#' @source <https://agriculture.canada.ca/atlas/data_donnees/griddedPopulationCanada10km/data_donnees/geoJSON/griddedPopulationCanada10km_2016.geojson>
"gridded_2016_population"

#' Canadian provinces from OpenStreetMap
#'
#' Locations, names, and abbreviations of all provinces and territories in Canada from OpenStreetMap.
#'
#' @format ## `provinces_and_territories`
#' An `sf` data frame with 13 rows and 4 columns:
#' \describe{
#'   \item{name}{Name of province or territory. Factor.}
#'   \item{abbr}{Abbreviation of province or territory. Factor.}
#'   \item{is_province}{TRUE if province, FALSE if territory. Logical.}
#'   \item{geometry}{Province/territory boundary polygons. `sf` sfc_GEOMETRY.}
#'   ...
#' }
#' @source <https://openstreetmap.org>
"provinces_and_territories"
