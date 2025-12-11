prov_order <- c(
  BC = "British Columbia",
  AB = "Alberta",
  SK = "Saskatchewan",
  MB = "Manitoba",
  ON = "Ontario",
  QC = "Qu\u00e9bec",
  NB = "New Brunswick",
  NS = "Nova Scotia",
  PE = "Prince Edward Island",
  NL = "Newfoundland and Labrador",
  YT = "Yukon",
  NT = "Northwest Territories",
  NU = "Nunavut"
)

bbox <- osmdata::getbb("Canada", format_out = "sf_polygon")

osm_results <- osmdata::opq(bbox = bbox, timeout = 600) |>
  osmdata::add_osm_feature(key = "type", value = "boundary") |>
  osmdata::add_osm_feature(key = "boundary", value = "administrative") |>
  osmdata::add_osm_feature(key = "border_type", value = "province") |>
  osmdata::add_osm_feature(key = "admin_level", value = "4") |>
  osmdata::osmdata_sf() |>
  osmdata::unique_osmdata()
osm_results <- osm_results$osm_multipolygons

provinces_and_territories <- osm_results |>
  dplyr::select("name", "geometry") |>
  dplyr::mutate(
    name = .data$name |>
      handyr::swap(
        "New Brunswick / Nouveau-Brunswick",
        with = "New Brunswick"
      ) |>
      handyr::swap("ᓄᓇᕗᑦ Nunavut", with = "Nunavut") |>
      factor(levels = prov_order),
    abbr = .data$name |>
      factor(levels = prov_order, labels = names(prov_order)),
    is_province = .data$abbr %in% names(prov_order)[1:10]
  ) |>
  dplyr::arrange(.data$name)

row.names(provinces_and_territories) <- NULL

# Union all provinces
for (i in seq_len(nrow(provinces_and_territories))) {
  provinces_and_territories$geometry[i] <-
    provinces_and_territories$geometry[i] |>
    sf::st_union()
}

# Smooth edges for smaller file size
provinces_and_territories <- provinces_and_territories |>
  rmapshaper::ms_simplify()

# Write out data
usethis::use_data(provinces_and_territories, overwrite = TRUE, compress = "xz")

# write out example to geojson as well
geojson_path <- "inst/extdata/example.geojson"
file.remove(geojson_path) |> invisible() |> suppressWarnings()
provinces_and_territories |>
  dplyr::filter(.data$abbr == "PE") |>
  sf::st_write(geojson_path, driver = "GeoJSON")
