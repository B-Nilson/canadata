prov_order <- c(
  BC = "British Columbia",
  AB = "Alberta",
  SK = "Saskatchewan",
  MB = "Manitoba",
  ON = "Ontario",
  QC = "Quebec",
  NB = "New Brunswick",
  NS = "Nova Scotia",
  NL = "Newfoundland and Labrador",
  PE = "Prince Edward Island",
  YT = "Yukon",
  NT = "Northwest Territories",
  NU = "Nunavut"
)

prov_en_francais <- c(
  BC = "Colombie-Britannique",
  AB = "Alberta",
  SK = "Saskatchewan",
  MB = "Manitoba",
  ON = "Ontario",
  QC = "Qu\u00e9bec",
  NB = "Nouvelle-\u00c9cosse",
  NS = "Nouveau-Brunswick",
  NL = "Terre-Neuve-et-Labrador",
  PE = "\u00eele du Prince-\u00c9douard",
  YT = "Yukon",
  NT = "Territoires du Nord-Ouest",
  NU = "Nunavut"
)

# ensure correct order
prov_en_francais <- prov_en_francais[match(
  names(prov_order),
  names(prov_en_francais)
)]

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
    name_en = .data$name |>
      handyr::swap(
        "New Brunswick / Nouveau-Brunswick",
        with = "New Brunswick"
      ) |>
      handyr::swap("Qu\u00e9bec", with = "Quebec") |>
      handyr::swap("\u14c4\u14c7\u1557\u1466 Nunavut", with = "Nunavut") |>
      factor(levels = prov_order),
    name_fr = .data$name_en |>
      factor(levels = prov_order, labels = prov_en_francais),
    abbreviation = .data$name_en |>
      factor(levels = prov_order, labels = names(prov_order)),
    is_province = .data$abbreviation %in% names(prov_order)[1:10]
  ) |>
  dplyr::select(-"name") |>
  dplyr::relocate("name_fr", .after = "name_en") |>
  dplyr::arrange(.data$abbreviation)

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
  dplyr::filter(.data$abbreviation == "PE") |>
  sf::st_write(geojson_path, driver = "GeoJSON")
