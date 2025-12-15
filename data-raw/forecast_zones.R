file_name <- "forecast_zones.geojson"
data_url <- "https://api.weather.gc.ca/collections/public-standard-forecast-zones/items?f=json"

# Download local copy to tempdir
local_path <- file.path(tempdir(), file_name)
if (!file.exists(local_path)) {
  withr::with_options(
    list(timeout = 6000),
    data_url |>
      utils::download.file(destfile = local_path, mode = "wb")
  )
  on.exit({
    unlink(local_path)
  })
}

desired_columns <- c(
  prov_terrs = "PROVINCE_C",
  name_en = "NAME",
  name_fr = "NOM",
  perimeter = "PERIM_KM",
  area = "AREA_KM2",
  "geometry"
)
manitoba_lakes_zone_names <- c(
  "Lake Manitoba",
  "Lake Winnipegosis",
  "Lake Winnipeg - north basin",
  "Lake Winnipeg - south basin"
)
# Read in and cleanup
forecast_zones <- local_path |>
  sf::read_sf() |>
  # Drop the Manitoba Lakes shapes which are over water and overlap the land zones which buffer into the water
  dplyr::filter(! NAME %in% manitoba_lakes_zone_names) |> 
  # Smooth high detail edges
  rmapshaper::ms_simplify(keep_shapes = TRUE, keep = 0.2) |>
  # Select/rename columns
  dplyr::select(dplyr::all_of(desired_columns)) |>
  # Round and add units
  dplyr::mutate(
    perimeter = perimeter |> round(digits = 2) |> units::set_units("km"),
    area = area |> round(digits = 1) |> units::set_units("km^2")
  )

forecast_zones <- forecast_zones |>
  # Sort by first province covered by zone, then bottom left to top right
  dplyr::arrange(
    .data$prov_terrs |>
      gsub(pattern = ",.*", replacement = "") |>
      factor(levels = levels(provinces_and_territories$abbreviation)),
    (.data$geometry |> 
      sf::st_centroid() |>
      sf::st_coordinates())[, 2],
    (.data$geometry |> 
      sf::st_centroid() |>
      sf::st_coordinates())[, 1]
  )

usethis::use_data(forecast_zones, overwrite = TRUE)
