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
  prov_terr = "PROVINCE_C",
  name_en = "NAME",
  name_fr = "NOM",
  perimeter = "PERIM_KM",
  area = "AREA_KM2",
  "geometry"
)

# Read in and cleanup
forecast_zones <- local_path |>
  sf::read_sf() |>
  # Smooth high detail edges
  rmapshaper::ms_simplify(keep_shapes = TRUE) |>
  # Select/rename columns
  dplyr::select(dplyr::all_of(desired_columns)) |>
  # Round and add units
  dplyr::mutate(
    perimeter = perimeter |> round(digits = 2) |> units::set_units("km"),
    area = area |> round(digits = 1) |> units::set_units("km^2")
  ) |>
  # Sort by first province covered by zone
  dplyr::arrange(
    .data$prov_terr |>
      gsub(pattern = ",*", replacement = "") |>
      factor(levels = levels(provinces_and_territories$abbreviation))
  )

usethis::use_data(forecast_zones, overwrite = TRUE)
