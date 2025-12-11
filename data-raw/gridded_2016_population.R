# Data Source
file_name <- "griddedPopulationCanada10km_2016.geojson"
data_url <- "https://agriculture.canada.ca/atlas/data_donnees/" |>
  paste0(
    "griddedPopulationCanada10km/data_donnees/geoJSON/",
    file_name
  )

desired_columns <- c(
  rural_land_area = "RU_LND_AREA_SQKM",
  urban_land_area = "UR_LND_AREA_SQKM",
  rural_population = "RU_POP2A",
  urban_population = "UR_POP2A" #,
  # rural_occupied_dwellings = "RU_POD",
  # urban_occupied_dwellings = "UR_POD"
)

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

# Read in and trim down to data of interest
pop_grid <- local_path |>
  sf::read_sf() |>
  dplyr::select(dplyr::all_of(desired_columns)) |>
  dplyr::mutate(
    dplyr::across(-"geometry", \(x) x |> handyr::swap(NA, with = 0)),
    total_land_area = round((rural_land_area + urban_land_area), digits = 1) |>
      units::set_units("km^2"),
    total_population = as.integer(rural_population + urban_population),
    rural_population = as.integer(rural_population)
  ) |>
  dplyr::filter(total_population > 0) |>
  mark_presence_in_polygon(y = provinces_and_territories, id_col = "abbreviation")

# Find grid centers (space saving)
gridded_2016_population <- pop_grid |>
  sf::st_transform(crs = 3347) |>
  sf::st_centroid() |>
  sf::st_transform(crs = "WGS84") |>
  # Reduce lat/lng precision
  handyr::sf_as_df(keep_coords = TRUE) |>
  dplyr::mutate(dplyr::across(c(lng = "x", lat = "y"), \(x) round(x, 4))) |>
  dplyr::select(
    "lat",
    "lng",
    prov_terr = "abbreviation",
    "total_land_area",
    "total_population",
    "rural_population"
  )

usethis::use_data(gridded_2016_population, overwrite = TRUE)
