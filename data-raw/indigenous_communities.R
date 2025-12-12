file_names <- c(
  "Premiere_Nation_First_Nation.csv",
  "Communaute_inuite_Inuit_Community.csv"
)
file_names_zip <- file_names |>
  gsub(pattern = "\\.csv", replacement = "_CSV.zip")
data_urls <- "https://data.sac-isc.gc.ca/geomatics/rest/directories/arcgisoutput/DonneesOuvertes_OpenData/" |>
  paste0(file_names |> gsub(pattern = ".csv", replacement = ""), "/") |>
  paste0(file_names_zip)
local_dir <- tempdir()
local_paths <- file.path(local_dir, file_names)
local_paths_zip <- file.path(local_dir, file_names_zip)

for (i in seq_along(file_names)) {
  # Download local copy to tempdir
  if (!file.exists(local_paths_zip[i])) {
    withr::with_options(
      list(timeout = 6000),
      data_urls[i] |>
        utils::download.file(destfile = local_paths_zip[i], mode = "wb")
    )
    on.exit({
      unlink(local_paths_zip[i])
    })
  }
  # unzip if needed
  if (!file.exists(local_paths[i])) {
    utils::unzip(local_paths_zip[i], exdir = local_dir)
    on.exit({
      unlink(local_paths[i])
    })
  }
}

desired_cols <- c(
  name = "BAND_NAME",
  lat = "LATITUDE",
  lng = "LONGITUDE"
)
fn_communities <- local_paths[1] |>
  read.csv() |>
  dplyr::select(dplyr::all_of(desired_cols)) |>
  sf::st_as_sf(coords = c("lng", "lat"), crs = 4617) |> # EPSG:4617 because `COORD_SYS` col is "GCS_North_American_1983_CSRS"
  sf::st_transform(crs = "WGS84") |>
  dplyr::mutate(
    type = "First Nations",
    name = name |> handyr::swap("Inuvik Native", with = "Inuvik") # so matches Inuit data
  )

desired_cols <- c(
  name = "NAME",
  lat = "LATITUDE",
  lng = "LONGITUDE"
)
inuit_communities <- local_paths[2] |>
  read.csv() |>
  dplyr::select(dplyr::all_of(desired_cols)) |>
  sf::st_as_sf(coords = c("lng", "lat"), crs = 4617) |> # EPSG:4617 because `COORD_SYS` col is "GCS_North_American_1983_CSRS"
  sf::st_transform(crs = "WGS84") |>
  dplyr::mutate(type = "Inuit")

indigenous_communities <- dplyr::bind_rows(
  fn_communities,
  inuit_communities
) |>
  handyr::sf_as_df(keep_coords = TRUE) |>
  dplyr::group_by(.data$name) |>
  dplyr::summarise(
    type = sort(unique(.data$type)) |> paste(collapse = ", "),
    dplyr::across(c(lng = "x", lat = "y"), \(x) round(mean(x), 5))
  ) |>
  sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") |>
  mark_presence_in_polygon(
    y = provinces_and_territories |> dplyr::rename(prov_terr = "abbreviation"),
    id_col = "prov_terr"
  ) |>
  mark_presence_in_polygon(
    y = forecast_zones |> dplyr::rename(fcst_zone = "name_en"),
    id_col = "fcst_zone"
  ) |>
  handyr::sf_as_df(keep_coords = TRUE) |>
  dplyr::mutate(
    type = .data$type |> factor(levels = c("First Nations", "Inuit", "First Nations, Inuit")),
    prov_terr = prov_terr |>
      factor(levels = provinces_and_territories$abbreviation)
  ) |>
  dplyr::arrange(.data$prov_terr, .data$type, .data$name) |>
  dplyr::select("name", "type", "prov_terr", "fcst_zone", lng = "x", lat = "y")

usethis::use_data(indigenous_communities, overwrite = TRUE)
