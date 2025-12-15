"https://open.canada.ca/data/en/dataset/f242b881-75e3-40bb-a148-63410b4ce2af"

"https://open.canada.ca/data/en/dataset/522b07b9-78e2-4819-b736-ad9208eb1067"

file_name <- "AL_TA_CA_2_179_eng.shp"
file_name_zip <- "AL_TA_CA_SHP_eng.zip"
data_url <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_al_ta/shp_eng/" |>
  paste0(file_name_zip)
local_dir <- tempdir()
local_path <- file.path(local_dir, file_name)
local_path_zip <- file.path(local_dir, file_name_zip)


# Download local copy to tempdir
if (!file.exists(local_path_zip)) {
  withr::with_options(
    list(timeout = 6000),
    data_url |>
      utils::download.file(destfile = local_path_zip, mode = "wb")
  )
  on.exit({
    unlink(local_path_zip)
  })
}
# unzip if needed
if (!file.exists(local_path)) {
  utils::unzip(local_path_zip, exdir = local_dir)
  on.exit({
    unlink(local_path)
  })
}

desired_cols <- c(
  type = "ALTYPE",
  name_en = "NAME1",
  name_fr = "NAME2",
  prov_terrs = "abbreviation",
  fcst_zones = "name_en",
  "geometry"
)

indigenous_lands <- local_path |>
  sf::read_sf() |>
  # Mark p/t(s) and fcst zone(s)
  sf::st_transform(crs = 3347) |> # equal area projection for Canada
  mark_presence_in_polygon(
    y = provinces_and_territories |>
      sf::st_transform(crs = 3347),
    id_col = "abbreviation"
  ) |>
  mark_presence_in_polygon(
    y = forecast_zones |>
      sf::st_transform(crs = 3347),
    id_col = "name_en"
  ) |>
  sf::st_transform(crs = "WGS84") |>
  # Cleanup and reduce line complexity for smaller files
  sf::st_make_valid() |>
  rmapshaper::ms_simplify(keep_shapes = TRUE) |>
  # Select desired columns and fix types
  dplyr::select(dplyr::all_of(desired_cols)) |>
  dplyr::mutate(type = factor(type)) |>
  # Sort by first province&zones covered by cell, then type
  dplyr::arrange(
    .data$prov_terrs |>
      gsub(pattern = ",.*", replacement = "") |>
      factor(levels = levels(provinces_and_territories$abbreviation)),
    .data$fcst_zones |>
      gsub(pattern = ",.*", replacement = "") |>
      factor(levels = unique(forecast_zones$name_en)),
    .data$type
  )

usethis::use_data(indigenous_lands, overwrite = TRUE, compress = "xz")
