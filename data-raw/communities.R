get_communities <- function(
  bbox,
  types = c("city", "town", "village", "hamlet"),
  timeout = 10000,
  quiet = TRUE
) {
  # Constants
  desired_cols <- c(type = "place", "name")

  # Convert area name to bounding box
  if (is.character(bbox)) {
    bbox <- osmdata::getbb(bbox, limit = 1, format_out = "polygon")
  }

  # Get OSM data for select place types within bbox
  osm_results <- bbox |>
    osmdata::opq(timeout = timeout) |>
    osmdata::add_osm_feature(
      key = "place",
      value = types
    ) |>
    osmdata::osmdata_sf(quiet = quiet) |>
    osmdata::unique_osmdata()

  osm_results$osm_points |>
    dplyr::select(dplyr::any_of(desired_cols))
}

osm_communities <- provinces_and_territories$geometry |>
  lapply(
    \(pt) {
      pt |>
        sf::st_make_grid(n = c(4, 4)) |> # cut into 16 polygons
        # get bounding boxes for each
        lapply(\(x) {
          bbox <- sf::st_bbox(x) |> unlist() |> matrix(ncol = 2, nrow = 2)
          rownames(bbox) <- c("x", "y")
          colnames(bbox) <- c("min", "max")
          return(bbox)
        }) |>
        # get communities within each bbox
        handyr::for_each(.bind = TRUE, \(bbox) {
          get_communities(bbox) |>
            # try a second time on failures in case of server issues
            tryCatch(error = \(e) get_communities(bbox)) |>
            # try a third time after a sleep on failures in case of server issues
            tryCatch(error = \(e) {
              warn(
                "Failed twice in a row, sleeping for 5 seconds and trying again."
              )
              Sys.sleep(5)
              get_communities(bbox)
            }) |>
            # If still fails, return NULL
            handyr::on_error(.return = NULL)
        })
    }
  )

communities <- osm_communities |>
  dplyr::bind_rows() |>
  # Drop non-desired types and the handful with missing names
  dplyr::mutate(type = factor(type, c("city", "town", "village", "hamlet"))) |>
  dplyr::filter(complete.cases(.data$type, .data$name)) |>
  # Mark province/territory and forecast zone
  sf::st_transform(crs = 3347) |> # equal area projection for Canada
  mark_presence_in_polygon(
    y = provinces_and_territories |> sf::st_transform(crs = 3347),
    id_col = "abbreviation"
  ) |>
  dplyr::filter(
    # drop US communities (where bounding boxes in search partly cover the US)
    !is.na(.data$abbreviation),
    # drop community in US that snuck in as a NB community
    !(.data$name == "Baring Plantation" & .data$abbreviation == "NB")
  ) |>
  mark_presence_in_polygon(
    y = forecast_zones |> sf::st_transform(crs = 3347),
    id_col = "name_en"
  ) |>
  sf::st_transform(crs = "WGS84") |>
  # Drop sf typing for space saving
  handyr::sf_as_df(keep_coords = TRUE) |>
  dplyr::select(
    "name",
    "type",
    prov_terr = "abbreviation",
    fcst_zone = "name_en",
    lng = "x",
    lat = "y"
  ) |>
  # Cleanup
  dplyr::mutate(
    # manual fix for points just on the edge of a fcst zone
    fcst_zone = dplyr::case_when(
      is.na(.data$fcst_zone) &
        .data$name %in% c("Port Lambton", "Mooretown") &
        .data$prov_terr == "ON" ~ "Sarnia - Lambton",
      TRUE ~ .data$fcst_zone
    ),
    # Factorize
    prov_terr = prov_terr |>
      factor(levels = provinces_and_territories$abbreviation),
    fcst_zone = fcst_zone |>
      factor(levels = forecast_zones$name_en)
  ) |>
  dplyr::arrange(.data$prov_terr, .data$type, .data$name) |>
  dplyr::distinct() |>
  tibble::as_tibble()

usethis::use_data(communities, overwrite = TRUE)
