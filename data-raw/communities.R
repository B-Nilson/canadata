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
      communities <- pt |>
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
      # keep only communities that are within the province/territory
      is_within_pt <- lengths(sf::st_intersects(communities, pt)) > 0
      communities |> dplyr::filter(is_within_pt)
    }
  ) |>
  stats::setNames(provinces_and_territories$abbreviation)

communities <- osm_communities |>
  dplyr::bind_rows(.id = "prov_terr") |>
  dplyr::mutate(
    prov_terr = prov_terr |>
      factor(levels = provinces_and_territories$abbreviation),
    type = factor(type, c("city", "town", "village", "hamlet"))
  ) |>
  dplyr::filter(complete.cases(.data$type, .data$name)) |>
  sf::st_transform(crs = "WGS84") |> # just in case
  mark_presence_in_polygon(
    y = forecast_zones |> dplyr::rename(fcst_zone = "name_en"),
    id_col = "fcst_zone"
  ) |>
  handyr::sf_as_df(keep_coords = TRUE) |>
  dplyr::select(
    "name",
    "type",
    "prov_terr",
    "fcst_zone",
    lng = "x",
    lat = "y"
  ) |>
  dplyr::arrange(.data$prov_terr, .data$type, .data$name) |>
  tibble::as_tibble()

row.names(communities) <- NULL

usethis::use_data(communities, overwrite = TRUE)
