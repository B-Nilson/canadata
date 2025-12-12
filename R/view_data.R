#' @importFrom rlang .data
view_gridded_2016_population <- function(
  gridded_2016_population = gridded_2016_population
) {
  layer_names <- c(
    "Total Population (2016)" = "total_population",
    "Urban Population (2016)" = "urban_population",
    "Rural Population (2016)" = "rural_population"
  )

  # Calculate urban population
  gridded_2016_population_w_urban <- gridded_2016_population |>
    dplyr::mutate(
      urban_population = .data$total_population - .data$rural_population
    )

  palettes <- layer_names |>
    lapply(\(layer_name) {
      domain <- c(0, max(gridded_2016_population_w_urban[[layer_name]]))
      leaflet::colorNumeric("viridis", domain = domain)
    })

  map_data <- gridded_2016_population_w_urban |>
    tidyr::pivot_longer(-(1:4)) |>
    dplyr::relocate("name", .before = 1) |>
    dplyr::mutate(
      name = .data$name |>
        factor(
          levels = layer_names,
          labels = names(layer_names)
        ),
    ) |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") |>
    tidyr::unite(
      col = "label",
      -"geometry",
      sep = "<br/>",
      remove = FALSE
    )

  map_data |>
    dplyr::group_split(.data$name, .keep = TRUE) |>
    lapply(\(group_data) {
      group <- unique(as.character(group_data$name))
      aqmapr::PointLayer(
        data = group_data |> dplyr::filter(.data$value > 0),
        fill = ~value,
        fill_palette = palettes[[group]],
        group = group,
        label = ~ label |> lapply(htmltools::HTML)
      )
    }) |>
    aqmapr::make_leaflet_map(point_layers = _)
}

view_provinces_and_territories <- function(
  provinces_and_territories = provinces_and_territories
) {
  pt_labels <- c("Province", "Territory")
  palette <- "viridis" |>
    leaflet::colorFactor(domain = pt_labels)

  map_data <- provinces_and_territories |>
    dplyr::mutate(
      is_province = .data$is_province |> ifelse(pt_labels[1], pt_labels[2])
    ) |>
    dplyr::relocate("is_province", .before = 1) |>
    tidyr::unite(
      col = "label",
      -"geometry",
      sep = "<br/>",
      remove = FALSE
    )

  map_data |>
    aqmapr::PolygonLayer(
      group = "Provinces and Territories",
      data = _,
      label = ~ label |> lapply(htmltools::HTML),
      fill = ~is_province,
      fill_palette = palette,
      opacity = 0.5
    ) |>
    list() |>
    aqmapr::make_leaflet_map(polygon_layers = _)
}

view_forecast_zones <- function(forecast_zones = forecast_zones) {
  palette <- "viridis" |>
    leaflet::colorFactor(domain = forecast_zones$prov_terr, ordered = TRUE)
  forecast_zones |>
    tidyr::unite(
      col = "label",
      -"geometry",
      sep = "<br/>",
      remove = FALSE
    ) |>
    aqmapr::PolygonLayer(
      group = "Forecast Zones",
      data = _,
      fill = ~prov_terr,
      fill_palette = palette,
      label = ~ label |> lapply(htmltools::HTML),
      opacity = 0.5
    ) |>
    list() |>
    aqmapr::make_leaflet_map(polygon_layers = _)
}

view_communities <- function(communities = communities) {
  palette <- "viridis" |>
    leaflet::colorFactor(domain = communities$type, reverse = TRUE)
  communities |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") |>
    tidyr::unite(
      col = "label",
      -"geometry",
      sep = "<br/>",
      remove = FALSE
    ) |>
    dplyr::group_split(forcats::fct_rev(.data$type), .keep = TRUE) |>
    lapply(\(type_data) {
      aqmapr::PointLayer(
        data = type_data,
        group = unique(as.character(type_data$type)),
        fill = ~type,
        fill_palette = palette,
        label = ~ label |> lapply(htmltools::HTML)
      )
    }) |>
    aqmapr::make_leaflet_map(point_layers = _)
}

view_indigenous_communities <- function(
  indigenous_communities = indigenous_communities
) {
  palette <- "viridis" |>
    leaflet::colorFactor(domain = indigenous_communities$type)

  map_data <- indigenous_communities |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") |>
    tidyr::unite(
      col = "label",
      -"geometry",
      sep = "<br/>",
      remove = FALSE
    )

  map_data |>
    dplyr::group_split(.data$type, .keep = TRUE) |>
    lapply(\(type_data) {
      aqmapr::PointLayer(
        data = type_data,
        group = unique(as.character(type_data$type)),
        fill = ~type,
        fill_palette = palette,
        label = ~ label |> lapply(htmltools::HTML)
      )
    }) |>
    aqmapr::make_leaflet_map(point_layers = _)
}
