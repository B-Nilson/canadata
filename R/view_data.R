#' @importFrom rlang .data
view_gridded_2016_population <- function(
  gridded_2016_population = gridded_2016_population
) {
  rlang::check_installed(c("leaflet", "htmltools", "aqmapr"))
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
  rlang::check_installed(c("leaflet", "htmltools", "aqmapr"))
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
  rlang::check_installed(c("leaflet", "htmltools", "aqmapr"))
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
  rlang::check_installed(c("leaflet", "htmltools", "aqmapr", "forcats"))
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
  rlang::check_installed(c("leaflet", "htmltools", "aqmapr"))
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

view_indigenous_lands <- function(indigenous_lands = indigenous_lands) {
  rlang::check_installed(c("leaflet", "aqmapr", "htmltools"))

  map_data <- indigenous_lands |>
    tidyr::unite(
      col = "label",
      -"geometry",
      sep = "<br/>",
      remove = FALSE
    )

  map_data |>
    aqmapr::PolygonLayer(
      group = "Indigenous Lands",
      data = _,
      fill = ~type,
      fill_palette = leaflet::colorFactor(
        "viridis",
        levels = indigenous_lands$type |> levels()
      ),
      label = ~ label |> lapply(htmltools::HTML)
    ) |>
    list() |>
    aqmapr::make_leaflet_map(polygon_layers = _)
}

view_all_layers <- function(
  gridded_2016_population = gridded_2016_population,
  provinces_and_territories = provinces_and_territories,
  forecast_zones = forecast_zones,
  communities = communities,
  indigenous_communities = indigenous_communities
) {
  rlang::check_installed(c("leaflet", "htmltools", "aqmapr", "handyr"))

  groups <- c("Population", "Communities", "Indigenous Communities")
  fills <- c("red", "green", "blue")
  point_layers <- list(
    gridded_2016_population,
    communities,
    indigenous_communities
  ) |>
    handyr::for_each(.enumerate = TRUE, .show_progress = FALSE, \(x, i) {
      aqmapr::PointLayer(
        data = x |>
          sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") |>
          tidyr::unite(
            col = "label",
            dplyr::starts_with("prov_terr") | dplyr::starts_with("fcst_zone"),
            sep = "<br/>",
            remove = FALSE
          ),
        group = groups[i],
        fill = fills[i],
        label = ~label
      )
    })

  groups <- c("Provinces and Territories", "Forecast Zones")
  polygon_layers <- list(
    provinces_and_territories,
    forecast_zones
  ) |>
    handyr::for_each(.enumerate = TRUE, .show_progress = FALSE, \(x, i) {
      aqmapr::PolygonLayer(
        data = x,
        group = groups[i],
        opacity = 0.5,
        label = ~name_en
      )
    })

  aqmapr::make_leaflet_map(
    point_layers = point_layers,
    polygon_layers = polygon_layers
  )
}
