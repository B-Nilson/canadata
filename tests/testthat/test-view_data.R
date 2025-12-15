test_that("no warnings/errors for communities", {
  skip_if_not(interactive())
  view_communities() |>
    expect_silent()
})

test_that("no warnings/errors for indigenous communities", {
  skip_if_not(interactive())
  view_indigenous_communities() |>
    expect_silent()
})

test_that("no warnings/errors for indigenous lands", {
  skip_if_not(interactive())
  view_indigenous_lands(indigenous_lands) |>
    expect_silent()
})

test_that("no warnings/errors for forecast zones", {
  skip_if_not(interactive())
  view_forecast_zones() |>
    expect_silent()
})

test_that("no warnings/errors for provinces and territories", {
  skip_if_not(interactive())
  view_provinces_and_territories() |>
    expect_silent()
})

test_that("no warnings/errors for gridded population", {
  skip_if_not(interactive())
  view_gridded_2016_population() |>
    expect_silent()
})

test_that("no warnings/errors for all layers together", {
  skip_if_not(interactive())

  view_all_layers(
    gridded_2016_population = gridded_2016_population |>
      dplyr::filter(is.na(fcst_zones)),
    provinces_and_territories = provinces_and_territories,
    forecast_zones = forecast_zones,
    communities = communities,
    indigenous_communities = indigenous_communities
  ) |>
    expect_silent()
})

test_that("all locations not in a forecast zone are expected", {
  skip_if_not(interactive())

  view_all_layers(
    gridded_2016_population = gridded_2016_population |>
      dplyr::filter(is.na(fcst_zones)),
    provinces_and_territories = provinces_and_territories,
    forecast_zones = forecast_zones,
    communities = communities |> dplyr::filter(is.na(fcst_zone)),
    indigenous_communities = indigenous_communities |>
      dplyr::filter(is.na(fcst_zone))
  ) |>
    expect_silent()
})
