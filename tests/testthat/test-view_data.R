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
