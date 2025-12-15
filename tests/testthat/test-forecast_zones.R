test_that("class / dimensions are correct", {
  # correct class
  forecast_zones |> expect_s3_class("tbl_df")
  forecast_zones |> expect_s3_class("sf")

  # correct dimensions
  nrow(forecast_zones) |>
    expect_equal(415)
  ncol(forecast_zones) |>
    expect_equal(6)

  # correct column names
  colnames(forecast_zones) |>
    expect_equal(c(
      "prov_terrs",
      "name_en",
      "name_fr",
      "perimeter",
      "area",
      "geometry"
    ))

  # correct column types
  lapply(forecast_zones, class) |>
    unname() |>
    expect_equal(list(
      "character",
      "character",
      "character",
      "units",
      "units",
      c("sfc_GEOMETRY", "sfc")
    ))

  # correct geometry type
  forecast_zones$geometry |>
    sf::st_geometry_type() |>
    expect_in(c("POLYGON", "MULTIPOLYGON"))

  # correct units
  forecast_zones$perimeter |>
    attr("units") |>
    as.character() |>
    expect_equal("km")
  forecast_zones$area |>
    attr("units") |>
    as.character() |>
    expect_equal("km^2")
})

test_that("NA's where expected", {
  expect_true(!anyNA(forecast_zones$prov_terrs))
  expect_true(!anyNA(forecast_zones$name_en))
  expect_true(!anyNA(forecast_zones$name_fr))
  expect_true(!anyNA(forecast_zones$perimeter))
  expect_true(!anyNA(forecast_zones$area))
  expect_true(!anyNA(forecast_zones$geometry))
})
