test_that("class / dimensions are correct", {
  # correct class
  indigenous_communities |>
    expect_s3_class("tbl_df")

  # correct dimensions
  nrow(indigenous_communities) |>
    expect_equal(685)
  ncol(indigenous_communities) |>
    expect_equal(6)

  # correct column names
  colnames(indigenous_communities) |>
    expect_equal(c("name", "type", "prov_terr", "fcst_zone", "lng", "lat"))

  # correct column types
  sapply(indigenous_communities, class) |>
    unname() |>
    expect_equal(c(
      "character",
      "factor",
      "factor",
      "factor",
      "numeric",
      "numeric"
    ))
})

test_that("NA's where expected", {
  expect_true(!anyNA(indigenous_communities$name))
  expect_true(!anyNA(indigenous_communities$type))
  expect_true(!anyNA(indigenous_communities$prov_terr))
  expect_true(sum(is.na(indigenous_communities$fcst_zone)) == 2)
  expect_true(!anyNA(indigenous_communities$lng))
  expect_true(!anyNA(indigenous_communities$lat))
})

test_that("NA fcst zones are truly outside fcst zones", {
  indigenous_communities_with_fcst_zone <- indigenous_communities |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") |>
    sf::st_transform(crs = 3347) |> # equal area projection for Canada
    sf::st_intersection(
      forecast_zones |>
        sf::st_transform(crs = 3347) |>
        dplyr::select(fcst_zone_expected = name_en)
    ) |>
    suppressWarnings() # its okay here that st_intersection assumes attributes are constant over geometries

  indigenous_communities_without_fcst_zone <- indigenous_communities |>
    dplyr::anti_join(
      indigenous_communities_with_fcst_zone,
      by = c("name", "prov_terr")
    ) |>
    dplyr::mutate(fcst_zone_expected = NA_character_)

  expect_identical(
    as.character(indigenous_communities_with_fcst_zone$fcst_zone),
    indigenous_communities_with_fcst_zone$fcst_zone_expected
  )
  expect_identical(
    as.character(indigenous_communities_without_fcst_zone$fcst_zone),
    indigenous_communities_without_fcst_zone$fcst_zone_expected
  )
})
