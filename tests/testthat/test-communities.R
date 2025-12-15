test_that("class / dimensions are correct", {
  # correct class
  communities |>
    expect_s3_class("tbl_df")

  # correct dimensions
  nrow(communities) |>
    expect_equal(11413)
  ncol(communities) |>
    expect_equal(6)

  # correct column names
  colnames(communities) |>
    expect_equal(c("name", "type", "prov_terr", "fcst_zone", "lng", "lat"))

  # correct column types
  sapply(communities, class) |>
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
  expect_true(!anyNA(communities$name))
  expect_true(!anyNA(communities$type))
  expect_true(!anyNA(communities$prov_terr))
  expect_true(sum(is.na(communities$fcst_zone)) == 53)
  expect_true(!anyNA(communities$lng))
  expect_true(!anyNA(communities$lat))
})

test_that("NA fcst zones are truly outside fcst zones", {
  communities_with_fcst_zone <- communities |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") |>
    sf::st_transform(crs = 3347) |> # equal area projection for Canada
    sf::st_intersection(
      forecast_zones |>
        sf::st_transform(crs = 3347) |>
        dplyr::select(fcst_zone_expected = name_en)
    ) |>
    suppressWarnings() # its okay here that st_intersection assumes attributes are constant over geometries

  communities_without_fcst_zone <- communities |>
    dplyr::anti_join(communities_with_fcst_zone, by = c("name", "prov_terr")) |>
    dplyr::mutate(fcst_zone_expected = NA_character_)

  # Handle manually fixed fcst zones
  communities_with_fcst_zone <- communities_with_fcst_zone |>
    dplyr::bind_rows(
      communities_without_fcst_zone |>
        dplyr::filter(
          name %in% c("Mooretown", "Port Lambton"),
          prov_terr == "ON"
        ) |>
        dplyr::mutate(fcst_zone_expected = "Sarnia - Lambton")
    ) |>
    dplyr::arrange(.data$prov_terr, .data$type, .data$name)
  communities_without_fcst_zone <- communities_without_fcst_zone |>
    dplyr::filter(
      !(name %in% c("Mooretown", "Port Lambton") & prov_terr == "ON")
    )

  expect_identical(
    as.character(communities_with_fcst_zone$fcst_zone),
    communities_with_fcst_zone$fcst_zone_expected
  )
  expect_identical(
    as.character(communities_without_fcst_zone$fcst_zone),
    communities_without_fcst_zone$fcst_zone_expected
  )
})
