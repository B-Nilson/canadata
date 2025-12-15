test_that("class / dimensions are correct", {
  # correct class
  indigenous_lands |> expect_s3_class("tbl_df")
  indigenous_lands |> expect_s3_class("sf")

  # correct dimensions
  nrow(indigenous_lands) |>
    expect_equal(3361)
  ncol(indigenous_lands) |>
    expect_equal(6)

  # correct column names
  colnames(indigenous_lands) |>
    expect_equal(c(
      "type",
      "name_en",
      "name_fr",
      "prov_terrs",
      "fcst_zones",
      "geometry"
    ))

  # correct column types
  lapply(indigenous_lands, class) |>
    unname() |>
    expect_equal(list(
      "factor",
      "character",
      "character",
      "character",
      "character",
      c("sfc_GEOMETRY", "sfc")
    ))

  # correct geometry type
  indigenous_lands$geometry |>
    sf::st_geometry_type() |>
    expect_in(c("POLYGON", "MULTIPOLYGON"))
})

test_that("NA's where expected", {
  expect_true(!anyNA(indigenous_lands$type))
  expect_true(!anyNA(indigenous_lands$name_en))
  expect_true(!anyNA(indigenous_lands$name_fr))
  expect_true(!anyNA(indigenous_lands$prov_terrs))
  expect_true(sum(is.na(indigenous_lands$fcst_zones)) == 8)
  expect_true(!anyNA(indigenous_lands$geometry))
})
