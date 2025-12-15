test_that("class / dimensions are correct", {
  # correct class
  provinces_and_territories |> expect_s3_class("tbl_df")
  provinces_and_territories |> expect_s3_class("sf")

  # correct dimensions
  nrow(provinces_and_territories) |>
    expect_equal(13)
  ncol(provinces_and_territories) |>
    expect_equal(5)

  # correct column names
  colnames(provinces_and_territories) |>
    expect_equal(c("abbreviation", "name_en", "name_fr", "is_province", "geometry"))

  # correct column types
  lapply(provinces_and_territories, class) |>
    unname() |>
    expect_equal(list(
      "factor",
      "factor",
      "factor",
      "logical",
      c("sfc_GEOMETRY", "sfc")
    ))
  
  # correct geometry type
  provinces_and_territories$geometry |>
    sf::st_geometry_type() |>
    expect_in(c("POLYGON", "MULTIPOLYGON"))
  
  # correct is_province values
  provinces_and_territories$is_province |>
    sum() |> 
    expect_equal(10)
  provinces_and_territories$is_province |>
    expect_identical(
      ! provinces_and_territories$abbreviation %in% c("YT", "NT", "NU")
    )
})

test_that("NA's where expected", {
  expect_true(!anyNA(provinces_and_territories$abbreviation))
  expect_true(!anyNA(provinces_and_territories$name_en))
  expect_true(!anyNA(provinces_and_territories$name_fr))
  expect_true(!anyNA(provinces_and_territories$is_province))
  expect_true(!anyNA(provinces_and_territories$geometry))
})
