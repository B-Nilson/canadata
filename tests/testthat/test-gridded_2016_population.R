test_that("class / dimensions are correct", {
  # correct class
  gridded_2016_population |>
    expect_s3_class("tbl_df")

  # correct dimensions
  nrow(gridded_2016_population) |>
    expect_equal(14780)
  ncol(gridded_2016_population) |>
    expect_equal(7)

  # correct column names
  colnames(gridded_2016_population) |>
    expect_equal(c(
      "lat",
      "lng",
      "prov_terrs",
      "fcst_zones",
      "total_land_area",
      "total_population",
      "rural_population"
    ))

  # correct column types
  sapply(gridded_2016_population, class) |>
    unname() |>
    expect_equal(c(
      "numeric",
      "numeric",
      "character",
      "character",
      "units",
      "integer",
      "integer"
    ))
})

test_that("NA's where expected", {
  expect_true(!anyNA(gridded_2016_population$lat))
  expect_true(!anyNA(gridded_2016_population$lng))
  expect_true(!anyNA(gridded_2016_population$prov_terrs))
  expect_true(sum(is.na(gridded_2016_population$fcst_zones)) == 19)
  expect_true(!anyNA(gridded_2016_population$total_land_area))
  expect_true(!anyNA(gridded_2016_population$total_population))
  expect_true(!anyNA(gridded_2016_population$rural_population))
})
