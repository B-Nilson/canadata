test_that("basic case works", {
  test <- aqmapr::canadian_provinces |> 
    sf::st_centroid() |> 
    dplyr::rename(expected = "abbr") |> 
    mark_presence_in_polygon(aqmapr::canadian_provinces, id_col = "abbr")
  test$abbr |> 
    expect_identical(test$expected |> as.character())
})
