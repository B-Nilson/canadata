test_that("basic case works", {
  test <- provinces_and_territories |> 
    sf::st_centroid() |> 
    dplyr::rename(expected = "abbr") |> 
    mark_presence_in_polygon(provinces_and_territories, id_col = "abbr")
  test$abbr |> 
    expect_identical(test$expected |> as.character())
})
