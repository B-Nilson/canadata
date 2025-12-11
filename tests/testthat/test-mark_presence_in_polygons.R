test_that("basic case works", {
  test <- provinces_and_territories |> 
    sf::st_centroid() |> 
    dplyr::rename(expected = "abbreviation") |> 
    mark_presence_in_polygon(provinces_and_territories, id_col = "abbreviation")
  test$abbreviation |> 
    expect_identical(test$expected |> as.character())
})
