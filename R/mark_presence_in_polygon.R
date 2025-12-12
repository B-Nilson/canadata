#' @importFrom rlang :=
mark_presence_in_polygon <- function(x, y, id_col = "id") {
  # Mark the id of the polygon that each cell is fully within
  fully_within_y <- x |>
    sf::st_within(y, sparse = FALSE) |>
    which(arr.ind = TRUE) |>
    as.data.frame() |>
    dplyr::distinct(row, .keep_all = TRUE) |>
    dplyr::mutate(!!id_col := as.character(y[[id_col]][col])) |>
    tidyr::complete(row = seq(1, max(row))) |>
    dplyr::arrange(row)
  x[[id_col]] <- fully_within_y[[id_col]]

  # Mark the ids of the polygons that each cell is partially within
  is_not_within <- fully_within_y$row[is.na(fully_within_y$col)]
  not_within <- x[is_not_within, ]
  if (nrow(not_within) == 0) {
    return(x)
  }
  overlaps_y <- not_within |>
    sf::st_overlaps(y, sparse = FALSE) |>
    which(arr.ind = TRUE) |>
    as.data.frame() |>
    dplyr::mutate(!!id_col := y[[id_col]][col])

  if (nrow(overlaps_y) > 0) {
    overlaps_y <- overlaps_y |>
      # Handle cells bordering multiple shapes
      dplyr::group_by(row) |>
      dplyr::summarise(
        !!id_col := paste(!!rlang::sym(id_col), collapse = ",")
      ) |>
      tidyr::complete(row = seq(1, max(row))) |>
      dplyr::arrange(row)

    x[[id_col]][is_not_within] <- overlaps_y[[id_col]]
  }
  return(x)
}
