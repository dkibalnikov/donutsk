#' Arrange data to distribute small values
#'
#' @description
#' Arrange data to distribute small values further apart from each other
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param value A .data field which contains values to distribute
#' @param level A .data grouping field for distribution
#'
#' @return An object of the same type as .data.
#' @export
#' @examples
#' # Create an example
#' n <- 20
#' df <- dplyr::tibble(
#'  lvl1 = sample(LETTERS[1:5], n, TRUE),
#'  lvl2 = sample(LETTERS[6:24], n, TRUE),
#'  value = sample(1:20, n, TRUE)
#'  )
#'
#' # Arrange all values
#' packing(df, value)
#'
#' # Arrange values within values
#' packing(df, value, lvl1)
packing <- function(.data, value, level=NULL){
  sm <- grp <- id <- NULL # fix globa; variable CMD check note

  lvl <- enquo(level)
  val <- enquo(value)

  if(deparse(substitute(level))!="NULL"){
    l1_rank <- group_by(.data, !!lvl) |>
      summarise(sm = sum(!!val)) |>
      mutate(grp = if_else(sm>stats::median(sm), 2, 1)) |>
      arrange(sm) |>
      mutate(id = if_else(grp == 1, row_number(), NA)) |>  #
      arrange(-sm) |>
      mutate(id = if_else(grp == 2, row_number(), id)) |>
      arrange(id, sm) |>
      mutate(rank = row_number()) |>
      select(!!lvl, rank)

    left_join(.data, l1_rank, by = deparse(substitute(level))) |>
      group_by(rank) |>
      mutate(grp = if_else(!!val>stats::median(!!val), 2, 1)) |>
      arrange(!!val) |>
      mutate(id = if_else(grp == 1, row_number(), NA)) |>
      arrange(-!!val) |>
      mutate(id = if_else(grp == 2, row_number(), id)) |>
      ungroup() |>
      arrange(rank, id, !!val) |>
      select(names(.data) |> all_of())
  }else{
    mutate(.data, grp = if_else(!!val>stats::median(!!val), 2, 1)) |>
      arrange(!!val) |>
      mutate(id = if_else(grp == 1, row_number(), NA)) |>
      arrange(-!!val) |>
      mutate(id = if_else(grp == 2, row_number(), id)) |>
      arrange(id, !!val) |>
      select(names(.data) |> all_of())
  }
}
