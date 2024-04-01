#' Create pie or donut chart
#'
#' Create pie or donut charts while retaining ggplot flexibility, such as leveraging faceting and palettes, and fine-tuning appearance
#' * The function `geom_donut_int()` creates visually **internal** donut layer as aggregation of passed values
#' * The function `geom_donut_ext()` creates visually **external** donut layer of passed values
#'
#'
#' @inheritParams ggplot2::geom_rect
#' @param r_int Internal donut radius
#' @param r_ext External pie or donut radius
#' @param hl_shift Sets the spacing to show highlighted segments
#' @param hl_col Sets the color for highlighted segments. When the `colour` parameter is specified, it overrides the hl_col parameter for highlighting segments.
#'
#' @name geom_donut
NULL
#> NULL
#'
#' @examples
#' # Create an example
#' set.seed(1605)
#' n <- 20
#' df <- dplyr::tibble(
#'   lvl1 = sample(LETTERS[1:5], n, TRUE),
#'   lvl2 = sample(LETTERS[6:24], n, TRUE),
#'   value = sample(1:20, n, TRUE),
#'   highlight_ext = sample(c(FALSE,TRUE), n, TRUE, c(.7, .3))) |>
#'   dplyr::mutate(highlight_int = ifelse(lvl1 == "A", TRUE, FALSE))
#'
#' # Create a simple pie chart
#' ggplot(df, aes(value = value, fill=lvl1)) +
#'   geom_donut_int(alpha=.6) +
#'   coord_polar(theta = "y")
#'
#' # Create a simple donut chart that can handle more granular data
#' # and highlight specific segments
#' ggplot(df, aes(value = value, fill=lvl2, highlight=highlight_ext)) +
#'   geom_donut_int(r_int=.5, alpha=.6, linewidth=.2) +
#'   coord_polar(theta = "y") +
#'   xlim(0, 1.5)
#'
#' # Perform data preparation tasks with `packing()`
#' # and apply specific color
#' packing(df, value) |>
#'   ggplot(aes(value = value, fill=lvl2, highlight=highlight_ext)) +
#'   geom_donut_int(r_int=.5, alpha=.6, linewidth=.2, col = "gray20") +
#'   coord_polar(theta = "y") +
#'   xlim(0, 1.5)
#'
#' # Built combined donut chart with interanl and external layers
#' dplyr::bind_rows(
#' # arrange by value
#' `arrange()` = dplyr::arrange(df, lvl1, lvl2, value),
#' # pack values for better space management
#' `packing()` = packing(df, value, lvl1),
#' .id = "prep_type") |>
#'  ggplot(aes(value = value, fill=lvl1)) +
#'  geom_donut_int(aes(highlight=highlight_int), alpha=.6) +
#'  geom_donut_ext(aes(alpha=ordered(lvl2), highlight=highlight_int)) +
#'  # apply facets
#'  facet_grid(~prep_type) +
#'  # style chart with palette and theme
#'  scale_fill_viridis_d(option = "inferno", begin = .1, end = .7) +
#'  theme_void() +
#'  coord_polar(theta = "y") +
#'  xlim(0, 2.5)
#'
#' @rdname geom_donut
#' @export
geom_donut_int <- function(mapping = NULL, data = NULL, stat = "donut_int", position = "identity",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                           r_int=0, r_ext=1, hl_shift=.1, hl_col="firebrick", ...){
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomRect, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, r_int=r_int, r_ext=r_ext, hl_shift=hl_shift, hl_col=hl_col,  ...)
  )
}
#'
#' @rdname geom_donut
#' @export
geom_donut_ext <- function(mapping = NULL, data = NULL, stat = "donut_ext", position = "identity",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                           r_int=1.5, r_ext=2, hl_shift=.1, hl_col="firebrick", ...){
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomRect, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, r_int=r_int, r_ext=r_ext, hl_shift=hl_shift, hl_col=hl_col,  ...)
  )
}
#'
#' @rdname geom_donut
#' @export
StatDonutInt <- ggproto("StatDonutInt", Stat,
                        compute_panel = function(data, scales, r_int, r_ext, hl_shift, hl_col, colour=NA){
                          if(!"highlight" %in% names(data)){data$highlight <- FALSE}
                          calc_aggr(data, val = value, lvl = fill, r_int=r_int, r_ext=r_ext) |>
                            mutate(xmin=if_else(highlight, r_int + hl_shift, r_int),
                                   xmax=if_else(highlight, r_ext + hl_shift, r_ext),
                                   colour=if_else(highlight, hl_col, colour))
                        },
                        required_aes = c("value", "fill"),
                        optional_aes = "highlight"
)
#'
#' @rdname geom_donut
#' @export
StatDonutExt <- ggproto("StatDonutExt", Stat,
                        compute_panel = function(data, scales, r_int=1.5, r_ext=2, hl_shift=.1, hl_col="firebrick", colour = NA){
                          if(!"highlight" %in% names(data)){data$highlight <- FALSE}
                          calc_coords(data, value, r_int=r_int, r_ext=r_ext) |>
                            mutate(xmin=if_else(highlight, r_int + hl_shift, r_int),
                                   xmax=if_else(highlight, r_ext + hl_shift, r_ext),
                                   colour=if_else(highlight, hl_col, colour))
                        },
                        required_aes = c("value", "fill"),
                        optional_aes = c("highlight", "alpha")
)






