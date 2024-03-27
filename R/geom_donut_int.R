#' Internal donut layer
#'
#' The function `geom_donut_int()` creates visually internal donut layer as aggregation of passed values
#'
#' @param data The data to be displayed in this layer. There are three options:
#'
#' If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot().
#'
#' A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame.
#' See fortify() for which variables will be created.
#'
#' A function will be called with a single argument, the plot data. The return value must be a data.frame, and will be used as the layer data.
#' A function can be created from a formula (e.g. ~ head(.x, 10)).
#'
#' @inheritParams ggplot2::geom_rect
#' @param r_int Internal donut radius
#' @param r_ext External pie or donut radius
#' @param hl_shift Sets the spacing to show highlighted segments
#' @param hl_col Sets the color for highlighted segments. When the `colour` parameter is specified, it overrides the hl_col parameter for highlighting segments.
#'
#'
#' @examples
#' # Create an example
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
#' @rdname geom_donut_int
#' @export
StatDonutInt <- ggproto("StatDonutInt", Stat,
                        compute_panel = function(data, scales, r_int, r_ext, hl_shift, hl_col, colour=NA){
                          if(!"highlight" %in% names(data)){data$highlight <- FALSE}
                          calc_aggr(data, val = value, lvl = fill, r_int=r_int, r_ext=r_ext) |>
                            mutate(xmin=if_else(highlight, r_int + hl_shift, r_int),
                                   xmax=if_else(highlight, r_ext + hl_shift, r_ext),
                                   colour=if_else(highlight, hl_col, colour))
                        },
                        required_aes = "value",
                        optional_aes = c("fill", "highlight")
)
