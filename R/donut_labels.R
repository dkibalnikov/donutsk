#' Create pie or donut label and text annotations
#'
#' The set of annotation functions utilizes layout functions to effectively distribute labels within the available space
#' Annotations are streamlined by leveraging pre-calculated special variables such as `.sum`, `.mean`, and `.n` (see Details).
#' * The function `geom_label_int()` creates `geom_label`-like **internal** donut layer as aggregation of passed values
#' * The function `geom_text_int()` creates `geom_text`-like **internal** donut layer as aggregation of passed values
#' * The function `geom_label_ext()` creates `geom_label`-like **external** donut layer of passed values
#' * The function `geom_text_ext()` creates `geom_text`-like **external** donut layer of passed values
#'
#' The label functions supports `glue::glue()` for convenient label construction like `Total: {.sum}`,
#' where `.sum` is pre-calculated variable. You can still use `glue::glue()` or `paste()`
#' functions to pass data.frame fields for label construction.
#'
#' In addition to generic aesthetics like `color`, `fill`, `alpha`, etc., the following list of pre-calculated variables
#' is available for `geom_label_int()` and `geom_text_int()`:
#'
#' - `.sum`: Summation of the value field
#' - `.mean`: Mean of the value field
#' - `.median`: Median of the value field
#' - `.n`: Observation count of the value field
#' - `.prc`: Percentage of the value field
#'
#' For `geom_label_ext()` and `geom_text_ext()`, which are suitable for external donut labels, the following list of
#' pre-calculated variables is available:
#'
#' - `.prc`: Percentage of the value field for the entire multiplicity
#' - `.prc_grp`: Percentage of the value field for the group defined by `fill`
#'
#' @inheritParams ggplot2::geom_label
#' @param r Sets the radius to place label or text for internal donut
#' @param layout The layout function to effectively display text and labels
#'
#' @seealso [layouts], [pins]
#'
#' @return None
#' @name donut_label
NULL
#> NULL
#'
#' @examples
#' # Create an example data set
#' n <- 30
#' set.seed(2021)
#' df <- dplyr::tibble(
#'   lvl1 = sample(LETTERS[1:5], n, TRUE),
#'   lvl2 = sample(LETTERS[6:24], n, TRUE),
#'   value = sample(1:20, n, TRUE),
#'   highlight_ext = sample(c(FALSE,TRUE), n, TRUE, c(.9, .1))) |>
#'  dplyr::mutate(highlight_int = dplyr::if_else(lvl1 == "A", TRUE, FALSE))
#'
#' # Starting plot with doubled donuts and annotations for internal one
#' p <- dplyr::group_by(df, lvl1, lvl2, highlight_ext, highlight_int) |>
#'  dplyr::summarise(value = sum(value), .groups = "drop") |>
#'  packing(value, lvl1) |>
#'  ggplot(aes(value = value, fill = lvl1)) +
#'  geom_donut_int(aes(highlight = highlight_int), alpha=.5, r_int=.25) +
#'  geom_text_int(lineheight = .8, r=1.2, show.legend = FALSE,
#'   aes(label = "Sum {fill}:\n{.sum}-{scales::percent(.prc)}", col=lvl1)) +
#'  geom_donut_ext(aes(opacity = lvl2, highlight = highlight_ext)) +
#'  scale_fill_viridis_d(option = "inferno", begin = .1, end = .7) +
#'  scale_color_viridis_d(option = "inferno", begin = .1, end = .7) +
#'  guides(alpha=guide_legend(ncol = 2), fill=guide_legend(ncol = 2)) +
#'  theme_void() +
#'  theme(legend.position = "inside", legend.position.inside = c(0.1, 0.9))
#'
#' p + coord_radial(theta = "y", expand = FALSE, rotate_angle = FALSE)
#'
#' # Add labels to external donut as percent inside group
#' p + coord_radial(theta = "y", expand = FALSE, rotate_angle = FALSE) +
#'  geom_label_ext(aes(label=paste0(lvl2, ": {scales::percent(.prc_grp)}")),
#'                 show.legend = FALSE, size=3, col="white")
#'
#' # Leverage ggplot2 feature for labels
#' p + coord_radial(theta = "y", expand = FALSE, rotate_angle = TRUE) +
#'  geom_label_ext(aes(label=paste0(lvl2, ": {scales::percent(.prc)}")),
#'                 show.legend = FALSE, size=3, col="white", angle=90,
#'                 layout = circle())
#'
#' # Leverage another layout
#' p + coord_radial(theta = "y", expand = FALSE, rotate_angle = FALSE) +
#'  geom_label_ext(aes(label=paste0(lvl2, ": {scales::percent(.prc_grp)}")),
#'                 show.legend = FALSE, size=3, col="white",
#'                 layout = tv(thinner = TRUE, thinner_gap = 0.15))
#'
#' @rdname donut_label
#' @usage NULL
#' @export
StatLabelInt <- ggproto("StatLabelInt", Stat,
                        compute_panel = function(data, scales, r){
                          calc_aggr(data, value, fill) |>
                            rowwise() |>
                            mutate(x = r, label = glue(label))
                        },
                        required_aes = c("value", "fill", "label")
)
#'
#' @rdname donut_label
#' @export
geom_label_int <- function(mapping = NULL, data = NULL, stat = "label_int", position = "identity",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                           r=1, ...){
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomLabel, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, r=r, ...)
  )
}
#'
#' @rdname donut_label
#' @usage NULL
#' @export
StatTextInt <- ggproto("StatTextInt", Stat,
                       compute_panel = function(data, scales, r=.75){
                         calc_aggr(data, value, fill) |>
                           rowwise() |>
                           mutate(x = r, label = glue(label))
                       },
                       required_aes = c("value", "fill", "label")
)
#'
#' @rdname donut_label
#' @export
geom_text_int <- function(mapping = NULL, data = NULL, stat = "text_int", position = "identity",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                          r=1, ...){
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomText, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, r=r, ...)
  )
}
#'
#' @rdname donut_label
#' @usage NULL
#' @export
StatLabelExt <- ggproto("StatLabelExt", Stat,
                        compute_panel = function(data, scales, layout){
                          df <- calc_coords(data, value) # populate NULL value to useless pars
                          lt <- layout(df$y)

                          (if(is.data.frame(lt)) mutate(df, x = lt$x, y = lt$y, hjust = if_else(lt$clove, 0, 1))
                            else mutate(df, x = lt)) |>
                            mutate(.prc = value/sum(value)) |>
                            group_by(group) |>
                            mutate(.prc_grp = value/sum(value)) |>
                            ungroup() |>
                            rowwise() |>
                            mutate(label = glue(label))
                        },
                        required_aes = c("value", "label")
)
#'
#' @rdname donut_label
#' @export
geom_label_ext <- function(mapping = NULL, data = NULL, stat = "label_ext", position = "identity",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, layout=circle(), ...){
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomLabel, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, layout=layout, ...)
  )
}
#'
#' @rdname donut_label
#' @usage NULL
#' @export
StatTextExt <- ggproto("StatTextExt", Stat,
                       compute_panel = function(data, scales, layout){
                         df <- calc_coords(data, value) # populate NULL value to useless pars
                         lt <- layout(df$y)

                         (if(is.data.frame(lt)) mutate(df, x = lt$x, y = lt$y, hjust = if_else(y>clove, 1, 0))
                           else mutate(df, x = lt)) |>
                           mutate(.prc = value/sum(value)) |>
                           group_by(group) |>
                           mutate(.prc_grp = value/sum(value)) |>
                           ungroup() |>
                           rowwise() |>
                           mutate(label = glue(label))
                       },
                       required_aes = c("value", "label")
)
#'
#' @rdname donut_label
#' @export
geom_text_ext <- function(mapping = NULL, data = NULL, stat = "text_ext", position = "identity",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, layout=circle(), ...){
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomText, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, layout=layout, ...)
  )
}
