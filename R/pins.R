#' Connecting labels with donut segments
#'
#' The set of functions served to connect text or labels with donut segments
#' * `geom_pin_line()` - builds curved line to linl label with donut segment
#' * `geom_pin_head()` - builds stylish point heads for pins
#' * `geom_pin()` - handy wrapper for `geom_pin_line()` and `geom_pin_head()`
#'
#' @inheritParams ggplot2::geom_segment
#' @param r The radius where donut is placed
#' @param cut Sets additional two-sided gap for pins
#' @param layout The layout function to effectively display text and labels.
#' Obviously it's better to have the same as for `geom_label_ext` or `geom_text_ext`
#' @param head Boolean - defines whether to add pin head
#' @param ...  Parameters to be passed to `geom_pin_line()` and `geom_pin_head()`
#'
#' @seealso [layouts], [donut_label]
#'
#' @return None
#' @name pins
NULL
#> NULL
#' @examples
#' n <- 30
#' set.seed(2021)
#' df <- dplyr::tibble(
#'   lvl1 = sample(LETTERS[1:5], n, TRUE),
#'   lvl2 = sample(LETTERS[6:24], n, TRUE),
#'   value = sample(1:20, n, TRUE),
#'   highlight_ext = sample(c(FALSE,TRUE), n, TRUE, c(.9, .1))) |>
#'   dplyr::mutate(highlight_int = dplyr::if_else(lvl1 == "A", TRUE, FALSE))
#'
#' # Starting plot with doubled donuts and annotations for internal one
#' p <- dplyr::group_by(df, lvl1, lvl2, highlight_ext, highlight_int) |>
#'   dplyr::summarise(value = sum(value), .groups = "drop") |>
#'   packing(value, lvl1) |>
#'   ggplot(aes(value = value, fill = lvl1)) +
#'   geom_donut_int(aes(highlight = highlight_int), alpha=.5, r_int = .25) +
#'   geom_label_int(aes(label = "Sum {fill}:\n{.sum}-{scales::percent(.prc)}"),
#'                  alpha = .6, col = "white", size = 3, r=1.2) +
#'   geom_donut_ext(aes(opacity = lvl2, highlight = highlight_ext)) +
#'   scale_fill_viridis_d(option = "inferno", begin = .1, end = .7) +
#'   guides(alpha = guide_legend(ncol = 2), fill = guide_legend(ncol = 2)) +
#'   theme_void() +
#'   theme(legend.position = "none")
#'
#' p + coord_radial(theta = "y", expand = FALSE, rotate_angle = FALSE)
#'
#' # Add labels to external donut as percent inside group
#' p + coord_radial(theta = "y", expand = FALSE, rotate_angle = FALSE) +
#'   geom_label_ext(aes(label = paste0(lvl2, ": {scales::percent(.prc_grp)}")),
#'                  show.legend = FALSE, size=3, col="white") +
#'   geom_pin(size = .5, linewidth=.1, show.legend = FALSE, cut = .2)
#'
#' # Leverage tv() layout
#' p + coord_radial(theta = "y", expand = FALSE, rotate_angle = FALSE) +
#'   geom_label_ext(aes(label = paste0(lvl2, ":{scales::percent(.prc_grp)}")),
#'                  show.legend = FALSE, size=3, col="white",
#'                  layout = tv(thinner = TRUE, thinner_gap = .15)) +
#'   geom_pin(size = .5, linewidth=.1, show.legend = FALSE, cut = .2,
#'            layout = tv(thinner = TRUE, thinner_gap = .15))
#'
#' # Leverage another layout
#' p + coord_radial(theta = "y", expand = FALSE, rotate_angle = FALSE) +
#'   geom_label_ext(aes(label = paste0(lvl2, ": {scales::percent(.prc_grp)}")),
#'                  show.legend = FALSE, size=3, col="white", layout = eye()) +
#'   geom_pin(size = .5, linewidth=.1, show.legend = FALSE, layout = eye())
#'
#' @rdname pins
#' @usage NULL
#' @export
StatPinLine <- ggproto("StatPinLine", Stat,
                      compute_panel = function(data, scales, layout, r, cut, size){ # size is dummy parameter
                        df <- calc_coords(data, value) # populate NULL value to useless pars
                        lt <- layout(df$y)

                        (if(is.data.frame(lt)) mutate(df, xend = lt$x, yend = lt$y)
                          else mutate(df, xend = lt, yend = y)) |>
                          mutate(x=r+cut, xend=xend-cut, xend = if_else(xend-x<cut, x, xend))
                      },
                      required_aes = "value"
)
#'
#' @rdname pins
#' @export
geom_pin_line <- function(mapping = NULL, data = NULL, stat = "pin", position = "identity",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         r=1.5, cut=.1, layout = circle(), ...){
  layer(
    data = data, mapping = mapping, stat = StatPinLine, geom = "segment", position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, r=r, cut=cut, layout=layout, ...)
  )
}
#'
#' @rdname pins
#' @usage NULL
#' @export
StatPinHead <- ggproto("StatPinHead", Stat,
                        compute_panel = function(data, scales, layout, r, cut, linewidth){ # dummy linewidth parameter for combining two stats
                          df1 <- calc_coords(data, value) # populate NULL value to useless pars
                          lt <- layout(df1$y)

                          df2 <- (if(is.data.frame(lt)) mutate(df1, xend = lt$x, yend = lt$y)
                            else mutate(df1, xend = lt, yend = y)) |>
                            mutate(x=r+cut, xend=xend-cut, xend = ifelse(xend-x<cut, NA, xend)) |>
                            filter(!is.na(xend))

                          # Define points coordinates as start/finish line

                          select(df2, -xend, -yend) |>
                            bind_rows(select(df2, -x, -y, x = xend, y = yend))
                        },
                        required_aes = "value"
)
#'
#' @rdname pins
#' @export
geom_pin_head <- function(mapping = NULL, data = NULL, stat = "point", position = "identity",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                           r=1.5, cut=.1, layout = circle(), ...){
  layer(
    data = data, mapping = mapping, stat = StatPinHead, geom = "point", position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, r=r, cut=cut, layout=layout, ...)
  )
}
#'
#' @rdname pins
#' @export
geom_pin <- function(..., head=TRUE){
  if(head)list(
    geom_pin_line(...),
    geom_pin_head(...) # linewidth parameter affects nothing
  )else geom_pin_line(...)
}
