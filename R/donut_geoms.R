#' Create pie or donut chart
#'
#' Create pie or donut charts while retaining ggplot flexibility, such as leveraging faceting and palettes, and fine-tuning appearance
#' * The function `geom_donut_int()` creates visually **internal** donut layer as aggregation of passed values
#' * The function `geom_donut_ext()` creates visually **external** donut layer of passed values
#' * `geom_donut_int0()` and `geom_donut_ext()` are generic geoms not supporting highlight feature
#'
#' There are two additional aesthetics possible to use:
#'  * `highlight` - optional aesthetic which expects logical (TRUE/FALSE) variable in order to highlight particular donut segments
#'  * `opacity` - operates pretty much the same as `alpha` but ensure more contrast colors and removes legend. Once `alpha` is set `opacity` does not affect a chart
#'
#' @inheritParams ggplot2::geom_rect
#' @param r_int Internal donut radius
#' @param r_ext External pie or donut radius
#' @param hl_shift Sets the spacing to show highlighted segments
#' @param hl_col Sets the color for highlighted segments. It's possible to use both simultaneously `hl_col` and generic `colour`
#'
#' @return None
#' @name donut_geom
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
#'  geom_donut_ext(aes(opacity=lvl2, highlight=highlight_int)) +
#'  # apply facets
#'  facet_grid(~prep_type) +
#'  # style chart with palette and theme
#'  scale_fill_viridis_d(option = "inferno", begin = .1, end = .7) +
#'  theme_void() +
#'  coord_polar(theta = "y") +
#'  xlim(0, 2.5)
#'
#' @rdname donut_geom
#' @usage NULL
#' @export
StatDonutInt <- ggproto("StatDonutInt", Stat,
                        compute_panel = function(data, scales, r_int, r_ext, hl_shift){
                          if(!"highlight" %in% names(data)){data$highlight <- FALSE}
                          calc_aggr(data, val = value, lvl = fill) |>
                            mutate(xmin=if_else(highlight, r_int + hl_shift, r_int),
                                   xmax=if_else(highlight, r_ext + hl_shift, r_ext))
                        },
                        required_aes = c("value", "fill"),
                        optional_aes = "highlight"
)
#'
#' @rdname donut_geom
#' @export
geom_donut_int0 <- function(mapping = NULL, data = NULL, stat = "donut_int", position = "identity",
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                            r_int=0, r_ext=1, hl_shift=.1, ...){
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomRect, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, r_int=r_int, r_ext=r_ext, hl_shift=hl_shift, ...)
  )
}
#'
#' @rdname donut_geom
#' @usage NULL
#' @export
StatDonutIntHl <- ggproto("StatDonutIntHl", Stat,
                          compute_panel = function(data, scales, r_int, r_ext, hl_shift){
                            if(!"highlight" %in% names(data)){data$highlight <- FALSE}
                            calc_aggr(data, val = value, lvl = fill) |>
                              mutate(xmin=if_else(highlight, r_int + hl_shift, r_int),
                                     xmax=if_else(highlight, r_ext + hl_shift, r_ext)) |>
                              filter(highlight)
                          },
                          required_aes = c("value", "fill"),
                          optional_aes = "highlight"
)
#'
#' @rdname donut_geom
#' @usage NULL
#' @export
geom_donut_int_hl <- function(mapping = NULL, data = NULL, stat = "donut_int_hl", position = "identity",
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                              r_int=0, r_ext=1, hl_shift=.1, ...){
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomRect, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, r_int=r_int, r_ext=r_ext, hl_shift=hl_shift,...)
  )
}

#'
#' @rdname donut_geom
#' @export
geom_donut_int <- function(..., hl_col="firebrick"){
  list(
    geom_donut_int0(...),
    suppressWarnings(geom_donut_int_hl(colour=hl_col, ...))
  )
}
#'
#' @rdname donut_geom
#' @usage NULL
#' @export
StatDonutExt <- ggproto("StatDonutExt", Stat,
                        compute_panel = function(data, scales, r_int, r_ext, hl_shift, hl_col){
                          if(!"highlight" %in% names(data)){data$highlight <- FALSE}
                          df1 <- calc_coords(data, value) |>
                            mutate(xmin=if_else(highlight, r_int + hl_shift, r_int),
                                   xmax=if_else(highlight, r_ext + hl_shift, r_ext))

                          if("alpha"%in% names(data)) df1
                          else group_by(df1, fill) |>
                            mutate(alpha = dplyr::cume_dist(opacity))
                        },
                        required_aes = c("value", "fill"),
                        optional_aes = c("highlight", "opacity")
)
#'
#' @rdname donut_geom
#' @export
geom_donut_ext0 <- function(mapping = NULL, data = NULL, stat = "donut_ext", position = "identity",
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                            r_int=1.5, r_ext=2, hl_shift=.1, ...){
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomRect, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, r_int=r_int, r_ext=r_ext, hl_shift=hl_shift, ...)
  )
}
#'
#' @rdname donut_geom
#' @usage NULL
#' @export
StatDonutExtHl <- ggproto("StatDonutExtHl", Stat,
                          compute_panel = function(data, scales, r_int, r_ext, hl_shift, hl_col){
                            if(!"highlight" %in% names(data)){data$highlight <- FALSE}
                            df1 <- calc_coords(data, value) |>
                              mutate(xmin=if_else(highlight, r_int + hl_shift, r_int),
                                     xmax=if_else(highlight, r_ext + hl_shift, r_ext)) |>
                              filter(highlight)
                          },
                          required_aes = "value",
                          optional_aes = c("highlight", "opacity")
)
#'
#' @rdname donut_geom
#' @usage NULL
#' @export
geom_donut_ext_hl <- function(mapping = NULL, data = NULL, stat = "donut_ext_hl", position = "identity",
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                              r_int=1.5, r_ext=2, hl_shift=.1, ...){
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomRect, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, r_int=r_int, r_ext=r_ext, hl_shift=hl_shift, ...)
  )
}
#'
#' @rdname donut_geom
#' @export
geom_donut_ext <- function(..., hl_col="firebrick"){
  list(
    geom_donut_ext0(...),
    suppressWarnings(geom_donut_ext_hl(colour=hl_col, fill=NA, ...))
  )
}
