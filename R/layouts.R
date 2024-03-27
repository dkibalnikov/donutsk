#' The set of layout functions is designed to effectively display text and labels
#'
#' @description
#' The layout functions help to streamline displaying text and labels *geoms* without overlapping effectively leveraging space available for pie and donut charts
#' * `tv()` - The function builds layout resembled an old-fashioned TV screen
#' * `petal()` - The function builds layout resembled flower with petals
#' * `circle()` - The function builds circle layout
#'
#' @param scale_x Scales the layout in horizontal perspective
#' @param scale_y Scales the layout in vertical perspective
#' @param bend_x Adjusts the bend level in horizontal perspective
#' @param bend_y Adjusts the bend level in vertical perspective
#' @param thinner Distributes text or label elements across two different levels
#' @param thinner_gap Sets the spacing between thinner levels
#'
#' @return Layout functions return layout function i.e. a function that takes a vector of angles and returns a numeric
#' radius vector giving a position for each input value.
#'
#' Layout functions are designed to be used with the layout argument of donutsk functions.
#'
#' @name layouts
NULL
#> NULL
#'
#'
#' @examples
#' # Render multiple layouts simultaneously
#' list(petal_2n = petal(n = 2),
#'      petal_3n = petal(n = 3, rotate = 180),
#'      petal_4n = petal(n = 4),
#'      tv_base = tv(),
#'      tv_ext = tv(bend_x = 0, bend_y = 0, thinner = TRUE)) |>
#'   lapply(function(x){
#'     rlang::exec(x, 1:300/300) |>
#'       dplyr::tibble(r = _) |>
#'       dplyr::mutate(theta = 1:300/300)
#'   }) |>
#'   dplyr::bind_rows(.id = "layouts") |>
#'   ggplot(aes(x=r, y=theta, col = layouts)) +
#'   geom_point(alpha = .3) +
#'   coord_polar(theta = "y") +
#'   xlim(0, 3.5)
#'
#' @seealso Utilized in the following functions: [geom_label_ext], [geom_text_ext], [geom_pin]
#'
#' @rdname layouts
#' @export
tv <- function(scale_x = 1.5, scale_y = 1.5, bend_x = 1, bend_y = 1,  thinner = FALSE, thinner_gap = .1){
  force(scale_x)
  force(scale_y)
  force(bend_x)
  force(bend_y)
  force(thinner)
  force(thinner_gap)

  function(theta){
    if(thinner){
      if_else(between(theta, 0, .125)|between(theta, .375, .625)|between(theta, .875, 1),
             bend_y + scale_y*sqrt((tan(2*pi*theta))^2+1),
             bend_x + scale_x*sqrt((1/tan(2*pi*theta))^2+1)) +
        c(rep(c(-thinner_gap, thinner_gap), length(theta)%/%2), thinner_gap)[seq_along(theta)]
    }
    else if_else(between(theta, 0, .125)|between(theta, .375, .625)|between(theta, .875, 1),
                bend_y + scale_y*sqrt((tan(2*pi*theta))^2+1),
                bend_x + scale_x*sqrt((1/tan(2*pi*theta))^2+1))
  }
}
#'
#' @param rotate Rotates the layout clockwise
#' @param n Sets the number of petals in the layout
#' @param scale Scales the layout
#' @param bend Manages the bending level
#'
#' @rdname layouts
#' @export
petal <- function(rotate = 0, n = 4, scale = 1.5, bend=.3, thinner = FALSE, thinner_gap = .1){
  force(rotate)
  force(n)
  force(bend)
  force(scale)
  force(thinner)
  force(thinner_gap)

  function(theta){
    if(thinner){
      scale+bend*sinpi(theta*2*n-0.5-rotate*n/180) +
        c(rep(c(-thinner_gap, thinner_gap), length(theta)%/%2), thinner_gap)[seq_along(theta)]
    }else scale+bend*sinpi(theta*2*n-0.5-rotate*n/180)
  }
}
#'
#' @param r Sets the radius of the layout circle
#'
#' @rdname layouts
#' @export
circle <- function(r=2.25, thinner = FALSE, thinner_gap = .1){
  function(theta){
    if(thinner){
      r + c(rep(c(-thinner_gap, thinner_gap), length(theta)%/%2), thinner_gap)[seq_along(theta)]
    }else r
  }
}
