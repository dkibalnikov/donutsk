#' The World Bank GDP, PPP (current international $)
#'
#' * A pre-processed subset of GDP data from the World Bank
#' * GDP, PPP means gross domestic product based on purchasing power parity
#' * *current international $* means actual (not adjusted to inflation) US dollars
#'
#' @format ## `GDP`
#' A data frame with 6,004 rows and 5 columns:
#' \describe{
#'   \item{date}{Year}
#'   \item{country}{Country name}
#'   \item{region}{Region hierarchy}
#'   \item{region_ISO}{3 letter ISO region codes}
#'   \item{GDP}{GDP, PPP (current international $)}
#'   ...
#' }
#' @source <https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.CD>
"GDP"
