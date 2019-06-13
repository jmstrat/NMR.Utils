#' Pseudo Voigt curve
#'
#' This function defines the pseudo voigt curve.
#' @param x A vector of x coordinates
#' @param height The height parameter
#' @param centre The centre parameter
#' @param hwhm The hwhm parameter
#' @param shape The shape parameter (0--1)
#' @return The value(s) of the pseudo voigt function evaluated with the specified parameters
#' @export
#' @examples
#' pseudoVoigt(10, 10, 0, 2, 0.5)
#' pseudoVoigt(c(1:10), 10, 0, 2, 0.5)
#'
#' f <- function(x) {
#'   pseudoVoigt(x, height=10, centre=0, hwhm=2, shape=0.5)
#' }
#' curve(f, from=-5, to=5)
pseudoVoigt <- function(x, height, centre, hwhm, shape) {
  height * ((1 - shape) * exp(-log(2) * ((x - centre) / hwhm)^2) + shape / (1 + ((x - centre) / hwhm)^2))
}
