# Miscellaneous Functions
#' Robust Max, an alternative to max() that doesn't throw a warning
#'
#' @param x A vector to find the maximum value of
#'
#' @return The max value from a vector
#' @export

RobustMax <- function(x) {
  if (length(x) > 0) max(x) else -Inf
}

#' Robust Min, an alternative to max() that doesn't throw a warning
#'
#' @param x A vector find the minimum value of
#'
#' @return The minimum value from the vector
#' @export
#'
RobustMin <- function(x) {
  if (length(x) > 0) min(x) else Inf
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Please see the documentation on https://data.lesslikely.com/concurve/ or by typing `help(concurve)`")
}
