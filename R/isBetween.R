#' Checks ifa value is between other 2
#'
#' Recieve one array and two numbers and find which elements of the array are between the two numbers.
#' Can also recieve three arrays of same length and compares each element of one
#' with other two's same posistion elements to check if the first one is between these two last.
#' @param x array with elements to check. Don't use List nor Dataframes.
#' @param a value or array with elements to use as lower bound. Don't use List nor Dataframes.
#' @param b value or array with elements to use as upper bound. Don't use List nor Dataframes.
#' @param ... array of "nulls". It takes NA, Inf, -Inf, NULL and NaN as "nulls" by default.
#' @usage isBetween(x,a,b,...)
#' @details All "nulls" in a or b would be replaced by -Inf or Inf respectevely to be compared.
#' @return Returns a logical array with TRUE for "i" positions where a<=x[i]<=b (or a[i]<=x[i]<=b[i],
#'  if a and b are arrays).
#' @example examples\isBetweenex.R
#' @author Daniel Fischer
#' @export
isBetween <-
  function(x, a, b, ...) {
    x >= replaceNull(a, -Inf, ...) & x <= replaceNull(b, Inf, ...)
  }
