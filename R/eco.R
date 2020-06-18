#' Adds lags of a funcion
#'
#' takes an array and sums n displaced versions of it. Each k-displaced version of the array got zeros on the first k positions.
#' k takes values from 1 to n
#' @param x an array
#' @param n an integer >0
#' @usage eco(x,n)
#' @return An array with same length as x.
#' @author Daniel Fischer
#' @example examples\ecoex.R
#' @export
eco = function(x, n) {
  if (n < 1) {
    stop("the second parameter must be greater than 1")
  }
  if (length(x) == 1) {
    return(0)
  }
  rowSums(mapply(
    1:n,
    FUN = function(y)
      lagpad(x, y)
  ))
}
