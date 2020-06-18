#' Replace Null values with new value
#'
#' Recieve an array and replaces the "nulls" elements of it by a replacement.
#' @param x array. Don't use List nor Dataframes.
#' @param replacement value or array to replace nulls.
#' @param ... array of "nulls". It takes NA, Inf, -Inf, NULL and NaN as "nulls" by default.
#' @usage replaceNull(x,replacement,...)
#' @details If replacement is an array with length L shorter than x's length, the funciton would replace the null on the position
#' K*L+i of x, by replacement's i-element. This means the function would go through both arrays at same, repeating the replacement till the end
#' @return Returns an array with nulls replaced.
#' @author Daniel Fischer
#' @example examples\replaceNullex.R
#' @export
replaceNull = function(x, replacement, ...) {
  if (is.data.frame(x) | is.list(x)) {
    stop("the input cannot be a list")
  }
  ifelse(isNull(x, ...), replacement, x)
}
