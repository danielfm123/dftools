#' Check if value is not valid, it considers Inf, NA, Null NaN
#'
#' Recieve an array and matches its elements with arbitrary "nulls".
#' @param x array  with the elements to match. Don't use List nor Dataframes.
#' @param nulls array of "nulls" to be matched with. It takes NA, Inf, -Inf, NULL and NaN as "nulls" by default.
#' @usage isNull(x,nulls)
#' @return A logical vector with True for the nulls elements' positions. Even if x is a matrix, the return value would be a vector.
#' @example examples\isNullex.R
#' @author Daniel Fischer
#' @export
isNull = function(x,
                  nulls = c(NA, Inf,-Inf, NULL, NaN)) {
  if(is.data.frame(x)|is.list(x)){stop("the input cannot be a list")}
  x %in% nulls
}
