#' cuts continuous variable into quantiles
#'
#' Takes an array of values and assign Quantiles to each one.
#' @param values array of values. The values must be able to rank.
#' @param n number of Quantiles to make the assignment. Takes n=100 by default to assign a percentile to every value.
#' @usage toQuantiles(values,n)
#' @return It returns a List of factors with the percentile of cut of each Quantile.
#' @author Daniel Fischer
#' @example examples\toQuantilesex.R
#' @export
toQuantiles = function(values, n = 100) {
  score = rank(values, ties.method = "random") / length(values)
  cortes = seq(from = 0,
               to = 1,
               length = n + 1)
  cut(score,
      cortes,
      include.lowest = T,
      labels = paste(cortes[-1] * 100, "%"))
}
