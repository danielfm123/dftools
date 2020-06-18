#' Lags a vector
#'
#' Displaces an array the first or last k positions and fill the dislpaced empty spots with a specific value.
#' @param x array to be displaced.
#' @param k the number of positions to displace. If its negative it would displace from the end.
#' @param default value to fill the displaced empty spots. It takes 0 by default.
#' @usage lagpad(x,k,default)
#' @return Returns an array of same length as x, with x's values displaced k positions and the default values where its supposed to.
#' @example examples\lagpadex.R
#' @author Daniel Fischer
#' @export
lagpad <-
  function(x, k, default = 0) {

    if (k >= 0) {
      c(rep(default, k), x)[1:length(x)]
    }
    else{
      c(x, rep(default,-k))[(-k + 1):(length(x) - k)]
    }

  }
