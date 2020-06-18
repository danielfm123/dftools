#' calculate the mode
#' 
#' calculates the mode or most frequent value
#' @param values values
#' @param append_dv should the calculated DV be apended to the rut or just return the DV
#' 
#' @examples 
#' 
#' values = c(1,2,3,4,2,3,2,1)
#' getMode(values)
#' @export

getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


