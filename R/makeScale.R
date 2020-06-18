#' Makes functions to scale (zscore) variables
#'
#' Normalize tables by columns with Z score. This function returns the mean value and standard deviation
#' of each column, and the functions to pass from the original table to a normilized one and vice versa.
#' @usage makeScale(table, cols= colnames(table), mean.function=mean, sd.function=sd)
#' @param table table that contain the data.
#' @param cols character vector with names of columns to normalize.
#' @param mean.function function to get the mean value to use in Z score.
#' @param sd.function function to get the standard deviation value to use in Z score.
#' @author Daniel Fischer
#' @return Returns a list that contain 4 elements, the functions scale(table) and unScale(table) which recieve a table an returns
#' a normalized or un-normalized one, a vector with the mean value of each column with its name and another vector with
#' the standard deviation value of each column with its name.
#' @example examples\makeScaleex.R
#' @export
makeScale = function(table,
                     cols = colnames(table),
                     mean.function = mean,
                     sd.function = sd) {
  cols_mean = apply(table[, cols], 2, mean.function)
  cols_sd = apply(table[, cols], 2, sd.function)
  names(cols_mean) = colnames(table[, cols])
  names(cols_sd) = colnames(table[, cols])

  scale = function(table_aux) {
    table_aux[, cols] = sweep(table_aux[cols], 2, cols_mean, "-")
    table_aux[, cols] = sweep(table_aux[cols], 2, cols_sd, "/")
    return(table_aux)
  }
  unScale = function(table_aux) {
    table_aux[, cols] = sweep(table_aux[cols], 2, cols_sd, "*")
    table_aux[, cols] = sweep(table_aux[cols], 2, cols_mean, "+")
    return(table_aux)
  }
  return(list(
    scale = scale,
    unScale = unScale,
    cols_mean = cols_mean,
    cols_sd = cols_sd
  ))
}
