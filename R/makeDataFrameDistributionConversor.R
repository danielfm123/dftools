#' Applies redistribution to a dataframe
#' 
#' Make distribution from columns of a data frame and passes every one to other 
#'  distribution by equaling the probabilities of a quantile. 
#' @usage makeDataFrameDistributionConversor(dataset,
#' columns = colnames(dataset),
#' objectiveDistribution = qnorm,
#' ...)
#' @param dataset dataframe with the columns of data samples to make a distribution of.
#' @param columns character vector with the names of the columns to make a distribution of.
#' @param objectiveDistribution Quantile function of the distribution to convert in (qnorm,qexp,qunif,...)
#' @param ... parameters to pass to \code{objectiveDistribution}. 
#' @author Daniel Fischer
#' @details This function bassically apply \code{makeDistributionConversor()} to selected columns.
#' @seealso \code{makeDistributionConversor} documentation from this package.
#' @return Returns a function that recieve a data frame and returns a data frame with the selected column converted to columns that share
#' the same cumulative probability in the objective distribution for every row.
#' @example examples\MDFDCex.R 
#'
#' @export
makeDataFrameDistributionConversor = function(dataset,columns = colnames(dataset),objectiveDistribution = qnorm,...){
  conversors = lapply(columns,function(x) makeDistributionConversor(dataset[,x],objectiveDistribution) )
  names(conversors) = columns

  convert = function(x){
    for(col in columns){
      x[,col] = conversors[[col]]( x[,col])
    }
    return(x)
  }
  
  return(convert)
}
