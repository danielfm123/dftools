#' Creates a funcion to convert an empiric distribution into other
#' 
#' @title Distribution Conversor
#' @description Convert from a distribution to other by equaling the probabilities of  quantiles for each one. 
#' @param distribution distribution made by \code{makeDistribution()} or a data sample to make a distribution of it. Function 
#' \code{toDistribute()} only admit data samples, no distributions.  
#' @param objectiveDistribution Quantile function of the distribution to convert in (\code{qnorm,qexp,qunif,}...)
#' @param ... parameters to pass to \code{objectiveDistribution}. 
#' @details This function bassically compose functions by probability like Quantile2(probability(Q)),
#'  where Q is a quantile from the original distribution and Quantile2 gives the quantile in objective distribution that 
#'  have the same cumulative probability of Q. It uses the function \code{makeDistribution} form this package.
#' @return \code{makeDistributionConversor()} Returns a function that recieve a quantile vector and returns a vector with quantiles that share the same cumulative probability in the objective distribution with the input vector.\cr
#' \cr \code{toDistribution()} apply that function so it returns the converted vector without keeping the function to convert.
#' @examples 
#' dist=rnorm(10000) # take normal distribution sample
#' conversor=makeDistributionConversor(dist,objectiveDistribution=qexp) #and declare the conversor to exp distribution
#' 
#' # Now prove it with a known quantile 
#' pnorm(0)
#' qexp(pnorm(0)) # same probability so this would  be the conversion
#' conversor(0)
#' #and use it to convert the entire sample
#' head(conversor(dist))
#' 
#' 
#' head(toDistribution(dist,objectiveDistribution=qexp)) # same but without saving the function conversor
#' @author Daniel Fischer
NULL
   
   
#' @rdname  DistributionConversor  
#' @export
makeDistributionConversor = function(distribution, objectiveDistribution = qnorm,...){
  if(class(distribution) != "list"){
    distribution = makeDistribution(distribution)
  }
  fn = function(q){
    objectiveDistribution(distribution$pfun(q),...)
  }
  return(fn)
}

#' @rdname  DistributionConversor  
#' @export
toDistribution = function(distribution, objectiveDistribution = qnorm,...){
  distribution.conversor = makeDistributionConversor(distribution,objectiveDistribution,...)
  distribution.conversor(distribution)
}