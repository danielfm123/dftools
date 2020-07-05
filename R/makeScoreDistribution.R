#' Provides Score Distribution interpoles
#' 
#' Calculates the lift curve based on a score(numeric 0-1) and responce(TRUE/FALSE) vector
#' @param score a numeric vector with the scores from the prodictive model
#' @param response a boolean (TRUE or FALSE) vector with the real values from the dataset
#' @param method method used to interpolate scores between groups, approx or spline
#' @param groups number of percentiles, default 100
#' @param ... passed to aproximation method
#' @return data.frame with the values of the Lift curve.
#' score = runif(1000)
#' response = (score + rnorm(1000,0,0.1)) > 0.5
#' fn = makeScoreDistribution(score,response,approx)
#' fn(0.5)
#' 
#' @author Daniel Fischer
#' @export
makeScoreDistribution = function(score, response, method = 'approx',groups = 100,...){
  lift = getLift(score,response,groups)
  
  x = c(1,lift$mean_score,0)
  y = c(first(lift$response_rate),lift$response_rate,last(lift$response_rate))
  
  if(method == 'approx'){
    fn = approxfun(x,y,...)
  }else{
    fn = splinefun(x,y,...)
  }

  
  return(fn)
}