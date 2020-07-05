#' Provides Score Distribution interpoles
#' 
#' Calculates the lift curve based on a score(numeric 0-1) and responce(TRUE/FALSE) vector
#' @param score a numeric vector with the scores from the prodictive model
#' @param response a boolean (TRUE or FALSE) vector with the real values from the dataset
#' @param method method used to interpolate scores between groups, approx for approxfun or spline for splinefun
#' @param groups number of percentiles, default 100
#' @param ... passed to aproximation method
#' @return function to aproximate probabilities based on score
#' score = runif(1000)
#' response = (score + rnorm(1000,0,0.1)) > 0.5
#' fn = makeScoreDistribution(score,response,methond = 'approx')
#' fn(0.5)
#' 
#' @author Daniel Fischer
#' @export
makeScoreDistribution = function(score, response, method = 'approx',groups = 100,...){
  lift = getLift(score,response,groups)
  
  x = lift$mean_score
  y = lift$response_rate
  
  min_x = last(lift$mean_score)
  min_y = last(lift$response_rate)
  
  max_x = first(lift$mean_score)
  max_y = first(lift$response_rate)

  suppressWarnings({
    if(method == 'approx'){
      approximation = approxfun(x,y,...)
    }else if(method == 'spline'){
      approximation = splinefun(x,y,...)
    }else{
      stop('not valid method')
    }
  })

  fn = function(.x){
    estimate = approximation(.x)
    estimate = ifelse(.x < min_x, min_y, estimate)
    estimate = ifelse(.x > max_x, max_y, estimate)
    return(estimate)
  }
  
  return(fn)
}

