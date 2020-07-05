#' Provides Lift Curve
#' 
#' Calculates the lift curve based on a score(numeric 0-1) and responce(TRUE/FALSE) vector
#' @param score a numeric vector with the scores from the prodictive model
#' @param response a boolean (TRUE or FALSE) vector with the real values from the dataset
#' @param groups number of percentiles, default 100
#' @return data.frame with the values of the Lift curve.
#' score = runif(1000)
#' response = (score + rnorm(1000,0,0.1)) > 0.5
#' lift = getLift(score,response)
#' head(lift)
#' tail(lift)
#' 
#' @author Daniel Fischer
#' @export
getLift = function(score, response, groups = 100){
  suppressWarnings({
    tasa_promedio = mean(response)
    dataset = data.frame(score, response)
    lift = dataset %>% 
      arrange(-score) %>% 
      mutate(percentil = cut(rank(score,ties.method = "first"),
                             quantile(rank(score,ties.method = "first"),seq(from = 0, to = 1, length.out = groups + 1)),
                             include.lowest = T,
                             labels = seq(from = 100, to = 1, length.out = groups))) %>%
      group_by(percentil) %>% 
      summarise(min_score = min(score),
                max_score = max(score),
                mean_score = mean(score),
                response_rate = mean(response),
                detected = sum(response),
                frequency = n()
      ) %>% 
      ungroup() %>% 
      mutate(lift = response_rate/tasa_promedio,
             percentil = as.numeric(as.character(percentil))) %>% 
      arrange(percentil) %>% 
      mutate(cum_response = cumsum(detected)/cumsum(frequency),
             cum_lift = cum_response/tasa_promedio)
  })

  
  return(lift)
}
