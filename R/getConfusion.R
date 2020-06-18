#' Calculates the confussion matrix
#'
#' Calculates the confussion matrix
#' @param score prediction scores or probabilities to evaluate.
#' @param response Boolean vector of real values.
#' @param unbral cut point for scores, check maxPrecision function.
#' @param asMatric Should the function return a matrix object? otherwise will be a dataframe.
#' @param unbral should confussion matrix be retourned as percentages.
#' @usage getConfusion(score, response, 0.5)
#' @return RReturns the conffusin matrix
#' @export
#' @author Daniel Fischer


getConfusion = function(score, response, umbral = 0.5,asMatrix = TRUE, asPortion = TRUE){
  safeLibrary(tidyverse)
  
  confussion = data.frame(prediction = score > umbral, response = response) %>% 
              group_by(prediction,response) %>% 
              summarise(cases = n()) %>% 
              ungroup() %>% 
              right_join(crossing(prediction = c(T,F), response = c(T,F)),by = c("prediction", "response")) %>% 
              mutate(cases = replace_na(cases,0),
                     portion = cases/sum(cases)) 
  
  if(asMatrix){
    if(asPortion){
      tabla = select(confussion,-cases) %>% 
        spread(prediction,portion)
    }else{
      tabla = select(confussion,-portion) %>% 
        spread(prediction,cases)
    }
    tabla = as.data.frame(tabla)
    rownames(tabla) = tabla$response
    tabla$response = NULL
    return(tabla)
  }else{
    return(confussion)
  }
  
}