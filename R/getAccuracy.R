#' Calculates the accuracy
#'
#' Calculates the accuracy of a prediction
#' @param score prediction scores or probabilities to evaluate.
#' @param response Boolean vector of real values.
#' @param unbral cut point for scores, check maxPrecision function.
#' @usage getAcuracy(score, response, 0.5)
#' @return Returns the accuracy
#' @export
#' @author Daniel Fischer

getAccuracy = function(score, response, umbral = 0.5){
  aux = getConfusion(score,response,umbral,asMatrix = F) %>% 
    filter(prediction == response)
  return(sum(aux$portion))
}
