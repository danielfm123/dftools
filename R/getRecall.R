#' Calculates the Recall
#'
#' Calculates the Recall
#' @param score prediction scores or probabilities to evaluate.
#' @param response Boolean vector of real values.
#' @param unbral cut point for scores, check maxPrecision function.
#' @usage getRecall(score, response, 0.5)
#' @return Returns the recall
#' @export
#' @author Daniel Fischer

getRecall = function(score, response, umbral = 0.5){
  aux = getConfusion(score,response,umbral,asMatrix = T)
  return(aux["TRUE","TRUE"]/sum(aux[,"TRUE"]))
}
