#' Maximizes the accuracy
#'
#' Finds th optimal cut point in order to maximize the accuracy
#' @param score prediction scores or probabilities to evaluate.
#' @param response Boolean vector of real values.
#' @usage getAcuracy(score, response, 0.5)
#' @return returns a list with the optimization results, where metrics element has the report.
#' @export
#' @author Daniel Fischer


maxAccuracy = function(score, response){

  cortes = seq(from = 0, to = 1, by = 0.01)
  valores = data.frame(umbral = cortes,
                       accuracy = map_dbl(cortes,~getAccuracy(score,response,.)))
  umbral_max = valores$umbral[valores$accuracy == max(valores$accuracy)][1]
  return(list(
    gr = ggplot(valores,aes(umbral,accuracy)) + geom_line(),
    umbral = umbral_max,
    values = valores,
    confussion = getConfusion(score, response,umbral_max),
    metrics = data.frame(metric = c("accuracy","precision","recall","optimal_cut"),
                         value = c(max(valores$accuracy),
                                   getPrecision(score, response,umbral_max),
                                   getRecall(score, response,umbral_max),
                                   umbral_max
                                   ))
  ))
}