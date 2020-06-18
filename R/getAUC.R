#' Calculate The AUC from a ROC (getROC)
#' 
#' Calculates the AUC
#' @rocs named list of ROC calculated with getROC or single ROC calculated with getROC
#' @... mode results  from getROC
#' #' score1 = runif(1000)
#' response1 = (score1 + rnorm(1000,0,0.1)) > 0.5
#' ROC1 = getROC(score1,response1)
#' score2 = runif(1000)
#' response2 = (score2 + rnorm(1000,0,0.3)) > 0.5
#' ROC2 = getROC(score2,response2)
#' rocs = list(model1 = ROC1, model2 = ROC2)
#' getAUC(rocs)
#' #or
#' getAUC(model1 = ROC1, model2 = ROC2)
#' # or both
#' getAUC(rocs,model3 = ROC1, model4 = ROC2)
#' 
#' @author Daniel Fischer
#' @export
getAUC = function(rocs,...){
  if(class(rocs) != "list"){
    rocs = list(model = rocs)
  }
  extra_rocs = list(...)
  for(model in names(extra_rocs)){
    rocs[[model]] = extra_rocs[[model]]
  }
  suppressWarnings({
    dataset = map2_dfr(names(rocs), rocs, function(model, values){
      data.frame(
        model = model,
        AUC = sum( ( values$true_positive[-1] + values$true_positive[-nrow(values)] ) / 2 * diff(values$false_positive))
      )
    })
  })
  
  return(dataset)
}