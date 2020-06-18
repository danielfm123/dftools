#' plot ROC from getROC
#'
#' returns a ggplot object with the ROC for the provided models
#' @rocs named list of ROC calculated with getROC or single ROC calculated with getROC
#' @... mode results  from getROC
#' score1 = runif(1000)
#' response1 = (score1 + rnorm(1000,0,0.1)) > 0.5
#' ROC1 = getROC(score1,response1)
#' score2 = runif(1000)
#' response2 = (score2 + rnorm(1000,0,0.3)) > 0.5
#' ROC2 = getROC(score2,response2)
#' rocs = list(model1 = ROC1, model2 = ROC2)
#' plotROC(rocs)
#' #or
#' plotROC(model1 = ROC1, model2 = ROC2)
#' # or both
#' plotROC(rocs,model3 = ROC1, model4 = ROC2)
#' 
#' @author Daniel Fischer
#' @export

plotROC = function(rocs = list(),...,show_auc = T){
  
  if(class(rocs) != "list"){
    rocs = list(model = rocs)
  }
  extra_rocs = list(...)
  for(model in names(extra_rocs)){
    rocs[[model]] = extra_rocs[[model]]
  }
    
  ## Formato tidy
  dataset = map2_dfr(names(rocs), rocs, function(model,values){
    values$model = model
    return(values)
  })
  
  # graficar
  gr = ggplot(dataset,aes(false_positive, true_positive,color = model)) + 
    geom_line(size = 1.5) + 
    geom_abline(slope=1, intercept=0, size = 1.5)
  
  if(show_auc){
    auc_data = getAUC(rocs)
    gr = gr + 
      annotation_custom(tableGrob(auc_data,rows=NULL), xmin=0.5, xmax=1, ymin=0, ymax=0.5)
  }
  return(gr)
}


