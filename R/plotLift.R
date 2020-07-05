#' Plots lifts from getLift function
#' 
#'  Plots lifts ffrom getLift function
#'  @param lifts named list with lifts from getLift
#'  @param ... lifts from getLift
#'  @param cuts points to show exact value of lifts
#'  @param value column to be ploted, cum_lift, lift, cum_response, response
#'  @param independient independient value of the plot, can be 'percentil' or 'mean_score'
#'  @param level_line should shouw the average responce rate? TRUE or FALSE
#'  
#' @examples 
#' score1 = runif(1000)
#' response1 = (score1 + rnorm(1000,0,0.1)) > 0.5
#' lift1 = getLift(score1,response1)
#' score2 = runif(1000)
#' response2 = (score2 + rnorm(1000,0,0.3)) > 0.5
#' lift2 = getLift(score2,response2)
#' lifts = list(model1 = lift1, model2 = lift2)
#' plotLift(lifts)
#' #or
#' plotLift(model1 = lift1, model2 = lift2) 
#' 
#' @author Daniel Fischer
#' @export


plotLift = function(lifts = list(),
                    ...,
                    cuts = c(5,15), 
                    value = 'response_rate', 
                    independient = 'percentil',
                    level_line = T){
  # library(purrr)
  # library(ggplot2)
  # library(gridExtra)
  
  if(class(lifts) != "list"){
    lifts = list(model = lifts)
  }
  extra_lifts = list(...)
  for(model in names(extra_lifts)){
    lifts[[model]] = extra_lifts[[model]]
  }
  
  dataset = map2_dfr(names(lifts), lifts, function(model,values){
    values$model = model
    # values[,"value"] = values[,value]
    return(values)
  })
  
  hline = switch (value,
                  cum_lift = 1,
                  lift = 1,
                  response_rate = with(dataset,sum(frequency*response_rate)/sum(frequency)),
                  cum_response = with(dataset,sum(frequency*response_rate)/sum(frequency))
  )
  
  gr = ggplot(dataset,aes_string(independient, value,color = "model")) + 
    geom_line(size = 1.5) + 
    ylab(value)
  
  if(level_line){
    gr = gr + geom_abline(slope=0, intercept=hline,size=1.5)
  }
   
  
   if(length(cuts) > 0 & independient == 'percentil'){
     gr = gr + geom_vline(xintercept = cuts)
     tabla = filter(dataset,percentil %in% cuts) 
     gr = gr + geom_label(aes(label = round(tabla[[value]],2),fill=model),tabla,color = "black")
   }
   
   return(gr)
}

