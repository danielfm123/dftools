#' Calculates ROC Curve
#'
#' Calculates the ROC curve
#' @param score a numeric vector with the scores from the prodictive model
#' @param response a boolean (TRUE or FALSE) vector with the real values from the dataset
#' @param resolution maximum resolution for the table, maximum rows
#' @return dataframe with the values of the ROC curve.
#' @examples 
#' score = runif(1000)
#' response = (score + rnorm(1000,0,0.1)) > 0.5
#' ROC = getROC(score, response)
#' plot(ROC$false_positive,ROC$true_positive,type="l")
#' 
#' @export


getROC = function(score, response, resolution = 1000){
  suppressWarnings({
    dataset = data.frame(score, response)
    dataset$slice = cut(dataset$score,
                        unique(quantile(dataset$score,seq(from = 0,to = 1,length.out = min(resolution,nrow(dataset) ) + 1 ))),
                        include.lowest = T)
    dataset =  dataset %>% 
      group_by(slice) %>% 
      summarise(true_positive = sum(response),
                false_positive = sum(!response))
    dataset = dataset[order(dataset$slice,decreasing = T),]
    
    dataset = transform(
      dataset,
      true_positive = cumsum(true_positive) / sum(true_positive),
      false_positive = cumsum(false_positive) / sum(false_positive)
    )
  })
  dataset = bind_rows(
    data.frame(slice = '0', true_positive = 0, false_positive = 0),
    dataset %>% mutate(slice = as.character(slice))
  )
  return(dataset)
}