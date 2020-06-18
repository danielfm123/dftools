#' Sets response rate for dataset
#' 
#' @param dataset input dataframe
#' @param response_variable column to be resampled
#' @param ... porportional number of cases per value of response_variable column
#' 
#' @examples 
#' setResponseRate(mtcars,"cyl", "4" = 4, "6" = 1, "8" = 1)
#' 
#' @author Daniel Fischer
#' @export

setResponseRate = function(dataset, response_variable, ...){
  safeLibrary(dplyr)
  safeLibrary(purrr)
  
  rates = data.frame(col = names(...), rate = c(...))

  freq = data.frame(col = as.character(dataset[,response_variable])) %>% 
    group_by(col) %>% 
    summarise(freq = n()) %>% 
    left_join(rates) %>% 
    ungroup() %>% 
    mutate(ratio = freq/rate,
           muestra = floor(min(ratio)*rate))
  
  splitted_dataset = split(dataset,dataset[,response_variable])
  
  map2_dfr(freq$col,freq$muestra, function(grupo, numero){
    sampleDataset(splitted_dataset[[grupo]],numero)
  })
  
}
