#' Sets the categories of a dataset given a sample
#'
#' Any category not included in level_values, will be classified as other_name, 
#' if the provided level_values is a factor type, 
#' it will use the levles(level_values) as the list of categoryes
#' @param vector vector with character values
#' @param level_values sample with avivable categories
#' @param category for the "other" category
#' @examples 
#' vector = sample(letters[1:10],100,replace = T)
#' limitLevels(vector,c("a","b","b"))
#' 
#' @author Daniel Fischer
#' @export

setLevels = function(vector, level_values,other_name = "other"){
  if(class(level_values) == "factor"){
    niveles = levels(level_values)
  }else{
    niveles = unique(level_values)
  }
  
  vector[!vector %in% niveles] = other_name
  vector = as.factor(vector) 
  
  return(vector)
}