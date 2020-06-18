#' Truncates number of categories o a vector
#'
#' Limits the number of categoryes on a vector, it selects the most frecuent categories.
#' @param vector vector with character values
#' @param max_levels number of categories minuts the "other" category
#' @param category for the "other" category
#' @examples 
#' vector = sample(letters[1:10],100,replace = T)
#' limitLevels(vector,5)
#' 
#' @author Daniel Fischer
#' @export

limitLevels = function(vector, max_levels, other_name = "other"){
  vector = as.character(vector)
  freq = sort(table(vector),decreasing = T)
  selected = freq[1:min(max_levels, length(freq))]
  vector[!vector %in% names(selected)] = other_name
  vector = as.factor(vector) 
  
  return(vector)
}


