#' Predict K-means from kmeans function
#' 
#' Predict with k-means method.
#' @usage predictKmeans(fit,dataset,escalator = NULL, segment.names = NULL)
#' @param fit fitting
#' @param dataset Data array 
#' @param escalator escalator, \code{NULL} by default
#' @param segment.names segments' names
#' @details This function has no documentation yet 
#' @author Daniel Fischer
#' @export
predictKmeans =  function(fit,dataset,escalator = NULL, segment.names = NULL){
  safeLibrary(class)
  if (is.null(escalator)) {
    dataset.kmeans = dataset
  }
  else{
    dataset.kmeans = escalator$scale(dataset)
  }
  assigned.segment = as.numeric(
    as.character(
      knn1(fit$centers,dataset.kmeans,1:nrow(fit$centers))
      )
    )
  
  if(is.null(segment.names)){
    return(assigned.segment)
  }else{
    return(segment.names[assigned.segment])
  }
}
