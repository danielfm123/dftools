#' Samples dataframe
#' 
#' Extract a mber of rows randomly from a fatafram
#' @param dataset dataset to be sampled
#' @param sampleSize size of the sample
#' 
#'  @export


sampleDataset = function(dataset, sampleSize){
  aleatoreo = rank(runif(nrow(dataset)))
  return(dataset[aleatoreo <= sampleSize,])
}