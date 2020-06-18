#' Snow Cluster
#' 
#' This function has no documentation yet
#' @usage snowCluster(n = parallel::detectCores(),scripts = c("./init.R"))
#' @param n  number of workers
#' @param scripts R scripts to execute in each worker 
#' @details this function execute this code:\cr
#'  \code{snowCluster = function(n = parallel::detectCores(),scripts = c("./init.R")){\cr
#' safeLibrary("doSNOW")\cr
#' cl = makeSOCKcluster(n,outfile="cl.txt")\cr
#' registerDoSNOW(cl)\cr
#' lapply(scripts, function(x) clusterCall(cl,source,x))\cr
#' return(cl)\cr
#' }} 
#' @author Daniel Fischer
#' @export
#' 
#' 
snowCluster = function(n = parallel::detectCores(),scripts = c("./init.R")){
  cl = makeSOCKcluster(n,outfile="cl.txt")
  registerDoSNOW(cl)
  lapply(scripts, function(x) clusterCall(cl,source,x))
  return(cl)
}