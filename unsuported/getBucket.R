
#' Returns the team
#'
#' Returns the team
#' @author Daniel Fischer
#' @export

getBucket = function(){
  tryCatch({
      bucket = suppressWarnings({readLines("~/.bucket")[1]})
      return(bucket)
    },error = function(e) {
      stop("Please setup your bucket using workspaces application")
    })
}