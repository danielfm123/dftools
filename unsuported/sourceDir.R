#' Sources R files in a Directory.
#'
#' Reads R codes from a directory with its path. This is a directory version of source function from base.
#' @param path character with the directory's path from we want to read R files.
#' @param encoding character vector. The encoding(s) to be assumed.
#' @param exclude character vector. Files we don't want to be readed as "filename.R".
#' @param ... All other parameters that are used in source function.
#' @usage sourceDir(path, encoding= "UTF-8", exclude= c(), ...)
#' @seealso Source function's dodumentation for more details, using help(source).
#' @author Daniel Fischer
#' @export
sourceDir = function(path,
                     encoding = "UTF-8",
                     exclude = c(),
                     ...) {
  if (!grepl("[/|\\]$", path)) {
    path = paste0(path, "/")
  }
  all_files = dir(path, "*.R$", include.dirs = F,recursive = T)
  lapply(setdiff(all_files, exclude),
         function(x) {
           print(x)
           source(paste0(path, x), encoding = encoding, ...)
         })
  return(TRUE)
}
