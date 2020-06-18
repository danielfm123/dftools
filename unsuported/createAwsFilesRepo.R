
#' Create folder for proyect files
#'
#' Create folder for proyect files
#' @param repo_folder folder where repository is mounted
#' @param link_files should the repository be linked after beeing created?
#' @example createAwsFilesRepo()
#' @export
#' @author Daniel Fischer

createAwsFilesRepo = function(path = "files_project"){
  safeLibrary(aws.s3)
  bucket = paste0(getBucket())
  
  current_repo = basename(getwd())
  value = aws.s3::put_folder(paste(path,current_repo,"files",sep='/'),bucket, use_https = F)
  # toS3(bucket=bucket,path=path)
  return(value)
}