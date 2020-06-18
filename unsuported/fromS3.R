
#' sync from S3
#'
#' synchronizes from s3 proyect
#' @param delete shoud file be deleted localy?
#' @param bucket bucket name
#' @param path path to repository
#' @param ... passed to system
#' @usage fromS3()
#' @return None
#' @export
#' @author Daniel Fischer

fromS3 = function(delete = F,path = "files_project",...){
  safeLibrary("rstudioapi")
  suppressWarnings(dir.create('files'))
  bucket = getBucket()
  
  current_repo = basename(getwd())
  param = c(proj = current_repo, 
            del = ifelse(delete,' --delete',''),
            bucket = bucket,
            path = path)
  cmd = sqlGsub("aws s3 sync s3://@bucket@/@path@/@proj@/files ./files/ @del@",param)
  print(cmd)
  if(version$os == "mingw32"){
    terminals = terminalList()
    if(length(terminals) == 0){
      error("Please open a terminal")
    }
    term = terminals[1]
    terminalActivate(term)
    terminalSend(term,paste0(cmd,"\r\n"))
  }else{
    system(cmd,...)  
  }
}