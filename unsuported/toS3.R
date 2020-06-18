
#' sync to S3
#'
#' synchronizes to s3 proyect
#' @param delete shoud file be deleted on s3?
#' @param bucket bucket name
#' @param path path to repository
#' @usage toS3()
#' @return None
#' @export
#' @author Daniel Fischer

toS3 = function(delete = F,path = "files_project",...){
  safeLibrary("rstudioapi")
  bucket = getBucket()
  #createAwsFilesRepo(path = path)
  
  current_repo = basename(getwd())
  param = c(proj = current_repo, 
            del = ifelse(delete,' --delete',''),
            bucket = bucket,
            path = path)
  cmd = sqlGsub("aws s3 sync ./files/ s3://@bucket@/@path@/@proj@/files @del@",param)
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