#' Link Files folder
#'
#' Move files to Blob
#' @export
#' @author Daniel Fischer

toBlob = function(delete = F,account="datarts",container="proyects",path = "files_project"){
  safeLibrary("rstudioapi")
  #createAwsFilesRepo(path = path)
  
  current_repo = basename(getwd())
  param = c(proj = current_repo, 
            del = ifelse(delete,'--delete-destination true','--delete-destination false'),
            account = account,
            container = container,
            path = path)
  cmd = sqlGsub('azcopy sync "./files/" "https://@account@.blob.core.windows.net/@container@/@path@/@proj@/files" @del@',param)
  print(cmd)
  terminals = terminalList()
  if(length(terminals) == 0){
    error("Please open a terminal")
  }
  term = terminals[1]
  terminalActivate(term)
  terminalSend(term,paste0(cmd,"\r\n"))
}