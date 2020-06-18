#' Link Files folder
#'
#' Link Files
#' @param repo_folder folder where repository is mounted
#' @export
#' @author Daniel Fischer

linkFiles = function(){
  if(version$os == "mingw32"){
    print('linux AWS only function')
    return(FALSE)
  }
  repo_folder = paste0("/mnt/",getBucket(),"/files_project/")
  current_repo = basename(getwd())
  avivable_repos = basename(list.dirs(repo_folder,recursive = F))
  if(current_repo %in% avivable_repos){
    print("Repository avivable")
    if(file.exists("files")){
      print("files already exists, please remove it first")
    }else{
      file.symlink(paste0(repo_folder,current_repo,"/files"), "./files")
    }
  }else{
    print("repository does not exist or run toS3() to create repo structure")
  }
  
}