#' gets connection from sqlServers
#' 
#' @title SQL server connection
#' @description  This functions allow to work with DB in differents servers ending connections after every interaction.
#' @param server_name character, name of the server to connect, in sqlServer list.
#' @return \code{sqlGetConn()} set the connection by evaluate the expression on List "sqlServers".
#' @return \code{sqlListTable()} returns a character vector with names of every table in database.
#' @return \code{sqlClose()} disconnect from database.
#' @details  sqlServers is
#' a list built-in sqlGetConn().
#' @examples
#'   sqlGetConn(local) #connect with "local" in the list sqlServers.
#'   sqlListTable(local) # its empty
#'   sqlClose(local) ## end connection


#' @author Daniel Fischer
NULL

#' @rdname SQLserverconnection
#' @export
sqlGetConn = function(server_name){

  # tryCatch({
    if("expression" %in% class(server_name)){
      connObj = eval(server_name)
    }else{
      connObj = server_name
    }
  # },error = function(e){
    # stop("Cant connect to Database, wrong login parameters")
  # })

  
  if(version$os == "mingw32"){

  }else{
    # sqlQuery(connObj,"SET ANSI_PADDING ON")
    # sqlQuery(connObj,"SET ANSI_WARNINGS ON")
    # sqlQuery(connObj,"SET ANSI_NULLS ON")
  }
  return(connObj)
}




