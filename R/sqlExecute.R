#' excecutes query on SQL
#'
#' It uses dbExecute() function from DBI to execute a statement and return the number of rows affected
#' and provide the option to make general queries and change its parameters easier, as in sqlGsub() from
#' this package.
#' @param server_name character, name of the server where DB is alocated. It must be in sqlServers.
#' @param query character, query structure with variant parameters between @'s, if they are.
#' @param param character vector with the value of the parameters that would be used in query.
#' @param debug boolean to show excecuted query.
#' @param close boolean to force closing the connection after execution
#' @param ... inherit parameters used in dbExecute().
#' @usage sqlExecute(server_name, query, param=c(), ...)
#' @author Daniel Fischer
#' @details This function ends the connection with DB inmediately after it execute and update the statement.
#' sqlServers is a list that contain the expressions necessary to connect DB's.
#'
#' sqlServers is
#' a list built-in sqlGetConn().
#' @seealso "sqlServerConn","setSqlServers()" and "sqlGsub()" documentation in toolkitEntel and "dbExecute()" from DBI for more details.
#' @examples
#'   connection=sqlGetConn("local")
#'   dbWriteTable(connection, "cars",head(cars, 3))
#'   dbReadTable(connection, "cars")   # there are 3 rows
#'   dbDisconnect(connection) #end connection tu see the use of sqlExecute
#'   sqlExecute("local",
#'          "INSERT INTO cars (speed, dist) VALUES (1, 1), (2, 2), (3, 3)") #can omite "param" to make a simple query
#'
#'   connection=sqlGetConn("local")
#'   dbReadTable(connection, "cars")   # there are now 6 rows
#'   dbDisconnect(connection) #end connection again
#'
#'   q="INSERT INTO @table@ (@col1@, @col2@) VALUES (4, 0), (5, 0), (6, 0)" #use this form to make mutable queries
#'   p1=c(table="cars",col1="speed",col2="dist")
#'   p2=c(table="cars",col1="dist",col2="speed")
#'
#'   sqlExecute("local",q,p1)
#'
#'   connection=sqlGetConn("local")
#'   dbReadTable(connection, "cars")
#'   dbDisconnect(connection) #end connection again
#'
#'   sqlExecute("local",q,p2)
#'
#'   connection=sqlGetConn("local")
#'   dbReadTable(connection, "cars")
#'   dbDisconnect(connection) #end connection again
#'
#' @export
sqlExecute = function(server_name,query, param = c(),debug=F,close = T, ...){
  #try({
    sql = sqlGetConn(server_name)
    query = sqlGsub(query,param)
    if(debug){print(query)}
    dbExecute(sql,query,...)
    # dbCommit(poolCheckout(sql))
  #})
  if(any(c("expression","character") %in% class(server_name)) | close){sqlClose(sql)}
}
