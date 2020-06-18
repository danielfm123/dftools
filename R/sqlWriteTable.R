#' saves dataframe on SQL
#'
#'Writes, overwrite or appends a data frame to a database table.
#'It calls dbWriteTable().
#'@usage sqlWriteTable(server_name, data, table, ...)
#'@param server_name character, name of the DB server from sqlServers list.
#'@param data a data frame or coercible to data frame
#'@param table character, name of the table to write in DB.
#'@param bulk boolean, in order to excecute in bulk mode or not (bulk does not use autoincrementals, but is faster)
#'@param ... other parameters passed on to methods.
#'@details It ends the connection inmediately after getting the results. sqlServers is
#'a list built-in sqlGetConn().
#'@seealso "sqlServerConn" and "setSqlServers()" documentation in toolkitEntel and "dbWriteTable()"
#'from DBI for more details.
#'@return returns booleans as it is specificated for dbWriteTable().
#'@examples
#'sqlWriteTable("local",head(iris),"iris")
#'sqlWriteTable("local",head(mtcars),"MTCARS")
#'con=sqlGetConn("local")
#'dbReadTable(con,"iris")
#'dbReadTable(con,"MTCARS")
#'
#'@export
sqlWriteTable = function(server_name,data,table,bulk = T,...){
  try({
    sql = sqlGetConn(server_name)
    if(bulk){
      dbWriteTable(sql, table, data, append = TRUE,row.names = F,...) 
    }else{
      query = sqlAppendTable(ANSI(),table,data,row.names = F)
      dbExecute(sql,query)
    }
  })
  if(any(c("expression","character") %in% class(server_name)) & !"Pool" %in%  class(sql)){sqlClose(sql)}
}
