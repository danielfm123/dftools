#' saves dataframe on SQL row by row
#'
#'saves dataframe on SQL row by row
#'It calls dbWriteTable().
#'@usage sqlWriteTable(server_name, data, table, ...)
#'@param server_name character, name of the DB server from sqlServers list.
#'@param data a data frame or coercible to data frame
#'@param table character, name of the table to write in DB.
#'@param close boolean to force closing the connection after execution
#'@details It ends the connection inmediately after getting the results. sqlServers is
#'a list built-in sqlGetConn().
#'@seealso "sqlServerConn" ocumentation in dftools and "dbWriteTable()"
#'from DBI for more details.
#'@return returns booleans as it is specificated for dbWriteTable().
#'@examples
#'sqlInsert(local,head(iris),"iris")
#'sqlInsert(local,head(mtcars),"MTCARS")
#'@export
sqlInsert = function(server_name,data,table,close = T){
  values = sql_script_insert(data,table)
  
  # try({
    sql = sqlGetConn(server_name)
    # print("insertando registros en tabla")
    map(values,function(x) try(dbExecute(sql,x)))
  # })
  
  if(any(c("expression","character") %in% class(server_name)) | close){sqlClose(sql)}
}