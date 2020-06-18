#' Gets result from sql query
#'
#'Does query to a DB in a server from sqlServers and return its result as a data frame or data table.
#'It calls dbGetQuery() from DBI package so the result is always free-d. Provide the option
#'to make general queries and change its parameters easier, as in sqlGsub() from
#' this package.
#'@details It ends the connection inmediately after getting the results. sqlServer is
#'a list built-in sqlGetConn().
#'@seealso "sqlServerConn","setSqlServers()" and "sqlGsub()" documentation in toolkitEntel and "dbGetQuery()"
#'from DBI for more details.
#'@param server_name character, name of the DB server from sqlServers list.
#'@param query character, query structure with variant parameters between @'s, if they are.
#'@param param character vector with the value of the parameters that would be used in query.
#'@param dt boolean, If true, the results would be data table class.
#'@param key character vector, it contains the names of the columns to be used as keys if return a data table.
#'@param ... inherit parameters used in dbGetQuery().
#'@usage sqlGetQuery(server_name, query, param=c(), dt=FALSE, key=c(), ...)
#'@return A data frame or data table with query results.
#'@examples
#'sqlWriteTable("local",mtcars,"mtcars") #create mtcars table in local server
#'sqlListTables("local")
#'q1="SELECT * FROM mtcars WHERE mpg>20" #decalrate query
#'
#'sqlGetQuery("local",q1) #do a simple query
#'
#'q2="SELECT @col@ FROM @table@ WHERE @condition@" #decalrate a mutable query
#'p1=c(table="mtcars",col="*",condition="hp>100")
#'p2=c(table="mtcars",col="wt",condition="wt>2.600") #and declarate the parameter to use in mutable query
#'
#'sqlGetQuery("local",q2,p1)
#'
#'sqlGetQuery("local",q2,p2,dt=T) # as data table
#'@export
sqlGetQuery = function(server_name,query, param = c(),dt=FALSE,key=c(),  ...){
  try({
    sql = sqlGetConn(server_name)
    query = sqlGsub(query,param)
    # print(query)
    rowset = dbGetQuery(sql,query,...)
  })

  if(any(c("expression","character") %in% class(server_name)) & !"Pool" %in%  class(sql)){sqlClose(sql)}

  if(dt){
    safeLibrary("data.table")
    rowset = as.data.table(rowset)
    setkeyv(rowset,key)
    return(rowset)
  }
  else{
    return(rowset)
  }
}
