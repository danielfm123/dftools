#' @rdname SQLserverconnection
#' @export
sqlListTables = function(server_name){
  try({
    sql = sqlGetConn(server_name)
    tablas = dbListTables(sql)
  })
  if(class(server_name) == "character"){sqlClose(sql)}
  return(tablas)
}
