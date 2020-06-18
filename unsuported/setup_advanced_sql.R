sql_script = function(datos,tabla,accion,...){
  # print(datos)
  if(!is.null(datos) ){
    if(accion == "INSERT"){
      query = sql_script_insert(datos,tabla,...)
    }
    if(accion == "DELETE"){
      query = sql_script_delete(datos,tabla,...)
    }
    if(accion == "UPDATE"){
      query = sql_script_update(datos,tabla,...)
    }
  }else{
    query = ""
  }
  return(query)
}

sql_script_insert = function(datos,tabla,...){
  print("generando query insert")
  # print(tabla)
  # print(datos)
  query = "INSERT INTO @tabla@ (@col@) VALUES (@val@)"
  query = sqlGsub(query,param=c(tabla = tabla, col = paste(colnames(datos),collapse=",") ))
  values = alply(datos,1,function(x) paste(
    llply(x,function(y) if(is.na(y)){"NULL"}else{paste0("'",gsub("'","''",as.character(y,...) ),"'")}),collapse="," )
    )
  values = sapply(values,function(x) sqlGsub(query,param = c(val = x) ))
  return(values)
}

sqlInsert = function(sqlStr, datos, tabla,...){
  libreria("plyr")
  values = sql_script(datos,tabla,"INSERT",...)
  
  try({
    sql = sqlGetConn(sqlStr)
    print("insertando registros en tabla")
    l_ply(values,function(x) sqlQuery2(sql,x),.progress = "text")
  })
  
  if(class(sqlStr) == "character"){sqlClose(sql)}
}

sql_script_delete =function(datos,tabla,...){
  print("generando query delete")
  query = "DELETE FROM @tabla@ WHERE @val@"
  query = sqlGsub(query,param=c(tabla = tabla))
  values = apply(datos,2,function(x) paste("'",trim(x),"'",sep=""))
  for(col in colnames(values)){ values[,col] = paste(col,"=",values[,col]) }
  values = apply(values,1,paste,collapse = " AND ")
  query = sapply(values,function(x) sqlGsub(query,param = c(val = x)))
  return(query)
}

sql_script_update =function(datos,tabla,...){
  print("generando query update")
  parametros = c(tabla = tabla)
  query = "UPDATE @tabla@ SET @asignacion@ WHERE @filtro@"
  query = sqlGsub(query,param=parametros)
  values = apply(datos,2,function(x) paste("'",trim(x),"'",sep=""))
  for(col in colnames(values)){ values[,col] = paste(col,"=",values[,col]) }
  cols_filtro = colnames(values)[-ncol(values)]
  cols_asignacion = colnames(values)[ncol(values)]
  query = apply(values,1,function(x) sqlGsub(query,param = c(asignacion = paste0(x[cols_asignacion]), filtro = paste(x[cols_filtro],collapse = " AND ") )))
  return(query)
}

sql_run_script = function(sqlStr,scriptPath,...){
  sql = sqlGetConn(sqlStr)
  query = paste(readLines(scriptPath),collapse=" ")
  return(sqlQuery2(sqlStr,query,...))
  
}