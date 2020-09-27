sql_script_insert = function(datos,tabla){
  # print("generando query insert")
  # print(tabla)
  # print(datos)
  query = "INSERT INTO @tabla@ (@col@) VALUES (@val@)"
  query = sqlGsub(query,param=c(tabla = tabla, col = paste(colnames(datos),collapse=",") ))
  values = datos %>% apply(1,function(x) paste(ifelse(is.na(x),"NULL",paste0("'",str_replace_all(x,"\'","''''"),"'") ),collapse=","))
  
  values = sapply(values,function(x) sqlGsub(query,param = c(val = x) ))
  return(values)
}