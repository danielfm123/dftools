###################################################
#### OPCIONES #####################################
###################################################


options(stringsAsFactors = FALSE)
Sys.setenv(TZ='GMT')
nCPU = as.numeric(Sys.getenv("NUMBER_OF_PROCESSORS")[1])
options(java.parameters = "-Xmx256m")
options(repos=structure(c(CRAN="https://cran.dcc.uchile.cl/")))
options(xtable.include.rownames=F)

###################################################
#### FUNCIONES SQL ################################
###################################################

sqlGetConn = function(con_name){
  libreria("RODBC")
  connObj = switch (class(con_name),
                    character = odbcDriverConnect(sql_connections[con_name]),
                    RODBC = con_name
  )
  if(version$os == "mingw32"){
    
  }else{
    sqlQuery(connObj,"SET ANSI_PADDING ON")
    sqlQuery(connObj,"SET ANSI_WARNINGS ON")
    sqlQuery(connObj,"SET ANSI_NULLS ON")
  }
  return(connObj)
}

sqlQuery2 = function(sqlStr,query, param = c(),dt=FALSE,key=c(),  ...){
  try({
    sql = sqlGetConn(sqlStr)
    query = sqlGsub(query,param)
    # print(query)
    rowset = sqlQuery(sql,query,...)
  })
  
  if(class(sqlStr) == "character"){sqlClose(sql)}
  
  if(dt){
    libreria("data.table")
    rowset = as.data.table(rowset)
    setkeyv(rowset,key)
    return(rowset)
  }
  else{
    return(rowset)
  }
}

sqlQuery_excel = function(excel,query, param = c(),dt=FALSE,key=c(),  ...){
  libreria("RODBC")
  conn = odbcConnectExcel2007(excel)
  rowset = sqlQuery2(conn,query,param,dt,key,...)
  sqlClose(conn)
  
  return(rowset)
}

sqlSave2 = function(sqlStr,data,tabla,...){
  try({
    sql = sqlGetConn(sqlStr) 
    sqlSave(sql, data, tabla, append = TRUE, rownames = FALSE,...)
  })
  if(class(sqlStr) == "character"){sqlClose(sql)}
}

sqlTables2 = function(sqlStr){
  try({
    sql = sqlGetConn(sqlStr)
    tablas = sqlTables(sql)
  })
  
  if(class(sqlStr) == "character"){sqlClose(sql)}
  return(tablas)
}

sqlColumns = function(sqlStr,tabla){
  columnas = sqlQuery2(sqlStr,"SELECT COLUMN_NAME
                       FROM INFORMATION_SCHEMA.COLUMNS
                       WHERE TABLE_NAME = '@tabla@'
                       order by ORDINAL_POSITION asc",param = c(tabla = tabla))[,1]
  
  return(columnas)
}

sqlClose = function(sql){
  odbcClose(sql)
}

sqlGsub = function(query, param = c()){
  for(p in names(param) ){
    query = gsub(paste0("@",p,"@"), param[p],query,ignore.case = T)
  }
  return(query)
}

sql_connections = sapply(dir("../setup_conecciones_sql/"),
                         function(x) readLines(paste("../setup_conecciones_sql/",x,sep="")),USE.NAMES = T)


###################################################
#### FUNCIONES PERSONALES #########################
###################################################

libreria <- function(x) { 
  paquetes = as.character(substitute(x)) 
  if(!all(paquetes %in% .packages(all.available=TRUE))) { 
    install.packages(paquetes,dependencies=TRUE)
  }
  paquetes = paste(paquetes,collapse = ",")
  string = paste("suppressPackageStartupMessages(require(",paquetes,"))",sep="")
  eval(parse(text = string)) 
}

lagpad <- function(x, k, default = 0) {
  if(k>=0){c(rep(default, k), x)[1 : length(x)]}
  else{c(x,rep(default, -k))[(-k+1) : (length(x)-k)]}
}

eco = function(x,n){
  if(length(x)==1){return(0)}
  rowSums(mapply(1:n,FUN=function(y) lagpad(x,y)))
}

isnull = function(x,reemplazo,Nulos = c(NA,Inf,-Inf,NULL,NaN)){
  ifelse(x %in% Nulos,reemplazo,x)
}

is.between <- function(x, a, b) {
  x >= isnull(a,-Inf) & x <= isnull(b,Inf)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

safe.ifelse <- function(cond, yes, no){ 
  class.y <- class(yes)
  X <- ifelse(cond,yes,no)
  class(X) <-class.y
  return(X)
}

merge2 = function(x,y,by = intersect(names(x), names(y)), by.x = by,by.y = by,all=T,all.x = all, all.y = all,...){
  libreria("plyr")
  x = ddply(x,by.x,function(x) data.frame(x,key_merge = 1:nrow(x)))
  y = ddply(y,by.y,function(x) data.frame(x,key_merge = 1:nrow(x)))
  tabla = merge(x,y,by.x = c(by.x,"key_merge"),by.y = c(by.y,"key_merge"),all.x = all.x, all.y = all.y,...)
  tabla$key_merge = NULL
  return(tabla)
}

rnw_to_pdf = function(archivo, pdf = NULL){
  libreria("tools")
  rnw = paste(archivo,".Rnw",sep="")
  tex = paste(archivo,".tex",sep="")
  
  Sweave(rnw,encoding = "UTF8")
  texi2pdf(tex)
  
  if(is.null(pdf)){
    pdf = paste(archivo,".pdf",sep="")
  }else{
    file.rename(paste(archivo,".pdf",sep=""),pdf)
  }
  
  return(pdf)
}

hacerCluster = function(n = nCPU,scripts = c("../setup.R")){
  libreria("doSNOW")
  cl = makeSOCKcluster(n,outfile="cl.txt")
  registerDoSNOW(cl)
  lapply(scripts, function(x) clusterCall(cl,source,x))
  return(cl)
}

