##################################
# PARAMETROS #####################
##################################

correo_informes = "cumplimiento@cuprum.cl"

##################################
# FUNCIONES Y LIBRERIAS ##########
##################################

libreria("xtable")
libreria("RODBC")

mail = function(SQL,titulo, mensaje, destinatario){
  query = "{CALL Rmail('@titulo@','@mensaje@','@destinatario@')}"
  query = gsub("@titulo@",titulo,query)
  query = gsub("@mensaje@",mensaje,query)
  query = gsub("@destinatario@",destinatario,query)
  null = sqlQuery2(SQL,query)
}

mailTable = function(SQL,titulo, tabla, destinatario){
  mensaje = print(xtable(tabla),type="html")
  mensaje = paste(unlist(mensaje),collapse = " ")
  mail(SQL,titulo,mensaje,destinatario)
}

aplicarApertura = function(mapeo, apertura){
  
  apertura = mutate(apertura,ID_ORIGEN = toupper(ID_ORIGEN),
                          TIPO_ID_ORIGEN = toupper(TIPO_ID_ORIGEN))
  totales = group_by(apertura,ID_ORIGEN,TIPO_ID_ORIGEN) %>%
                    summarise(total = sum(PESO)) %>%
                    filter(ID_ORIGEN %in% mapeo$ID)
  noSuma = filter(totales,abs(total) - 1 > 0.0005)
  if(nrow(noSuma)>0){
    mailTable(SQL,paste("Aperturas no suman 100% en",Parametro),noSuma,correo_informes)
  }

  mapeo = mutate(mapeo,ID_ORIGEN = toupper(ID),
                 TIPO_ID_ORIGEN = toupper(TIPO_ID))
  mapeo = left_join(mapeo,apertura,by = c("ID_ORIGEN","TIPO_ID_ORIGEN"))
  
  mapeo = mutate(mapeo, PESO = isnull(PESO.x,1)*isnull(PESO.y,1))
  mapeo = mutate(mapeo,LOG = ifelse(is.na(ID_DESTINO),LOG,paste(LOG,Parametro,"->",TIPO_ID_DESTINO,"=",ID_DESTINO,";",sep="")))
  mapeo = mutate(mapeo,ID = ifelse(is.na(ID_DESTINO),ID,toupper(ID_DESTINO)))
  mapeo = mutate(mapeo,ID = ifelse(is.na(ID_DESTINO),TIPO_ID,toupper(TIPO_ID_DESTINO)))
  
  mapeo=select(mapeo,-ID_ORIGEN,-ID_DESTINO,-TIPO_ID_ORIGEN,-TIPO_ID_DESTINO,-PESO.x,-PESO.y)
  
  return(mapeo)

}
