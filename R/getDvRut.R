#' Calculates DV from RUT
#' 
#' Calculates DV from RUT
#' @param rut vector of ruts on multiple format format (specified in examples)
#' @param append_dv should the calculated DV be apended to the rut or just return the DV
#' 
#' @examples 
#' 
#' ruts_sin_dv = c("15829332","16876895","17994104")
#' getDvRut(ruts_sin_dv)
#' getDvRut(ruts_sin_dv,append_dv = T)
#' 
#' ruts = "30.686.957-X"
#' getDvRut(ruts_sin_dv)
#' @export

getDvRut = function(rut, append_dv = F){
  secuencia = rep(2:7,3)
  
  rut_char = as.character(rut)
  rut_char = str_replace(rut_char,"-.$","")
  rut_char = str_extract_all(rut_char,"[0-9]")
  
  # r = rut_char[[1]]
  verificador = map_chr(rut_char,function(r){
    num_rut = as.integer(r)[length(r):1]
    verif = as.integer(11 - num_rut %*% secuencia[1:length(num_rut)] %% 11)
    verif
  })
  verificador = ifelse(verificador == 10L,"k",verificador)
  
  if(append_dv){
    ruts_dv = map_chr(rut_char,paste,collapse="")
    ruts_dv = paste(ruts_dv,verificador,sep="-")
    return(ruts_dv)
  }
  
  return(verificador)
}


