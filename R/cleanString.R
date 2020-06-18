#' Clean characters on strings
#'
#' Clean characters on strings
#' @param string characters to be cleaned
#' @param default_replacements replacement patterns by default in format c("pattern" = "replacement")
#' @param extra_replacements extra replacements to be added to de default
#' @param allowed_chars in regular expression format, default "a-zA-Z0-9"
#' @param space_char character used for spaces, default _
#' @example cleanString(c("Dániel","Data     Avengers     ","Ñandú  "))
#' @export
#' @author Daniel Fischer

cleanString = function(string,
                       default_replacements = c(
                         "Á" = "A",
                         "É" = "E",
                         "Í" = "I",
                         "Ó" = "O",
                         "Ú|Ü" = "U",
                         "Ñ" = "N",
                         "á" = "a",
                         "é" = "e",
                         "í" = "i",
                         "ó" = "o",
                         "ú|ü" = "u",
                         "ñ" = "n"
                         ),
                       extra_replacements =c(),
                       allowed_chars = "a-zA-Z0-9",
                       space_char = "_"
                       ) 
{

replacements = c(default_replacements,extra_replacements)
string = map_chr(string,function(s){
  for(rep in names(replacements) ){
    s = str_replace_all(s,rep,replacements[rep])
  }
  return(s)
})


string = str_replace_all(string,paste0("[^",allowed_chars,"]"),"_")
string = str_replace_all(string,"_+","_")
string = str_replace_all(string,"^_|_$","")
string = str_replace_all(string,"_",space_char)
return(string)

}
