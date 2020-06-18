#' converts month name into number (WIP)
#'
#' Takes a month, or array of months, wrote and turns it into numeric representation.
#' @param months array of months to be tunred. Every element of months has to be a Character or able
#' to be coerced into one.
#' @usage numericMonth(months)
#' @return Returns an array of same length as months, with numeric representation of its months.
#' @details It accepts spanish months too.
#' @author Daniel Fischer
#' @example examples\numericMonthex.R
#' @example examples\numericMonthESex.R
#' @export
numericMonth = function(months, interger = T) {
  safeLibrary(dplyr)
  safeLibrary(stringr)
  months = tolower(enc2utf8(months))
  months = trim(months)
  months = recode(months,
                  january = 1,
                  jan = 1,
                  february = 2,
                  feb = 2,
                  march = 3,
                  mar = 3,
                  april = 4,
                  apr = 4,
                  may = 5,
                  june = 6,
                  jun = 6,
                  july = 7,
                  jul = 7,
                  august = 8,
                  aug = 8,
                  september = 9,
                  sep = 9,
                  sept = 9,
                  october = 10,
                  oct = 10,
                  november = 11,
                  nov = 11,
                  december = 12,
                  dec = 12,
                  
                  enero = 1,
                  ene = 1,
                  febrero = 2,
                  feb = 2,
                  marzo = 3,
                  mar = 3,
                  abril = 4,
                  abr = 4,
                  mayo = 5,
                  junio = 6,
                  julio = 7,
                  agosto = 8,
                  ago = 8,
                  septiembre = 9,
                  octubre = 10,
                  noviembre = 11,
                  diciembre = 12,
                  dic = 12,
                  .default = 99)
  if(!interger){
    str_pad(months,2,"left","0")
  }else{
    months
  }
}
