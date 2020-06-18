#' Calculates first day of the month of a date
#'
#' Recieve a date and return the first day of that date's month.
#' @param date A POSIXct, POSIXlt or Date parameter. Do NOT use character.
#' @usage firstMonthDay(date)
#' @return The first day of the input's month as a Date
#' @author Daniel Fischer
#' @example examples\firstMonthDayex.R
#' @author Daniel Fischer
#' @export
firstMonthDay = function(date) {
  if (any(class(date) == "character")) {
    stop("use date formats")
  }
  as.Date(as.character(date, "%Y-%m-01"))
}
