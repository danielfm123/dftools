#' Returns the last day of the month of a date
#'
#' Recieve a date and return the Last  day of that date's month.
#' @param date A POSIXct, POSIXlt or Date parameter. Do NOT use character.
#' @usage lastMonthDay(date)
#' @return The Last  day of the input's month as a Date.
#' @author Daniel Fischer
#' @example examples\lastMonthDayex.R
#' @export
lastMonthDay = function(date) {
  if (any(class(date) == "character")) {
    stop("use date formats")
  }
  seq(from = firstMonthDay(date),
      length = 2,
      by = "month")[2] - 1
}
