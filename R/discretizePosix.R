#' Discretizes time periods
#'
#' Takes an array of timestamps and discretizes it by a specific time difference.
#' @param timestamps array with timestamps to dscretize, must be POSIX.
#' @param delta time difference to discretize-with in seconds, use 60 to descretize by minutes,
#' 3600 by hours, etc.
#' @usage discretizePosix(timestamps, delta)
#' @return Returns an array with times discretized.
#' @example examples\discretizePosixex.R
#' @author Daniel Fischer
#' @export
discretizePosix = function(timestamps, delta) {
  dates = as.character(as.Date(timestamps))
  times = as.POSIXct(paste("2000-01-01", as.character(timestamps, "%H:%M:%S")))
  cuts = seq(
    from = as.POSIXct("2000-01-01 00:00:00"),
    to =  as.POSIXct("2000-01-03 00:00:00"),
    by = delta
  )
  times = cut(times, cuts, include.lowest = T)
  levels(times) = substr(levels(times), 12, 100)
  as.POSIXct(paste(dates, times))
}
