#' Number of days in a given year/month
#'
#' @param year Integer.
#' @param month Integer, 1-12.
#' @return Integer, number of days in that month.
daysInMonth <- function(year, month) {
  if (month == 12) return(31L)
  as.integer(format(
    as.Date(paste(year, month + 1, "01", sep = "-")) - 1,
    "%d"
  ))
}
