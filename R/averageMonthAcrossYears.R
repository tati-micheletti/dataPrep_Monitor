#' Average one calendar month across all years in a rolling window
#'
#' @param variable Character. One of "tasmin", "tasmax", "prec".
#' @param month Integer, 1-12.
#' @param windowYears Integer vector of years in the window.
#' @param chelsaMonthlyDir Character. Directory monthly rasters are cached in.
#' @param europeBbox A `terra::ext` object to crop to.
#' @param chelsaBase Character. Base URL of the CHELSA archive.
#' @param monthlyMaxYr Integer. Last year with a CHELSA monthly source.
#' @return SpatRaster, the across-year mean for this month.
averageMonthAcrossYears <- function(variable, month, windowYears,
                                     chelsaMonthlyDir, europeBbox,
                                     chelsaBase, monthlyMaxYr = 2021L) {
  yearRasts <- lapply(windowYears, function(yr) {
    path <- getMonthlyFile(variable, yr, month, chelsaMonthlyDir,
                            europeBbox, chelsaBase, monthlyMaxYr)
    if (is.null(path)) return(NULL)
    terra::rast(path)
  })

  yearRasts <- yearRasts[!sapply(yearRasts, is.null)]
  if (length(yearRasts) == 0) {
    stop("No data available for ", variable, " month ", month,
         " across years ", paste(windowYears, collapse = ", "))
  }
  if (length(yearRasts) < length(windowYears)) {
    warning("Only ", length(yearRasts), "/", length(windowYears),
            " years available for ", variable, " month ", month)
  }

  yearStack <- terra::rast(yearRasts)
  monthMean <- terra::app(yearStack, mean, na.rm = TRUE)
  names(monthMean) <- sprintf("%s_mean_%02d", variable, month)
  monthMean
}
