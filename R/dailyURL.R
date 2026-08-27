#' Build a CHELSA daily-source URL (tasmin/tasmax 2022+, prec all years)
#'
#' @param variable Character. One of "tasmin", "tasmax", "prec".
#' @param year Integer.
#' @param month Integer, 1-12.
#' @param day Integer, day of month.
#' @param chelsaBase Character. Base URL of the CHELSA archive.
#' @return Character, a `/vsicurl/` GDAL-readable URL.
dailyURL <- function(variable, year, month, day, chelsaBase) {
  paste0("/vsicurl/", chelsaBase,
         "/daily/", variable, "/", year, "/",
         "CHELSA_", variable, "_",
         sprintf("%02d", day), "_",
         sprintf("%02d", month), "_",
         year, "_V.2.1.tif")
}
