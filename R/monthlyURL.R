#' Build a CHELSA monthly-source URL (tasmin/tasmax only, 2000-2021)
#'
#' @param variable Character. One of "tasmin", "tasmax".
#' @param year Integer.
#' @param month Integer, 1-12.
#' @param chelsaBase Character. Base URL of the CHELSA archive.
#' @return Character, a `/vsicurl/` GDAL-readable URL.
monthlyURL <- function(variable, year, month, chelsaBase) {
  paste0("/vsicurl/", chelsaBase,
         "/monthly/", variable, "/", year, "/",
         "CHELSA_", variable, "_",
         sprintf("%02d", month), "_", year, "_V.2.1.tif")
}
