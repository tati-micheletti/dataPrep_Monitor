#' Get (and cache) one monthly CHELSA raster for one variable/year/month
#'
#' Uses the pre-aggregated CHELSA monthly source for tasmin/tasmax up to
#' `monthlyMaxYr`; falls back to aggregating daily rasters otherwise (used
#' for prec throughout, and for tasmin/tasmax after `monthlyMaxYr`, and
#' as a fallback if the monthly file fails to open).
#'
#' @param variable Character. One of "tasmin", "tasmax", "prec".
#' @param year Integer.
#' @param month Integer, 1-12.
#' @param chelsaMonthlyDir Character. Directory to cache monthly rasters in.
#' @param europeBbox A `terra::ext` object to crop to.
#' @param chelsaBase Character. Base URL of the CHELSA archive.
#' @param monthlyMaxYr Integer. Last year with a CHELSA monthly source for
#'   tasmin/tasmax; defaults to 2021.
#' @return Invisibly, the path to the cached monthly raster, or NULL if no
#'   data could be retrieved.
getMonthlyFile <- function(variable, year, month, chelsaMonthlyDir,
                            europeBbox, chelsaBase, monthlyMaxYr = 2021L) {

  outFile <- file.path(chelsaMonthlyDir,
                        sprintf("%s_%d_%02d.tif", variable, year, month))

  if (isValidRasterFile(outFile)) return(invisible(outFile))

  useMonthlySource <- variable %in% c("tasmin", "tasmax") && year <= monthlyMaxYr

  if (useMonthlySource) {

    message("Monthly source: ", variable, " ", year, "/", sprintf("%02d", month))
    url <- monthlyURL(variable, year, month, chelsaBase)
    r <- tryCatch(terra::crop(terra::rast(url), europeBbox),
                  error = function(e) {
                    warning("Monthly file failed, falling back to daily: ", basename(url))
                    NULL
                  })

    if (!is.null(r)) {
      names(r) <- sprintf("%s_%d_%02d", variable, year, month)
      terra::writeRaster(r, outFile, overwrite = TRUE)
      return(invisible(outFile))
    }
    message("Falling back to daily aggregation...")
  }

  # Daily aggregation: used for prec (all years), tasmin/tasmax 2022+,
  # and as fallback if the monthly file failed
  dailyVar <- if (variable == "prec") "prec" else variable
  fun <- switch(variable,
                tasmin = "min",
                tasmax = "max",
                prec   = "sum")

  message("Daily aggregation: ", variable, " ", year, "/", sprintf("%02d", month))

  nDays <- daysInMonth(year, month)
  dailyLayers <- lapply(seq_len(nDays), function(d) {
    url <- dailyURL(dailyVar, year, month, d, chelsaBase)
    r <- tryCatch(terra::rast(url), error = function(e) {
      warning("Failed: ", sprintf("CHELSA_%s_%02d_%02d_%d_V.2.1.tif",
                                   dailyVar, d, month, year))
      NULL
    })
    if (is.null(r)) return(NULL)
    terra::crop(r, europeBbox)
  })

  dailyLayers <- dailyLayers[!sapply(dailyLayers, is.null)]
  nRetrieved <- length(dailyLayers)

  if (nRetrieved == 0) {
    warning("No data retrieved for ", variable, " ", year, "/", month)
    return(invisible(NULL))
  }
  if (nRetrieved < nDays) {
    warning("Only ", nRetrieved, "/", nDays, " days retrieved for ",
            variable, " ", year, "/", sprintf("%02d", month),
            " -- monthly value based on partial data")
  }

  dailyStack <- terra::rast(dailyLayers)
  monthly <- switch(fun,
                     "min" = terra::app(dailyStack, min, na.rm = TRUE),
                     "max" = terra::app(dailyStack, max, na.rm = TRUE),
                     "sum" = terra::app(dailyStack, sum, na.rm = TRUE))

  names(monthly) <- sprintf("%s_%d_%02d", variable, year, month)
  terra::writeRaster(monthly, outFile, overwrite = TRUE)
  invisible(outFile)
}
