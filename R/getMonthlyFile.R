#' Get (and cache) one monthly CHELSA raster for one variable/year/month
#'
#' Uses the pre-aggregated CHELSA monthly source for tasmin/tasmax up to
#' `monthlyMaxYr`; falls back to aggregating daily rasters otherwise (used
#' for prec throughout, and for tasmin/tasmax after `monthlyMaxYr`, and
#' as a fallback if the monthly file fails to open).
#'
#' Daily aggregation (fixed 2026-10-07; DECISIONS.md): tasmin/tasmax are the MEAN of
#' the daily minima/maxima, which is what CHELSA's monthly product (and the bioclim
#' definition) means. Until then they were the MIN of the daily minima and the MAX of
#' the daily maxima, which made every month from 2022 on about 4 K colder (tasmin) and
#' 5 K warmer (tasmax) than the months up to 2021 -- a break at the source change. Daily
#' layers are also cleaned of fill values (a few 2022/2025 months contained 0 or 6553.4),
#' and a month with fewer than 90% of its days is NOT used (partial months bias a mean
#' and a sum). Months aggregated this way from daily data after `monthlyMaxYr` are cached
#' under a NEW file name (`<variable>_<year>_<month>_dmean.tif`), so caches written by
#' the old rule are never reused and need no deleting.
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

  useMonthlySource <- variable %in% c("tasmin", "tasmax") && year <= monthlyMaxYr
  # tasmin/tasmax after the monthly source ends: daily mean (new name, see above)
  dailyMeanTemp <- variable %in% c("tasmin", "tasmax") && year > monthlyMaxYr

  outFile <- file.path(chelsaMonthlyDir,
                        sprintf("%s_%d_%02d%s.tif", variable, year, month, if (dailyMeanTemp) "_dmean" else ""))

  if (isValidRasterFile(outFile)) return(invisible(outFile))

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
      # terra objects wrap GDAL/vsicurl handles that R's automatic GC does
      # not reliably reclaim promptly in a long-running loop -- explicit
      # rm()+gc() here is what keeps memory flat across hundreds of calls
      # instead of accumulating open remote-file handles for the whole run.
      rm(r)
      gc(verbose = FALSE)
      return(invisible(outFile))
    }
    message("Falling back to daily aggregation...")
  }

  # Daily aggregation: used for prec (all years), tasmin/tasmax 2022+,
  # and as fallback if the monthly file failed
  dailyVar <- if (variable == "prec") "prec" else variable
  # tasmin/tasmax: the monthly value is the MEAN of the daily minima/maxima (CHELSA's monthly
  # definition); prec: the monthly total.
  fun <- switch(variable,
                tasmin = "mean",
                tasmax = "mean",
                prec   = "sum")

  message("Daily aggregation: ", variable, " ", year, "/", sprintf("%02d", month))

  nDays <- daysInMonth(year, month)
  readDay <- function(d) {
    url <- dailyURL(dailyVar, year, month, d, chelsaBase)
    r <- tryCatch(terra::rast(url), error = function(e) NULL)
    if (is.null(r)) { warning("Failed: ", sprintf("CHELSA_%s_%02d_%02d_%d_V.2.1.tif", dailyVar, d, month, year)); return(NULL) }
    readDailyCropped(url, europeBbox, variable)   # retries a failed read; fill values -> NA
  }
  dailyLayers <- lapply(seq_len(nDays), readDay)
  # a read that failed PARTLY leaves a layer with far fewer valid cells: re-read it, or drop it (see readDailyCropped.R)
  dailyLayers <- completeDailyLayers(dailyLayers, reread = readDay)

  dailyLayers <- dailyLayers[!sapply(dailyLayers, is.null)]
  nRetrieved <- length(dailyLayers)

  if (nRetrieved == 0) {
    warning("No data retrieved for ", variable, " ", year, "/", month)
    return(invisible(NULL))
  }
  if (nRetrieved < nDays) {
    warning("Only ", nRetrieved, "/", nDays, " days retrieved for ",
            variable, " ", year, "/", sprintf("%02d", month))
  }
  if (nRetrieved < 0.9 * nDays) {
    warning("Fewer than 90% of the days available for ", variable, " ", year, "/", sprintf("%02d", month),
            " -- month NOT used (a partial month would bias the monthly value)")
    return(invisible(NULL))
  }

  dailyStack <- terra::rast(dailyLayers)
  # dailyLayers is up to 31 separate open GDAL/vsicurl handles -- drop them
  # the moment they're combined into dailyStack, don't wait for the whole
  # function to return (see the note on the monthly-source branch above).
  rm(dailyLayers)
  gc(verbose = FALSE)

  monthly <- switch(fun,
                     "mean" = terra::app(dailyStack, mean, na.rm = TRUE),
                     "sum" = terra::app(dailyStack, sum, na.rm = TRUE))

  names(monthly) <- sprintf("%s_%d_%02d", variable, year, month)
  terra::writeRaster(monthly, outFile, overwrite = TRUE)
  rm(dailyStack, monthly)
  gc(verbose = FALSE)
  invisible(outFile)
}
