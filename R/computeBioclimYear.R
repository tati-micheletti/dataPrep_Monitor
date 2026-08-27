#' Compute a rolling-window BIO1-19 bioclim raster for one target year
#'
#' Window = (targetYear - windowLength + 1) to targetYear. Sources monthly
#' tasmin/tasmax/prec for every year/month in the window, averages each
#' calendar month across the window, reprojects, and computes BIO1-19.
#'
#' @param targetYear Integer.
#' @param windowLength Integer. Number of years in the rolling window.
#' @param climateOutputDir Character. Directory to save the output raster in.
#' @param chelsaMonthlyDir Character. Directory to cache monthly rasters in.
#' @param europeBbox A `terra::ext` object to crop to.
#' @param chelsaBase Character. Base URL of the CHELSA archive.
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param climateResolutionM Numeric. Output resolution in metres.
#' @param monthlyMaxYr Integer. Last year with a CHELSA monthly source.
#' @return Invisibly, the path to the saved bioclim raster.
computeBioclimYear <- function(targetYear, windowLength, climateOutputDir,
                                chelsaMonthlyDir, europeBbox, chelsaBase,
                                targetCRS, climateResolutionM,
                                monthlyMaxYr = 2021L) {

  windowStart <- targetYear - (windowLength - 1)
  windowYears <- windowStart:targetYear
  startYr <- min(windowYears)
  endYr <- max(windowYears)

  outFile <- file.path(climateOutputDir, paste0("bioclim_", startYr, "-", endYr, ".tif"))

  if (isValidRasterFile(outFile)) {
    r <- terra::rast(outFile)
    if (terra::nlyr(r) == 19) {
      message("Cache hit: ", basename(outFile))
      return(invisible(outFile))
    }
    message("Cache found but incomplete, recomputing...")
  }

  message("Target year: ", targetYear, "| Window: ", startYr, "-", endYr)
  message("Sources: tasmin/tasmax monthly <=", monthlyMaxYr,
          ", daily >", monthlyMaxYr, "; prec daily throughout")

  message("Getting monthly files for window years: ",
          paste(windowYears, collapse = ", "), "...")

  for (yr in windowYears) {
    for (m in 1:12) {
      getMonthlyFile("tasmin", yr, m, chelsaMonthlyDir, europeBbox, chelsaBase, monthlyMaxYr)
      getMonthlyFile("tasmax", yr, m, chelsaMonthlyDir, europeBbox, chelsaBase, monthlyMaxYr)
      getMonthlyFile("prec", yr, m, chelsaMonthlyDir, europeBbox, chelsaBase, monthlyMaxYr)
    }
  }

  message("Averaging each month across ", windowLength, "-year window...")

  tminMonths <- lapply(1:12, function(m)
    averageMonthAcrossYears("tasmin", m, windowYears, chelsaMonthlyDir,
                             europeBbox, chelsaBase, monthlyMaxYr))
  tmaxMonths <- lapply(1:12, function(m)
    averageMonthAcrossYears("tasmax", m, windowYears, chelsaMonthlyDir,
                             europeBbox, chelsaBase, monthlyMaxYr))
  precMonths <- lapply(1:12, function(m)
    averageMonthAcrossYears("prec", m, windowYears, chelsaMonthlyDir,
                             europeBbox, chelsaBase, monthlyMaxYr))

  tminStack <- terra::rast(tminMonths)
  tmaxStack <- terra::rast(tmaxMonths)
  precStack <- terra::rast(precMonths)

  # Temperature: Kelvin -> Celsius
  tminCelsius <- tminStack - 273.15
  tmaxCelsius <- tmaxStack - 273.15
  # prec: already in mm/month from daily aggregation

  message("Reprojecting to ", targetCRS, " at ", climateResolutionM / 1000, "km...")
  tminProj <- terra::project(tminCelsius, targetCRS, res = climateResolutionM, method = "bilinear")
  tmaxProj <- terra::project(tmaxCelsius, targetCRS, res = climateResolutionM, method = "bilinear")
  precProj <- terra::project(precStack, targetCRS, res = climateResolutionM, method = "bilinear")

  message("Computing BIO1-19 at ", climateResolutionM / 1000, "km resolution...")
  bv <- dismo::biovars(prec = raster::stack(precProj),
                        tmin = raster::stack(tminProj),
                        tmax = raster::stack(tmaxProj))
  names(bv) <- paste0("bio", 1:19)
  result <- terra::rast(bv)
  terra::crs(result) <- targetCRS

  terra::writeRaster(result, outFile, overwrite = TRUE)
  message("Saved: ", basename(outFile))
  invisible(outFile)
}
