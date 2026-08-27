#' Compute rolling-window BIO1-19 bioclim climatologies for all target years
#'
#' Hybrid CHELSA data source: tasmin/tasmax monthly files 2000-2021, daily
#' 2022+; prec daily throughout for a consistent source. For each target
#' year Y, averages the 6-year window (Y-5) to Y and computes BIO1-19.
#'
#' @param climateTargetYears Integer vector of target years.
#' @param climateWindowLength Integer. Rolling window length in years.
#' @param europeBboxVec Numeric vector `c(N, W, S, E)` in WGS84.
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param climateResolutionM Numeric. Output resolution in metres.
#' @param chelsaMonthlyDir Character. Directory to cache monthly rasters in.
#' @param climateOutputDir Character. Directory to save bioclim rasters in.
#' @param chelsaBase Character. Base URL of the CHELSA archive.
#' @param monthlyMaxYr Integer. Last year with a CHELSA monthly source.
#' @return Invisibly, a named list of output bioclim raster paths, one per
#'   target year.
prepareClimateData <- function(climateTargetYears, climateWindowLength,
                                europeBboxVec, targetCRS, climateResolutionM,
                                chelsaMonthlyDir, climateOutputDir,
                                chelsaBase = "https://os.unil.cloud.switch.ch/chelsa02/chelsa/global",
                                monthlyMaxYr = 2021L) {

  dir.create(chelsaMonthlyDir, recursive = TRUE, showWarnings = FALSE)
  dir.create(climateOutputDir, recursive = TRUE, showWarnings = FALSE)

  # europeBboxVec is c(N, W, S, E)
  europeBbox <- terra::ext(europeBboxVec[2], europeBboxVec[4],
                            europeBboxVec[3], europeBboxVec[1])

  message("===================================================")
  message("Rolling window bioclim -- European extent")
  message("Target years: ", paste(range(climateTargetYears), collapse = " to "))
  message("Window length: ", climateWindowLength, " years (Y-", climateWindowLength - 1, " to Y)")
  message("Data sources:")
  message(" tasmin/tasmax: monthly <=", monthlyMaxYr, ", daily >", monthlyMaxYr)
  message(" prec: daily throughout (consistent source)")
  message("Output dir: ", climateOutputDir)
  message("===================================================\n")

  outFiles <- lapply(climateTargetYears, function(yr) {
    message("\n-- Target year ", yr, " (window ", yr - (climateWindowLength - 1), "-", yr, ") --------")
    computeBioclimYear(targetYear = yr,
                        windowLength = climateWindowLength,
                        climateOutputDir = climateOutputDir,
                        chelsaMonthlyDir = chelsaMonthlyDir,
                        europeBbox = europeBbox,
                        chelsaBase = chelsaBase,
                        targetCRS = targetCRS,
                        climateResolutionM = climateResolutionM,
                        monthlyMaxYr = monthlyMaxYr)
  })

  names(outFiles) <- climateTargetYears

  present <- list.files(climateOutputDir, pattern = "bioclim_.*\\.tif$")
  message("\n===================================================")
  message("Done. ", length(present), " bioclim files produced:")
  for (f in present) message("  ", f)
  message("===================================================")

  invisible(outFiles)
}
