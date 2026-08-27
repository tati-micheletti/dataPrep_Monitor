#' Aggregate one 30m terrain derivative to one output scale and save it
#'
#' @param rast30m SpatRaster at native ~30m resolution.
#' @param scaleName Character. e.g. "habitat" or "landscape".
#' @param layerName Character. e.g. "elevation", "slope", "solar_radiation".
#' @param outputDir Character. Directory to save the aggregated raster in.
#' @param targetResM Numeric. Target resolution in metres.
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @return Invisibly, the path to the saved raster.
aggregateAndSave <- function(rast30m, scaleName, layerName, outputDir,
                              targetResM, targetCRS) {

  outFile <- file.path(outputDir, paste0(layerName, "_", scaleName, ".tif"))

  if (isValidRasterFile(outFile)) {
    message("  Cache hit: ", basename(outFile))
    return(invisible(outFile))
  }

  message("  Aggregating ", layerName, " to ", scaleName, " (", targetResM, "m)...")

  fact <- round(targetResM / 30)
  rAgg <- terra::aggregate(rast30m, fact = fact, fun = "mean", na.rm = TRUE)
  rProj <- terra::project(rAgg, targetCRS, res = targetResM, method = "bilinear")
  names(rProj) <- layerName
  terra::writeRaster(rProj, outFile, overwrite = TRUE)
  message("  Saved: ", outFile)
  invisible(outFile)
}
