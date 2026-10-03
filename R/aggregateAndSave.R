#' Aggregate one 30m terrain derivative to one output scale and save it
#'
#' @param rast30m SpatRaster at native ~30m resolution.
#' @param scaleName Character. e.g. "habitat" or "landscape".
#' @param layerName Character. e.g. "elevation", "slope", "solar_radiation".
#' @param outputDir Character. Directory to save the aggregated raster in.
#' @param targetResM Numeric. Target resolution in metres.
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param force Logical. If TRUE, recompute and overwrite even if a valid
#'   cached output already exists (e.g. a bug was found in the raw data).
#' @return Invisibly, the path to the saved raster.
aggregateAndSave <- function(rast30m, scaleName, layerName, outputDir,
                              targetResM, targetCRS, force = FALSE) {

  # Resolution appended to the filename itself (not just the containing
  # scaleLabel()-named folder) as a second, redundant safety layer -- if a
  # file is ever copied/moved out of its folder, its own name still says
  # what resolution it actually is. Derived from outputDir's own basename
  # (already the scaleLabel() string, e.g. "scale_02") rather than a new
  # parameter, so this can't drift from the folder it's actually saved in.
  outFile <- file.path(outputDir, paste0(layerName, "_", scaleName, "_", basename(outputDir), ".tif"))

  if (!force && isValidRasterFile(outFile)) {
    message("  Cache hit: ", basename(outFile))
    return(invisible(outFile))
  }

  message("  Aggregating ", layerName, " to ", scaleName, " (", targetResM, "m)...")

  fact <- round(targetResM / 30)
  rAgg <- terra::aggregate(rast30m, fact = fact, fun = "mean", na.rm = TRUE)
  rProj <- regridToResolution(rAgg, targetResM, targetCRS, method = "bilinear")
  names(rProj) <- layerName
  terra::writeRaster(rProj, outFile, overwrite = TRUE)
  message("  Saved: ", outFile)
  invisible(outFile)
}
