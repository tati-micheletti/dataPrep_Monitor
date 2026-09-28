#' Download and process CORINE Land Cover for all three snapshot years
#'
#' Downloads the 2006/2012/2018 CORINE snapshots via the CLMS Download
#' API, then computes the 3-category (built_up/trees/water) proportion
#' layers for each, at every distinct habitat/landscape resolution
#' actually needed (see `computeLandcover()`). Temporal assignment of
#' these snapshots to occurrence years is handled separately via
#' `corineYear()`/`corineYearMap()`.
#'
#' @param landcoverRawDir Character. Directory for downloaded raw rasters.
#' @param processedRoot Character. `predictors/processed` directory
#'   (without the scale_X leaf).
#' @param bboxVec Numeric vector `c(N, W, S, E)` in WGS84 to download for.
#' @param tokenJSONPath Character. Path to your CLMS API token JSON file.
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param habitatResolutions,landscapeResolutions Numeric vectors. Every
#'   distinct resolution (m) actually needed at that scale.
#' @param pythonScriptPath Character. Path to `download_landcover.py`.
#' @param requirementsPath Character. Path to `requirements.txt`.
#' @param force Logical. If TRUE, recompute and overwrite every CORINE year
#'   even if a valid cached output already exists (e.g. a bug was found in
#'   the raw CORINE rasters).
#' @return Invisibly, a named list of output file paths per CORINE year.
prepareLandcover <- function(landcoverRawDir, processedRoot, bboxVec, tokenJSONPath,
                              targetCRS, habitatResolutions, landscapeResolutions,
                              pythonScriptPath, requirementsPath, force = FALSE) {

  downloadLandcover(landcoverRawDir = landcoverRawDir,
                     bboxVec = bboxVec,
                     tokenJSONPath = tokenJSONPath,
                     pythonScriptPath = pythonScriptPath,
                     requirementsPath = requirementsPath)

  corineYears <- as.integer(names(corineYearMap()))

  message("Processing CORINE land cover snapshots: ", paste(corineYears, collapse = ", "))

  outFiles <- lapply(corineYears, function(yr) {
    message("CORINE year: ", yr)
    computeLandcover(corineYear = yr,
                      landcoverRawDir = landcoverRawDir,
                      processedRoot = processedRoot,
                      habitatResolutions = habitatResolutions,
                      landscapeResolutions = landscapeResolutions,
                      targetCRS = targetCRS,
                      force = force)
  })
  names(outFiles) <- corineYears

  message("\nDone.")
  invisible(outFiles)
}
