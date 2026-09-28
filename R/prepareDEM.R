#' Download and process the DEM (elevation, slope, solar radiation)
#'
#' Downloads Copernicus GLO-30 DEM tiles for `bboxVec`, then processes
#' them into elevation/slope/solar-radiation-aspect derivatives at every
#' distinct habitat/landscape resolution actually needed (see
#' `processDEM()`).
#'
#' @param demRawDir Character. Directory for downloaded DEM tiles.
#' @param processedDir Character. Directory for intermediate 30m rasters.
#' @param processedRoot Character. `predictors/processed` directory
#'   (without the scale_X leaf).
#' @param bboxVec Numeric vector `c(N, W, S, E)` in WGS84 to download DEM for.
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param habitatResolutions,landscapeResolutions Numeric vectors. Every
#'   distinct resolution (m) actually needed at that scale.
#' @param pythonScriptPath Character. Path to `download_dem.py`.
#' @param requirementsPath Character. Path to `requirements.txt`.
#' @param force Logical. If TRUE, recompute and overwrite every intermediate
#'   and output step even if a valid cached file already exists (e.g. a bug
#'   was found in the raw DEM tiles).
#' @return Invisibly, a named list of output DEM derivative file paths.
prepareDEM <- function(demRawDir, processedDir, processedRoot, bboxVec, targetCRS,
                        habitatResolutions, landscapeResolutions,
                        pythonScriptPath, requirementsPath, force = FALSE) {

  downloadDEM(demRawDir = demRawDir,
              bboxVec = bboxVec,
              pythonScriptPath = pythonScriptPath,
              requirementsPath = requirementsPath)

  processDEM(demRawDir = demRawDir,
             processedDir = processedDir,
             processedRoot = processedRoot,
             targetCRS = targetCRS,
             habitatResolutions = habitatResolutions,
             landscapeResolutions = landscapeResolutions,
             force = force)
}
