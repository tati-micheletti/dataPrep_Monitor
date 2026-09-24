#' Download and process the DEM (elevation, slope, solar radiation)
#'
#' Downloads Copernicus GLO-30 DEM tiles for `bboxVec`, then processes
#' them into elevation/slope/solar-radiation-aspect derivatives at
#' habitat and landscape scales.
#'
#' @param demRawDir Character. Directory for downloaded DEM tiles.
#' @param processedDir Character. Directory for intermediate 30m rasters.
#' @param habitatOutputDir Character. Directory for habitat-scale outputs.
#' @param landscapeOutputDir Character. Directory for landscape-scale outputs.
#' @param bboxVec Numeric vector `c(N, W, S, E)` in WGS84 to download DEM for.
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param habitatResolutionM Numeric. Habitat scale resolution in metres.
#' @param landscapeResolutionM Numeric. Landscape scale resolution in metres.
#' @param pythonScriptPath Character. Path to `download_dem.py`.
#' @param requirementsPath Character. Path to `requirements.txt`.
#' @param force Logical. If TRUE, recompute and overwrite every intermediate
#'   and output step even if a valid cached file already exists (e.g. a bug
#'   was found in the raw DEM tiles).
#' @return Invisibly, a named list of output DEM derivative file paths.
prepareDEM <- function(demRawDir, processedDir, habitatOutputDir,
                        landscapeOutputDir, bboxVec, targetCRS,
                        habitatResolutionM, landscapeResolutionM,
                        pythonScriptPath, requirementsPath, force = FALSE) {

  downloadDEM(demRawDir = demRawDir,
              bboxVec = bboxVec,
              pythonScriptPath = pythonScriptPath,
              requirementsPath = requirementsPath)

  processDEM(demRawDir = demRawDir,
             processedDir = processedDir,
             habitatOutputDir = habitatOutputDir,
             landscapeOutputDir = landscapeOutputDir,
             targetCRS = targetCRS,
             habitatResolutionM = habitatResolutionM,
             landscapeResolutionM = landscapeResolutionM,
             force = force)
}
