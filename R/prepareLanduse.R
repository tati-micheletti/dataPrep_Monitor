#' Download and process land use (crop type) maps for all target years
#'
#' Downloads the CTM/HCTM crop type maps (Tetteh et al. 2026 HCTM
#' requires a manual download, see `downloadLanduse()`), then computes
#' the 14-category proportion layers for every year in `landuseYears`, at
#' every distinct habitat/landscape resolution actually needed (see
#' `computeLanduse()`).
#'
#' @param landuseRawDir Character. Directory for downloaded raw maps.
#' @param processedRoot Character. `predictors/processed` directory
#'   (without the scale_X leaf).
#' @param landuseYears Integer vector of years to process.
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param habitatResolutions,landscapeResolutions Numeric vectors. Every
#'   distinct resolution (m) actually needed at that scale.
#' @param pythonScriptPath Character. Path to `download_landuse.py`.
#' @param requirementsPath Character. Path to `requirements.txt`.
#' @param force Logical. If TRUE, recompute and overwrite every year even if
#'   a valid cached output already exists (e.g. a bug was found in the raw
#'   crop type maps).
#' @return Invisibly, a named list of output file paths per year.
prepareLanduse <- function(landuseRawDir, processedRoot, landuseYears, targetCRS,
                            habitatResolutions, landscapeResolutions,
                            pythonScriptPath, requirementsPath, force = FALSE) {

  downloadLanduse(landuseRawDir = landuseRawDir,
                   pythonScriptPath = pythonScriptPath,
                   requirementsPath = requirementsPath)

  message("Processing land use for years: ", paste(landuseYears, collapse = ", "))
  message("Datasets: CTM (Schwieder/Tetteh v302) + HCTM (Tetteh 2026)")
  message("Output categories: 14 (consistent across all years)")
  message("Output scales: ", paste0(habitatResolutions, "m", collapse = "/"), " (habitat), ",
          paste0(landscapeResolutions, "m", collapse = "/"), " (landscape)")

  outFiles <- lapply(landuseYears, function(yr) {
    message("\nYear: ", yr)
    computeLanduse(year = yr,
                    landuseRawDir = landuseRawDir,
                    processedRoot = processedRoot,
                    habitatResolutions = habitatResolutions,
                    landscapeResolutions = landscapeResolutions,
                    targetCRS = targetCRS,
                    force = force)
  })
  names(outFiles) <- landuseYears

  message("\nDone.")
  invisible(outFiles)
}
