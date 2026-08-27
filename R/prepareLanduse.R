#' Download and process land use (crop type) maps for all target years
#'
#' Downloads the CTM/HCTM crop type maps (Tetteh et al. 2026 HCTM
#' requires a manual download, see `downloadLanduse()`), then computes
#' the 14-category proportion layers at habitat and landscape scales for
#' every year in `landuseYears`.
#'
#' @param landuseRawDir Character. Directory for downloaded raw maps.
#' @param habitatOutputDir Character. Directory for habitat-scale outputs.
#' @param landscapeOutputDir Character. Directory for landscape-scale outputs.
#' @param landuseYears Integer vector of years to process.
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param habitatResolutionM Numeric. Habitat scale resolution in metres.
#' @param landscapeResolutionM Numeric. Landscape scale resolution in metres.
#' @param pythonScriptPath Character. Path to `download_landuse.py`.
#' @param requirementsPath Character. Path to `requirements.txt`.
#' @return Invisibly, a named list of output file paths per year.
prepareLanduse <- function(landuseRawDir, habitatOutputDir, landscapeOutputDir,
                            landuseYears, targetCRS, habitatResolutionM,
                            landscapeResolutionM, pythonScriptPath,
                            requirementsPath) {

  downloadLanduse(landuseRawDir = landuseRawDir,
                   pythonScriptPath = pythonScriptPath,
                   requirementsPath = requirementsPath)

  dir.create(habitatOutputDir, recursive = TRUE, showWarnings = FALSE)
  dir.create(landscapeOutputDir, recursive = TRUE, showWarnings = FALSE)

  message("Processing land use for years: ", paste(landuseYears, collapse = ", "))
  message("Datasets: CTM (Schwieder/Tetteh v302) + HCTM (Tetteh 2026)")
  message("Output categories: 14 (consistent across all years)")
  message("Output scales: ", habitatResolutionM, "m (habitat), ",
          landscapeResolutionM, "m (landscape)")

  outFiles <- lapply(landuseYears, function(yr) {
    message("\nYear: ", yr)
    computeLanduse(year = yr,
                    landuseRawDir = landuseRawDir,
                    habitatOutputDir = habitatOutputDir,
                    landscapeOutputDir = landscapeOutputDir,
                    targetCRS = targetCRS,
                    habitatResolutionM = habitatResolutionM,
                    landscapeResolutionM = landscapeResolutionM)
  })
  names(outFiles) <- landuseYears

  message("\nDone.")
  invisible(outFiles)
}
