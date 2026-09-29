#' Compute the distance-to-woodland predictor for all three CORINE snapshot years
#'
#' Mirrors `prepareLandcover()`'s per-CORINE-year loop, reusing whatever
#' raw CORINE rasters that function already downloaded (this is always
#' scheduled after `prepareLandcover` in the same run, so no separate
#' download step is needed here).
#'
#' @param landcoverRawDir Character. Directory containing raw CORINE rasters.
#' @param processedRoot Character. `predictors/processed` directory
#'   (without the scale_X leaf).
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param habitatResolutions,landscapeResolutions Numeric vectors. Every
#'   distinct resolution (m) actually needed at that scale.
#' @param force Logical. If TRUE, recompute and overwrite every CORINE year
#'   even if a valid cached output already exists.
#' @return Invisibly, a named list of output file paths per CORINE year.
prepareDistToWoodland <- function(landcoverRawDir, processedRoot, targetCRS,
                                    habitatResolutions, landscapeResolutions,
                                    force = FALSE) {

  corineYears <- as.integer(names(corineYearMap()))

  message("Processing dist_to_woodland for CORINE years: ", paste(corineYears, collapse = ", "))

  outFiles <- lapply(corineYears, function(yr) {
    message("CORINE year: ", yr)
    computeDistToWoodland(corineYear = yr,
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
