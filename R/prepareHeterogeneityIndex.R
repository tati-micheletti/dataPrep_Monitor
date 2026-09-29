#' Compute the Simpson landscape-heterogeneity index for all years/scales
#'
#' Mirrors `prepareLanduse()`'s per-year loop, but also loops over every
#' distinct habitat/landscape resolution actually needed, since
#' `computeHeterogeneityIndex()` reads back one scale's already-written
#' `landuse_*`/`landcover_*` files directly (always scheduled after both
#' `prepareLanduse` and `prepareLandcover` in the same run, so those files
#' already exist).
#'
#' @param processedRoot Character. `predictors/processed` directory
#'   (without the scale_X leaf).
#' @param landuseYears Integer vector of years to process.
#' @param habitatResolutions,landscapeResolutions Numeric vectors. Every
#'   distinct resolution (m) actually needed at that scale.
#' @param force Logical. If TRUE, recompute and overwrite even if a valid
#'   cached output already exists.
#' @return Invisibly, a named list of output file paths, keyed
#'   `"<year>_<scaleName>_<resM>"`.
prepareHeterogeneityIndex <- function(processedRoot, landuseYears,
                                        habitatResolutions, landscapeResolutions,
                                        force = FALSE) {

  scales <- c(
    lapply(habitatResolutions, function(r) list(scaleName = "habitat", res = r)),
    lapply(landscapeResolutions, function(r) list(scaleName = "landscape", res = r))
  )

  message("Processing landscape_heterogeneity for years: ", paste(landuseYears, collapse = ", "))

  outFiles <- list()
  for (yr in landuseYears) {
    for (s in scales) {
      scaleDir <- file.path(processedRoot, scaleLabel(s$res))
      outFile <- file.path(scaleDir, paste0("landscape_heterogeneity_", yr, "_", s$scaleName,
                                             "_", basename(scaleDir), ".tif"))
      key <- paste0(yr, "_", s$scaleName, "_", s$res)

      if (!force && isValidRasterFile(outFile)) {
        message(key, " (landscape_heterogeneity) cache hit -- skipping")
        outFiles[[key]] <- outFile
        next
      }

      message(key, " (landscape_heterogeneity) needs (re)computation")
      heterogeneity <- computeHeterogeneityIndex(year = yr, scaleDir = scaleDir, scaleName = s$scaleName)
      if (is.null(heterogeneity)) {
        message("  Skipped ", key, " -- required land use/land cover file missing")
        next
      }
      terra::writeRaster(heterogeneity, outFile, overwrite = TRUE)
      message("  Saved ", key, ": ", outFile)
      outFiles[[key]] <- outFile
    }
  }

  message("\nDone.")
  invisible(outFiles)
}
