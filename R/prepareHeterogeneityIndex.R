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
                                        studyArea, recordsBufferM = 1000, force = FALSE) {
  if (missing(studyArea) || is.null(studyArea)) stop("prepareHeterogeneityIndex(): studyArea (buildStudyArea()) is required.")

  # definition of the index the cached files must have been made with (see computeHeterogeneityIndex(), DECISIONS.md 2026-10-20); change it when the definition changes
  workingArea <- studyAreaWorking(studyArea, recordsBufferM)                                  # the study area plus the buffer of the habitat/landscape scales
  heterogeneityVersion <- paste0("v2 2026-10-20: no rare-category threshold; 0 where no mapped class; without hedges; land cover categories: ", paste(names(landcoverCategoriesCorine()), collapse = ","), "; NA outside the study area: ", studyAreaFingerprint(studyArea, recordsBufferM))

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

      # a cached file counts only if it was made with the CURRENT definition of the index (sidecar <file>.version); older files are recomputed
      versionFile <- paste0(outFile, ".version")
      versionOk <- file.exists(versionFile) && identical(readLines(versionFile, warn = FALSE)[1], heterogeneityVersion)
      if (!force && versionOk && isValidRasterFile(outFile)) {
        message(key, " (landscape_heterogeneity) cache hit -- skipping")
        outFiles[[key]] <- outFile
        next
      }
      if (!force && !versionOk && file.exists(outFile)) message(key, " (landscape_heterogeneity) cached file is from an older definition of the index -- recomputing")

      message(key, " (landscape_heterogeneity) needs (re)computation")
      heterogeneity <- computeHeterogeneityIndex(year = yr, scaleDir = scaleDir, scaleName = s$scaleName, studyArea = workingArea)
      if (is.null(heterogeneity)) {
        message("  Skipped ", key, " -- required land use/land cover file missing")
        next
      }
      terra::writeRaster(heterogeneity, outFile, overwrite = TRUE)
      writeLines(heterogeneityVersion, versionFile)
      message("  Saved ", key, ": ", outFile)
      outFiles[[key]] <- outFile
    }
  }

  message("\nDone.")
  invisible(outFiles)
}
