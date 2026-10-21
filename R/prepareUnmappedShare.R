#' Compute the unmapped share (`unmapped_other`) for all years and scales
#'
#' Same pattern as `prepareHeterogeneityIndex()`: one file per year and scale (`unmapped_other_<year>_<scaleName>_<scale>.tif`), a sidecar `<file>.version` with the definition
#' (including the land-cover categories and the study area), so a file made with other categories or for another area is recomputed.
#'
#' @param processedRoot Character. `predictors/processed`.
#' @param landuseYears Integer vector of years.
#' @param habitatResolutions,landscapeResolutions Numeric vectors (m).
#' @param studyArea SpatVector from `buildStudyArea()`. @param recordsBufferM Numeric. Buffer (m) of the habitat and landscape scales.
#' @param force Logical. Recompute even if a current file exists.
#' @return Invisibly, a named list of output file paths, keyed `"<year>_<scaleName>_<resM>"`.
prepareUnmappedShare <- function(processedRoot, landuseYears, habitatResolutions, landscapeResolutions, studyArea, recordsBufferM = 1000, force = FALSE) {
  if (missing(studyArea) || is.null(studyArea)) stop("prepareUnmappedShare(): studyArea (buildStudyArea()) is required.")
  workingArea <- studyAreaWorking(studyArea, recordsBufferM)
  unmappedVersion <- paste0("v1 2026-10-21: 1 - (land use without hedges + land cover categories: ", paste(names(landcoverCategoriesCorine()), collapse = ","), "), clamped 0-1; NA outside the study area: ",
                            studyAreaFingerprint(studyArea, recordsBufferM))
  scales <- c(lapply(habitatResolutions, function(r) list(scaleName = "habitat", res = r)), lapply(landscapeResolutions, function(r) list(scaleName = "landscape", res = r)))
  message("Processing unmapped_other for years: ", paste(landuseYears, collapse = ", "))
  outFiles <- list()
  for (yr in landuseYears) for (s in scales) {
    scaleDir <- file.path(processedRoot, scaleLabel(s$res))
    outFile <- file.path(scaleDir, paste0("unmapped_other_", yr, "_", s$scaleName, "_", basename(scaleDir), ".tif"))
    key <- paste0(yr, "_", s$scaleName, "_", s$res); versionFile <- paste0(outFile, ".version")
    versionOk <- file.exists(versionFile) && identical(readLines(versionFile, warn = FALSE)[1], unmappedVersion)
    if (!force && versionOk && isValidRasterFile(outFile)) { message(key, " (unmapped_other) cache hit -- skipping"); outFiles[[key]] <- outFile; next }
    message(key, " (unmapped_other) needs (re)computation")
    r <- computeUnmappedShare(yr, scaleDir, s$scaleName, workingArea)
    if (is.null(r)) { message("  Skipped ", key, " -- required land use/land cover file missing"); next }
    terra::writeRaster(r, outFile, overwrite = TRUE)
    writeLines(unmappedVersion, versionFile)
    message("  Saved ", key, ": ", outFile)
    outFiles[[key]] <- outFile
  }
  message("\nDone.")
  invisible(outFiles)
}
