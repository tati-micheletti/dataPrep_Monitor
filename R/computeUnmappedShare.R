#' Share of a cell that none of the mapped classes describes (`unmapped_other`)
#'
#' `unmapped_other = 1 - (sum of the land-use proportions without hedges + sum of the land-cover proportions)`, limited to 0..1, per cell of one year and scale. The land-use
#' proportions (crop map: crops, grassland, fallow, vines, hops, orchards) and the CORINE categories (`landcoverCategoriesCorine()`) describe different things from different sources,
#' so they can overlap (the sum can exceed 1: the share is then 0) and they leave gaps: bare ground, dunes, tidal flats, coastal waters, and farmland cells where the crop map has no
#' class (measured 2026-10-20: 1.5% of the 200 m cells of Germany had less than 5% of their area mapped, 9.2% less than half). The residual lets a model see these cells instead of
#' reading them as a strange all-zero pattern. `hedges` is not a share of the cell's area and is not part of the sum.
#' Land cover is resampled onto the land-use grid first (same fix as in `computeHeterogeneityIndex()`). Cells outside `studyArea` are NA, none inside.
#'
#' @param year Integer. Land use year.
#' @param scaleDir Character. The scale's processed-covariates directory (`.../processed/scale_02`).
#' @param scaleName Character. `"habitat"` or `"landscape"`.
#' @param studyArea SpatVector, the study area widened by the buffer of the scale (`studyAreaWorking()`). REQUIRED.
#' @return SpatRaster, one layer named `"unmapped_other"`, or NULL if the land use/land cover files are missing.
computeUnmappedShare <- function(year, scaleDir, scaleName, studyArea) {
  if (missing(studyArea) || is.null(studyArea)) stop("computeUnmappedShare(): studyArea (the study area with its buffer) is required.")
  corineYr <- corineYear(year)
  resSuffix <- basename(scaleDir)
  luFile <- file.path(scaleDir, paste0("landuse_", year, "_", scaleName, "_", resSuffix, ".tif"))
  lcFile <- file.path(scaleDir, paste0("landcover_", corineYr, "_", scaleName, "_", resSuffix, ".tif"))
  if (!file.exists(luFile) || !file.exists(lcFile)) {
    warning("Land use or land cover file missing for ", year, " (unmapped share): ", luFile, " / ", lcFile)
    return(NULL)
  }
  lu <- terra::rast(luFile); lc <- terra::rast(lcFile)
  lu <- lu[[setdiff(names(lu), "hedges")]]
  lc <- terra::resample(lc, lu[[1]], method = "bilinear")
  mapped <- sum(c(sum(lu, na.rm = TRUE), sum(lc, na.rm = TRUE)), na.rm = TRUE)
  out <- terra::clamp(1 - mapped, lower = 0, upper = 1)
  out <- terra::mask(out, studyArea, touches = TRUE)                           # NA outside the study area
  names(out) <- "unmapped_other"
  out
}
