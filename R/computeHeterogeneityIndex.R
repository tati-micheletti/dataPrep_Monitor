#' Compute the Simpson landscape-heterogeneity index for one year/scale
#'
#' Combines the already-scale-aggregated land cover (`built_up`/`trees`/
#' `water`, 3 layers) and land use (14 crop-type categories) proportion
#' layers for a given year and scale into one per-cell Simpson diversity
#' index (`1 - sum(p_i^2)`), so diversity reflects composition AT the
#' scale being modeled rather than a separately-aggregated native-
#' resolution value.
#'
#' Any category below 5% of a cell's total (matching the existing
#' `changeThresh = 0.05` convention used elsewhere in the pipeline, since
#' no single universal external threshold exists in the literature) is
#' zeroed out and the remaining proportions renormalized before computing
#' the index, so noisy rare categories don't drive the result.
#'
#' Land cover and land use share the same nominal resolution per scale but
#' have a small grid-origin offset (different source datasets) -- land
#' cover is resampled onto land use's grid first, exactly the fix already
#' used in `loadCovariates.R`/`loadHabitatCovariates.R`.
#'
#' @param year Integer. Land use year.
#' @param scaleDir Character. The scale's processed-covariates directory
#'   (e.g. `.../processed/scale_1`) -- both `landuse_*`/`landcover_*` for
#'   this scale are expected there.
#' @param scaleName Character. `"habitat"` or `"landscape"`.
#' @param rareThreshold Numeric. Proportions below this are zeroed out
#'   before renormalizing (default 0.05).
#' @return SpatRaster, one layer named `"landscape_heterogeneity"`, or
#'   NULL if the required land use/land cover files are missing.
computeHeterogeneityIndex <- function(year, scaleDir, scaleName, rareThreshold = 0.05) {

  corineYr <- corineYear(year)
  resSuffix <- basename(scaleDir)

  luFile <- file.path(scaleDir, paste0("landuse_", year, "_", scaleName, "_", resSuffix, ".tif"))
  if (!file.exists(luFile)) {
    warning("Land use file missing for ", year, " (heterogeneity index): ", luFile)
    return(NULL)
  }
  lcFile <- file.path(scaleDir, paste0("landcover_", corineYr, "_", scaleName, "_", resSuffix, ".tif"))
  if (!file.exists(lcFile)) {
    warning("Land cover file missing for CORINE ", corineYr, " (heterogeneity index): ", lcFile)
    return(NULL)
  }

  lu <- terra::rast(luFile)
  lc <- terra::rast(lcFile)

  # Resolve the same ~grid-origin offset loadCovariates.R already documents
  # between land use and land cover at the same nominal resolution.
  lc <- terra::resample(lc, lu[[1]], method = "bilinear")
  terra::values(lc) <- terra::values(lc)

  allLayers <- c(as.list(lu), as.list(lc))
  combined <- combineLayersSafely(allLayers)

  simpsonFun <- function(x) {
    x[x < rareThreshold] <- 0
    total <- sum(x, na.rm = TRUE)
    if (total == 0 || is.na(total)) return(NA_real_)
    p <- x / total
    1 - sum(p^2, na.rm = TRUE)
  }

  heterogeneity <- terra::app(combined, fun = simpsonFun)
  names(heterogeneity) <- "landscape_heterogeneity"
  heterogeneity
}
