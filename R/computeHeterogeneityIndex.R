#' Compute the Simpson landscape-heterogeneity index for one year/scale
#'
#' Combines the already-scale-aggregated land cover (`built_up`/`trees`/
#' `water`, 3 layers) and land use (14 crop-type categories) proportion
#' layers for a given year and scale into one per-cell Simpson diversity
#' index (`1 - sum(p_i^2)`), so diversity reflects composition AT the
#' scale being modeled rather than a separately-aggregated native-
#' resolution value.
#'
#' `hedges` is NOT one of the classes (it is no share of the area and only exists for some years, so including it made the index change its definition in 2017, 2022 and 2024).
#'
#' VERSION 2 (2026-10-20, see DECISIONS.md): there is NO rare-category
#' threshold any more (`rareThreshold = 0`). Version 1 zeroed every category
#' below 5% of a cell and returned NA when nothing was left; that left holes
#' (NA covariate = NA prediction) in about 0.75% of German 200 m cells --
#' bare rock, tidal flats, lagoons, heath, and cells whose classes are all
#' below 5% -- in every map of a species that uses this layer. The Simpson
#' index squares the proportions, so a rare category hardly contributes
#' anyway (a 3% class adds 0.0009); dropping the threshold changed the values
#' by 0.019 on average (0-1 scale; correlation 0.987 with version 1, 2025).
#' A cell in which NO mapped category is present (total = 0) has no diversity
#' among the mapped classes and gets 0; a cell with no data at all (every
#' layer NA, i.e. outside the covariate coverage) stays NA. The remaining
#' proportions are renormalized to sum to 1, as before. Outside the study area the layer is NA (see `studyArea`).
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
#'   before renormalizing. Default 0 = no threshold (version 2; version 1
#'   used 0.05 and produced NA where nothing was left).
#' @param studyArea SpatVector, the study area widened by the buffer of the scale (`studyAreaWorking()`). REQUIRED: cells outside it are set to NA (the layer must be NA outside the
#'   study area, so that records and predictions outside it are excluded explicitly and not by accident; version 1 got this only because its NA also covered everything outside
#'   Germany) and complete (no NA) inside.
#' @return SpatRaster, one layer named `"landscape_heterogeneity"`, or
#'   NULL if the required land use/land cover files are missing.
computeHeterogeneityIndex <- function(year, scaleDir, scaleName, studyArea, rareThreshold = 0) {
  if (missing(studyArea) || is.null(studyArea)) stop("computeHeterogeneityIndex(): studyArea (the German outline) is required.")

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
  lu <- lu[[setdiff(names(lu), "hedges")]]        # hedges is not a share of the cell's area and exists only for some years: counting it would make the index jump in those years (2017, 2022, 2024)
  lc <- terra::rast(lcFile)

  # Resolve the same ~grid-origin offset loadCovariates.R already documents
  # between land use and land cover at the same nominal resolution.
  lc <- terra::resample(lc, lu[[1]], method = "bilinear")
  terra::values(lc) <- terra::values(lc)

  allLayers <- c(as.list(lu), as.list(lc))
  combined <- combineLayersSafely(allLayers)

  simpsonFun <- function(x) {
    if (all(is.na(x))) return(NA_real_)          # no data at all (outside the covariate coverage): stays NA
    x[is.na(x)] <- 0
    x[x < rareThreshold] <- 0
    total <- sum(x)
    if (total == 0) return(0)                    # no mapped class in this cell: no diversity among the mapped classes
    p <- x / total
    1 - sum(p^2)
  }

  heterogeneity <- terra::app(combined, fun = simpsonFun)
  heterogeneity <- terra::mask(heterogeneity, studyArea, touches = TRUE)      # NA outside Germany; inside Germany simpsonFun() never returns NA (except where no layer has data)
  names(heterogeneity) <- "landscape_heterogeneity"
  heterogeneity
}
