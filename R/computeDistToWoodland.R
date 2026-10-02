#' Compute the distance-to-woodland predictor for one CORINE snapshot
#'
#' Builds a binary forest mask directly from raw CORINE at its native
#' ~100m resolution (codes 23-25, the same closed-canopy-forest definition
#' `landcoverCategoriesCorine()`'s `trees` category already uses -- no
#' separate percent-cover threshold is needed here, since CORINE's own
#' class already encodes closed-canopy forest), runs `terra::distance()`
#' on that native-resolution mask (so distances reflect real geography,
#' not post-aggregation blur), then aggregates the resulting continuous
#' distance surface to every distinct habitat/landscape resolution
#' actually needed, mirroring `aggregateAndSave()`'s native -> aggregate
#' (mean) -> project pattern (used for elevation/slope).
#'
#' @param corineYear Integer. One of 2006, 2012, 2018.
#' @param landcoverRawDir Character. Directory containing raw CORINE rasters.
#' @param processedRoot Character. `predictors/processed` directory
#'   (without the scale_X leaf -- each resolution's own leaf, via
#'   `scaleLabel()`, is appended internally).
#' @param habitatResolutions,landscapeResolutions Numeric vectors. Every
#'   distinct resolution (m) actually needed at that scale, across all
#'   species.
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param force Logical. If TRUE, recompute and overwrite even if a valid
#'   cached output already exists.
#' @return Invisibly, a named list of output file paths, keyed
#'   `"habitat_<resM>"`/`"landscape_<resM>"`.
computeDistToWoodland <- function(corineYear, landcoverRawDir, processedRoot,
                                    habitatResolutions, landscapeResolutions,
                                    targetCRS, force = FALSE) {

  rawFile <- file.path(landcoverRawDir, corineRawFilename(corineYear))

  scales <- c(
    lapply(habitatResolutions, function(r) list(scaleName = "habitat", res = r)),
    lapply(landscapeResolutions, function(r) list(scaleName = "landscape", res = r))
  )
  scales <- lapply(scales, function(s) {
    s$dir <- file.path(processedRoot, scaleLabel(s$res))
    s
  })
  names(scales) <- vapply(scales, function(s) paste0(s$scaleName, "_", s$res), character(1))

  for (s in scales) dir.create(s$dir, recursive = TRUE, showWarnings = FALSE)

  outFiles <- lapply(scales, function(s) {
    file.path(s$dir, paste0("dist_to_woodland_", corineYear, "_", s$scaleName, "_", basename(s$dir), ".tif"))
  })

  need <- vapply(outFiles, function(f) force || !isValidRasterFile(f), logical(1))

  if (!any(need)) {
    message("Cache hit -- skipping dist_to_woodland for CORINE ", corineYear)
    return(invisible(outFiles))
  }
  for (nm in names(scales)[!need]) message(nm, " (dist_to_woodland) cache hit -- skipping")
  for (nm in names(scales)[need]) message(nm, " (dist_to_woodland) needs (re)computation")

  # Only needed when something must actually be computed.
  if (!file.exists(rawFile)) {
    stop("CORINE file not found: ", rawFile)
  }

  message("Loading CORINE ", corineYear, " for dist_to_woodland: ", basename(rawFile))
  lc <- terra::setMinMax(terra::rast(rawFile))

  if (terra::crs(lc, describe = TRUE)$code != "3035") {
    message("Reprojecting to ", targetCRS, "...")
    lc <- terra::project(lc, targetCRS, method = "near")
  }

  forestCodes <- landcoverCategoriesCorine()$trees
  message("Building native-resolution forest mask (codes: ", paste(forestCodes, collapse = ", "), ")...")
  forestMask <- terra::app(lc, function(x) {
    v <- rep(NA_real_, length(x))
    v[x %in% forestCodes] <- 1
    v
  })

  message("Computing distance to nearest forest cell...")
  distNative <- terra::distance(forestMask)
  names(distNative) <- "dist_to_woodland"
  # Force-materialize the computed (not read-from-disk) distance surface --
  # see combineLayersSafely.R's docstring / DECISIONS.md 2026-09-28 for why.
  terra::values(distNative) <- terra::values(distNative)

  neededNames <- names(scales)[need]
  for (nm in neededNames) {
    s <- scales[[nm]]
    fact <- round(s$res / terra::res(distNative)[1])
    distAgg <- terra::aggregate(distNative, fact = fact, fun = "mean", na.rm = TRUE)
    distProj <- terra::project(distAgg, targetCRS, res = s$res, method = "bilinear")
    names(distProj) <- "dist_to_woodland"
    terra::writeRaster(distProj, outFiles[[nm]], overwrite = TRUE)
    message("  Saved ", nm, ": ", outFiles[[nm]])
  }

  invisible(outFiles)
}
