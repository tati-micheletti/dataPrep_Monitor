#' Compute the 3-category land cover proportion layers for one CORINE snapshot
#'
#' Input CORINE rasters are already EPSG:3035 at 100m; this reprojects
#' only if needed and computes built_up/trees/water proportion layers at
#' every distinct habitat/landscape resolution actually needed (usually
#' just one of each -- the shared default -- but more than one when a
#' species has its own `resolution_m` override; see DECISIONS.md's
#' 2026-09-28 entry).
#'
#' @param corineYear Integer. One of 2006, 2012, 2018.
#' @param landcoverRawDir Character. Directory containing raw CORINE rasters.
#' @param processedRoot Character. `predictors/processed` directory
#'   (without the scale_X leaf -- each resolution's own leaf, via
#'   `scaleLabel()`, is appended internally).
#' @param habitatResolutions,landscapeResolutions Numeric vectors. Every
#'   distinct resolution (m) actually needed at that scale, across all
#'   species (usually a length-1 vector, the shared default).
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param force Logical. If TRUE, recompute and overwrite even if a valid
#'   cached output already exists (e.g. a bug was found in the raw data).
#' @return Invisibly, a named list of output file paths, keyed
#'   `"habitat_<resM>"`/`"landscape_<resM>"`.
computeLandcover <- function(corineYear, landcoverRawDir, processedRoot,
                              habitatResolutions, landscapeResolutions,
                              targetCRS, force = FALSE) {

  rawFile <- file.path(landcoverRawDir, corineRawFilename(corineYear))
  if (!file.exists(rawFile)) {
    stop("CORINE file not found: ", rawFile)
  }

  # One entry per distinct resolution actually needed, at each scale --
  # usually 2 total (one habitat, one landscape default), more when a
  # species has its own resolution_m override.
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

  # Resolution appended to the filename itself (second safety layer beyond
  # the containing scaleLabel()-named folder) -- see aggregateAndSave.R.
  outFiles <- lapply(scales, function(s) {
    file.path(s$dir, paste0("landcover_", corineYear, "_", s$scaleName, "_", basename(s$dir), ".tif"))
  })

  # Independent per-scale-entry cache check (Lisa Hildebrand's v2 pattern,
  # generalized beyond exactly 2 scales) -- an entry whose cached output is
  # already valid is skipped even when another entry needs recomputing.
  need <- vapply(outFiles, function(f) force || !isValidRasterFile(f), logical(1))

  if (!any(need)) {
    message("Cache hit -- skipping CORINE ", corineYear)
    return(invisible(outFiles))
  }
  for (nm in names(scales)[!need]) message(nm, " cache hit -- skipping")
  for (nm in names(scales)[need]) message(nm, " needs (re)computation")

  message("Loading CORINE ", corineYear, ": ", basename(rawFile))
  lc <- terra::setMinMax(terra::rast(rawFile))

  # Input is already EPSG:3035 at 100m -- no reprojection needed, just
  # confirm the CRS matches target
  if (terra::crs(lc, describe = TRUE)$code != "3035") {
    message("Reprojecting to ", targetCRS, "...")
    lc <- terra::project(lc, targetCRS, method = "near")
  }

  categories <- landcoverCategoriesCorine()
  message("Computing ", length(categories), " category proportions...")

  layersByScale <- stats::setNames(vector("list", length(scales)), names(scales))
  neededNames <- names(scales)[need]

  for (catName in names(categories)) {
    codes <- categories[[catName]]
    message("    ", catName, " (codes: ", paste(codes, collapse = ", "), ")")
    for (nm in neededNames) {
      s <- scales[[nm]]
      layersByScale[[nm]][[catName]] <- makeCategoryProportionLayer(lc, codes, s$res, catName, targetCRS)
    }
  }

  for (nm in neededNames) {
    terra::writeRaster(terra::rast(layersByScale[[nm]]), outFiles[[nm]], overwrite = TRUE)
    message("  Saved ", nm, ": ", outFiles[[nm]])
  }

  invisible(outFiles)
}
