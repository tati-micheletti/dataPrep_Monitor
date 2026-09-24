#' Compute the 3-category land cover proportion layers for one CORINE snapshot
#'
#' Input CORINE rasters are already EPSG:3035 at 100m; this reprojects
#' only if needed and computes built_up/trees/water proportion layers at
#' both habitat and landscape scales.
#'
#' @param corineYear Integer. One of 2006, 2012, 2018.
#' @param landcoverRawDir Character. Directory containing raw CORINE rasters.
#' @param habitatOutputDir Character. Directory for habitat-scale outputs.
#' @param landscapeOutputDir Character. Directory for landscape-scale outputs.
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param habitatResolutionM Numeric. Habitat scale resolution in metres.
#' @param landscapeResolutionM Numeric. Landscape scale resolution in metres.
#' @param force Logical. If TRUE, recompute and overwrite even if a valid
#'   cached output already exists (e.g. a bug was found in the raw data).
#' @return Invisibly, a list with `habitat` and `landscape` output paths.
computeLandcover <- function(corineYear, landcoverRawDir, habitatOutputDir,
                              landscapeOutputDir, targetCRS,
                              habitatResolutionM, landscapeResolutionM,
                              force = FALSE) {

  rawFile <- file.path(landcoverRawDir, corineRawFilename(corineYear))
  if (!file.exists(rawFile)) {
    stop("CORINE file not found: ", rawFile)
  }

  outHabitat <- file.path(habitatOutputDir, paste0("landcover_", corineYear, "_habitat.tif"))
  outLandscape <- file.path(landscapeOutputDir, paste0("landcover_", corineYear, "_landscape.tif"))

  if (!force && isValidRasterFile(outHabitat) && isValidRasterFile(outLandscape)) {
    message("Cache hit -- skipping CORINE ", corineYear)
    return(invisible(list(habitat = outHabitat, landscape = outLandscape)))
  }

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

  habitatLayers <- list()
  landscapeLayers <- list()

  for (catName in names(categories)) {
    codes <- categories[[catName]]
    message("    ", catName, " (codes: ", paste(codes, collapse = ", "), ")")
    habitatLayers[[catName]] <- makeCategoryProportionLayer(lc, codes, habitatResolutionM, catName, targetCRS)
    landscapeLayers[[catName]] <- makeCategoryProportionLayer(lc, codes, landscapeResolutionM, catName, targetCRS)
  }

  habitatStack <- terra::rast(habitatLayers)
  landscapeStack <- terra::rast(landscapeLayers)

  terra::writeRaster(habitatStack, outHabitat, overwrite = TRUE)
  terra::writeRaster(landscapeStack, outLandscape, overwrite = TRUE)
  message("  Saved habitat:   ", outHabitat)
  message("  Saved landscape: ", outLandscape)

  invisible(list(habitat = outHabitat, landscape = outLandscape))
}
