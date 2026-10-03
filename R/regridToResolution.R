#' Does a raster already carry the target EPSG code?
#' @param x SpatRaster.
#' @param targetCRS Character, e.g. "EPSG:3035".
#' @return Logical.
crsIsTarget <- function(x, targetCRS) {
  code <- terra::crs(x, describe = TRUE)$code
  !is.na(code) && identical(as.character(code), sub("^EPSG:", "", targetCRS))
}

#' Re-grid a raster to an exact cell size (and, if needed, the target CRS)
#'
#' Used right after `terra::aggregate()`, which can only reach multiples of the source
#' cell size (e.g. 23 x 30 m = 690 m, not 700 m). When the raster is ALREADY in the
#' target CRS only the cell size changes, so we `resample()` onto a template grid.
#' `terra::project()` would also work, but it asks GDAL/PROJ for a lon/lat "area of
#' interest" derived from the raster's extent, and the older GDAL/PROJ on EVE (3.10.3 /
#' 9.4.1) reject that for big LAEA rasters ("[project] area of interest not accepted",
#' GDAL: "Invalid dfSouthLatitudeDeg"). Verified with tools/probeRegrid.R on EVE.
#' A raster in a different CRS still goes through `project()`.
#'
#' @param x SpatRaster.
#' @param targetRes Numeric. Target cell size (m).
#' @param targetCRS Character. Target CRS, e.g. "EPSG:3035".
#' @param method Character. "bilinear" (continuous data) or "near" (categorical).
#' @return SpatRaster at `targetRes` in `targetCRS`.
regridToResolution <- function(x, targetRes, targetCRS, method = "bilinear") {
  if (!crsIsTarget(x, targetCRS)) {
    return(terra::project(x, targetCRS, res = targetRes, method = method))
  }
  terra::crs(x) <- targetCRS
  # Same grid layout project() produces: anchored at the lower-left corner, a whole number
  # of cells. The extent is built explicitly (not via rast(ext, resolution=)) because terra
  # would otherwise STRETCH the cells to fit the extent (e.g. 698 m instead of 700 m).
  e <- as.vector(terra::ext(x))                      # xmin, xmax, ymin, ymax
  nc <- max(1, round((e[2] - e[1]) / targetRes))
  nr <- max(1, round((e[4] - e[3]) / targetRes))
  template <- terra::rast(terra::ext(e[1], e[1] + nc * targetRes, e[3], e[3] + nr * targetRes),
                          resolution = targetRes, crs = targetCRS)
  terra::resample(x, template, method = method)
}

#' Reproject to the target CRS only when the raster is not already in it
#'
#' Same reason as `regridToResolution()`: `terra::project()` of a raster that is already
#' in the target CRS is a no-op in meaning but still triggers the failing area-of-use
#' check on EVE.
#'
#' @param x SpatRaster.
#' @param targetCRS Character, e.g. "EPSG:3035".
#' @param method Character. "near" for categorical data.
#' @return SpatRaster in `targetCRS`.
projectIfNeeded <- function(x, targetCRS, method = "near") {
  if (crsIsTarget(x, targetCRS)) return(x)
  terra::project(x, targetCRS, method = method)
}
