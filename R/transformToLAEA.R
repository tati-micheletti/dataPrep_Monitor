#' ETRS89-LAEA Europe (EPSG:3035) as an explicit PROJ definition
#'
#' EPSG:3035 officially lists NORTHING before EASTING. Whether `sf::st_transform(x, 3035)` then returns
#' (easting, northing) or (northing, easting) depends on GDAL/PROJ's axis-order setting, which differs
#' between installations and can be changed by other loaded packages. On EVE's session the bird points
#' came out with x and y SWAPPED (x ~ 2.7-3.5 million, y ~ 4.0-4.7 million), so every raster lookup
#' missed and every row was dropped for "NA covariates" (see DECISIONS.md, 2026-10-05). A PROJ string has
#' no ambiguity: x is always easting.
#'
#' Same projection and ellipsoid as EPSG:3035 (ETRS89 and WGS84 agree to well under a metre here, and
#' PROJ itself applies a null transformation between them), verified identical to the metre against
#' `st_transform(x, 3035)` where that works.
laeaCRSProj4 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' Transform an sf object to ETRS89-LAEA without axis-order ambiguity
#'
#' @param x sf object.
#' @return sf object in EPSG:3035 (easting/northing numbers), labelled `EPSG:3035`.
transformToLAEA <- function(x) {
  out <- sf::st_transform(x, laeaCRSProj4)
  suppressWarnings(sf::st_crs(out) <- 3035)   # label only: same definition, numbers stay easting/northing
  out
}

#' Get easting/northing from an sf object, and STOP if they look swapped
#'
#' The study data are German, so in EPSG:3035 x must be about 4.0-4.7 million and y about 2.7-3.5
#' million. If most points fit that box only after swapping x and y (or do not fit at all), something
#' is wrong upstream: fail loudly instead of silently producing NA covariates.
#'
#' @param x sf object (points or centroids) in EPSG:3035.
#' @param what Character. Label for the error message.
#' @return Numeric matrix with columns x, y.
laeaCoordinates <- function(x, what = "points") {
  xy <- sf::st_coordinates(x)[, 1:2, drop = FALSE]
  colnames(xy) <- c("x", "y")
  inGermany <- xy[, "x"] > 3.9e6 & xy[, "x"] < 4.8e6 & xy[, "y"] > 2.6e6 & xy[, "y"] < 3.7e6
  swapped <- xy[, "y"] > 3.9e6 & xy[, "y"] < 4.8e6 & xy[, "x"] > 2.6e6 & xy[, "x"] < 3.7e6
  if (mean(swapped, na.rm = TRUE) > 0.5) {
    stop("[COORDINATE ERROR] ", what, ": x and y look SWAPPED (x range ",
         paste(round(range(xy[, "x"], na.rm = TRUE)), collapse = ".."), ", y range ",
         paste(round(range(xy[, "y"], na.rm = TRUE)), collapse = ".."), "). Expected x ~ 4.0-4.7e6, y ~ 2.7-3.5e6.",
         call. = FALSE)
  }
  if (mean(inGermany, na.rm = TRUE) < 0.5) {
    stop("[COORDINATE ERROR] ", what, ": most points fall outside Germany's EPSG:3035 bounding box (x range ",
         paste(round(range(xy[, "x"], na.rm = TRUE)), collapse = ".."), ", y range ",
         paste(round(range(xy[, "y"], na.rm = TRUE)), collapse = ".."), ").", call. = FALSE)
  }
  xy
}
