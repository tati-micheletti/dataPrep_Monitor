#' Make sf initialise GDAL/PROJ BEFORE terra does any GDAL work in this R process
#'
#' On EVE (GDAL 3.10.3, PROJ 9.4.1, sf 1.1.3, terra 1.9.50) `sf::st_transform(., 3035)` returned x and y
#' SWAPPED whenever terra had already opened a raster/vector in the same process, but was correct when sf
#' made its first transformation first (minimal reprex: tools/reprexOrder.R; DECISIONS.md, 2026-10-05).
#' One tiny throw-away transformation early in the process avoids it. Cheap and harmless. It does NOT
#' replace the validation in transformToLAEA(), which stays the real safeguard.
#'
#' @return Invisibly TRUE if the warm-up transformation gave easting < northing order as expected for
#'   Germany in EPSG:3035 (x ~ 4.0-4.7 million, y ~ 2.7-3.6 million), FALSE if swapped, NA on error.
warmUpSfProj <- function() {
  ok <- tryCatch({
    p <- sf::st_as_sf(data.frame(lon = 11.5, lat = 48.1), coords = c("lon", "lat"), crs = 4326)
    xy <- sf::st_coordinates(sf::st_transform(p, 3035))
    xy[1, 1] > xy[1, 2]
  }, error = function(e) NA)
  if (isFALSE(ok)) message("warmUpSfProj(): sf already returns swapped x/y in this process (terra initialised first?)")
  invisible(ok)
}
