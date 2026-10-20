#' Closed-form coordinate conversions needed for the Germany data, independent of GDAL/PROJ
#'
#' Used as GROUND TRUTH to validate the library transformations (and as the fallback when they disagree),
#' because on EVE's full SpaDES session `st_transform()` returned wrong coordinates (swapped x/y for
#' EPSG:3035, nonsense for a PROJ string) that a bounding-box check alone cannot characterise
#' (DECISIONS.md, 2026-10-05). Two steps are implemented, both on GRS80/WGS84 (the two agree to well under
#' a metre here, as PROJ itself assumes):
#'  * UTM zone 32N (EPSG:25832) -> lon/lat: Krueger series (Karney 2011), accurate to sub-millimetre.
#'  * lon/lat -> ETRS89-LAEA (EPSG:3035): Lambert azimuthal equal-area, ellipsoidal (Snyder 1987;
#'    EPSG Guidance Note 7-2, section 3.2.4.1), lat0 = 52, lon0 = 10, FE = 4321000, FN = 3210000.

grs80A <- 6378137
grs80F <- 1 / 298.257222101
grs80E2 <- 2 * grs80F - grs80F^2

#' UTM zone 32N (EPSG:25832) easting/northing -> lon/lat (degrees)
#' @param E,N Numeric vectors (m).
#' @return Matrix with columns lon, lat.
utm32ToLonLat <- function(E, N) {
  n <- grs80F / (2 - grs80F); k0 <- 0.9996; lon0 <- 9 * pi / 180
  A <- grs80A / (1 + n) * (1 + n^2 / 4 + n^4 / 64)
  b <- c(n / 2 - 2 * n^2 / 3 + 37 * n^3 / 96 - n^4 / 360,
         n^2 / 48 + n^3 / 15 - 437 * n^4 / 1440,
         17 * n^3 / 480 - 37 * n^4 / 840,
         4397 * n^4 / 161280)
  d <- c(2 * n - 2 * n^2 / 3 - 2 * n^3 + 116 * n^4 / 45,
         7 * n^2 / 3 - 8 * n^3 / 5 - 227 * n^4 / 45,
         56 * n^3 / 15 - 136 * n^4 / 35,
         4279 * n^4 / 630)
  xi <- N / (k0 * A); eta <- (E - 500000) / (k0 * A)
  xi0 <- xi; eta0 <- eta
  for (j in 1:4) {
    xi0 <- xi0 - b[j] * sin(2 * j * xi) * cosh(2 * j * eta)
    eta0 <- eta0 - b[j] * cos(2 * j * xi) * sinh(2 * j * eta)
  }
  chi <- asin(sin(xi0) / cosh(eta0))
  phi <- chi
  for (j in 1:4) phi <- phi + d[j] * sin(2 * j * chi)
  lam <- lon0 + atan2(sinh(eta0), cos(xi0))
  cbind(lon = lam * 180 / pi, lat = phi * 180 / pi)
}

#' lon/lat (degrees) -> ETRS89-LAEA Europe (EPSG:3035) easting/northing (m)
#' @param lon,lat Numeric vectors (degrees).
#' @return Matrix with columns x (easting), y (northing).
lonLatToLAEA <- function(lon, lat) {
  e2 <- grs80E2; e <- sqrt(e2); a <- grs80A
  qf <- function(phi) { s <- sin(phi)
    (1 - e2) * (s / (1 - e2 * s^2) - (1 / (2 * e)) * log((1 - e * s) / (1 + e * s))) }
  phi <- lat * pi / 180; lam <- lon * pi / 180
  phi0 <- 52 * pi / 180; lam0 <- 10 * pi / 180
  q <- qf(phi); qp <- qf(pi / 2); q0 <- qf(phi0)
  Rq <- a * sqrt(qp / 2); beta <- asin(q / qp); beta0 <- asin(q0 / qp)
  m1 <- cos(phi0) / sqrt(1 - e2 * sin(phi0)^2)
  D <- a * m1 / (Rq * cos(beta0))
  B <- Rq * sqrt(2 / (1 + sin(beta0) * sin(beta) + cos(beta0) * cos(beta) * cos(lam - lam0)))
  cbind(x = 4321000 + B * D * cos(beta) * sin(lam - lam0),
        y = 3210000 + (B / D) * (cos(beta0) * sin(beta) - sin(beta0) * cos(beta) * cos(lam - lam0)))
}

#' Independent reference EPSG:3035 coordinates for an sf object (points, or polygon centroids)
#'
#' @param x sf object in EPSG:4326 or EPSG:25832.
#' @return Numeric matrix (x, y) with one row per feature, or NULL if the CRS is not supported.
analyticLAEA <- function(x) {
  epsg <- tryCatch(sf::st_crs(x)$epsg, error = function(e) NA)
  if (is.na(epsg) || !epsg %in% c(4326, 25832)) return(NULL)
  g <- sf::st_geometry(x)
  xy <- if (all(sf::st_geometry_type(g) == "POINT")) sf::st_coordinates(g)[, 1:2, drop = FALSE] else
    sf::st_coordinates(suppressWarnings(sf::st_centroid(g)))[, 1:2, drop = FALSE]
  ll <- if (epsg == 4326) xy else utm32ToLonLat(xy[, 1], xy[, 2])
  lonLatToLAEA(ll[, 1], ll[, 2])
}
