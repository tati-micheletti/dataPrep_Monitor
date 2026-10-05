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

#' Do most coordinates fall in Germany's EPSG:3035 bounding box?
#' @param xy Numeric matrix, columns x, y.
#' @return Logical.
laeaLooksLikeGermany <- function(xy) {
  mean(xy[, 1] > 3.9e6 & xy[, 1] < 4.8e6 & xy[, 2] > 2.6e6 & xy[, 2] < 3.7e6, na.rm = TRUE) >= 0.5
}

#' Do most coordinates fall in Germany's EPSG:3035 box only AFTER swapping x and y?
#' @param xy Numeric matrix, columns x, y.
#' @return Logical.
laeaLooksSwapped <- function(xy) {
  mean(xy[, 2] > 3.9e6 & xy[, 2] < 4.8e6 & xy[, 1] > 2.6e6 & xy[, 1] < 3.7e6, na.rm = TRUE) >= 0.5
}

#' Transform an sf object to ETRS89-LAEA (EPSG:3035), robust to axis-order problems
#'
#' Tries independent methods in turn -- an explicit PROJ string, `EPSG:3035`, and `terra` -- and uses the
#' FIRST whose result falls in Germany's bounding box (the data are German by construction). On EVE's full
#' SpaDES session a plain `st_transform(x, 3035)` returned swapped x/y and a PROJ-string transform returned
#' nonsense for the shapefile, although a plain R session loading the same packages was fine (see
#' DECISIONS.md, 2026-10-05), so the cause could not be isolated from outside. This function does not depend
#' on it, logs which method was used, and when none works it stops with the input CRS/bounding box and every
#' method's result so the session can be diagnosed from the log.
#'
#' @param x sf object (any geometry) in a known CRS (e.g. 4326 or 25832).
#' @return sf object labelled EPSG:3035 with easting/northing numbers.
transformToLAEA <- function(x) {
  methods <- list(
    projString = function() sf::st_transform(x, laeaCRSProj4),
    epsg3035 = function() sf::st_transform(x, 3035),
    terra = function() sf::st_as_sf(terra::project(terra::vect(x), "EPSG:3035")))
  rng <- function(v) paste(round(range(v, na.rm = TRUE)), collapse = "..")
  log <- character()
  swapCandidate <- NULL
  for (nm in names(methods)) {
    out <- tryCatch(suppressWarnings(methods[[nm]]()), error = function(e) {
      log <<- c(log, paste0(nm, ": ERROR ", conditionMessage(e))); NULL })
    if (is.null(out)) next
    xy <- tryCatch(sf::st_coordinates(suppressWarnings(sf::st_centroid(sf::st_geometry(out))))[, 1:2, drop = FALSE],
                   error = function(e) NULL)
    if (is.null(xy)) { log <- c(log, paste0(nm, ": no coordinates")); next }
    ok <- laeaLooksLikeGermany(xy)
    sw <- !ok && laeaLooksSwapped(xy)
    log <- c(log, sprintf("%s: x %s, y %s -> %s", nm, rng(xy[, 1]), rng(xy[, 2]),
                          if (ok) "OK" else if (sw) "SWAPPED (fits Germany after swapping x and y)" else "rejected"))
    if (sw && is.null(swapCandidate)) swapCandidate <- out
    if (ok) {
      message("transformToLAEA: using method '", nm, "' (", sum(grepl("rejected|ERROR", log)), " rejected before)")
      suppressWarnings(sf::st_crs(out) <- 3035)
      return(out)
    }
  }
  if (!is.null(swapCandidate)) {
    # A consistent x/y swap (northing/easting returned instead of easting/northing): correct it, loudly.
    message("transformToLAEA: the transformation returned x and y SWAPPED in this session (axis order); ",
            "swapping them back. Log: ", paste(log, collapse = " | "))
    out <- swapCandidate
    sf::st_geometry(out) <- sf::st_geometry(out) * matrix(c(0, 1, 1, 0), 2, 2)
    suppressWarnings(sf::st_crs(out) <- 3035)
    return(out)
  }
  bb <- tryCatch(sf::st_bbox(x), error = function(e) NULL)
  stop("[COORDINATE ERROR] transformToLAEA(): no method produced coordinates inside Germany's EPSG:3035 box.
",
       "  input CRS: ", tryCatch(sf::st_crs(x)$input, error = function(e) "?"),
       " | input bbox: ", if (is.null(bb)) "?" else paste(round(bb, 2), collapse = " "), "
  ",
       paste(log, collapse = "
  "), "
  env: OSR_DEFAULT_AXIS_MAPPING_STRATEGY='",
       Sys.getenv("OSR_DEFAULT_AXIS_MAPPING_STRATEGY"), "' PROJ_DATA='", Sys.getenv("PROJ_DATA"),
       "' PROJ_LIB='", Sys.getenv("PROJ_LIB"), "' sf ", as.character(utils::packageVersion("sf")),
       " PROJ ", sf::sf_extSoftVersion()[["PROJ"]], " GDAL ", sf::sf_extSoftVersion()[["GDAL"]], call. = FALSE)
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
