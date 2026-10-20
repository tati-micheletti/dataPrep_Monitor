#' German national outline (GADM level 0) in EPSG:3035, and the study-area filter for point records
#'
#' The study area of the pipeline is Germany. Layers and records outside it must be removed EXPLICITLY, not as a side effect of an NA covariate
#' (before 2026-10-20 the NA of `landscape_heterogeneity` outside Germany was the only thing that kept records of neighbouring countries out of the
#' training tables; see DECISIONS.md). The outline is built from the lon/lat vertices with the closed-form `lonLatToLAEA()` (never through a GDAL/PROJ
#' transformation: the EPSG:3035 axis order of terra/sf transformations has come out swapped on EVE, DECISIONS.md 2026-10-05) and labelled with `crs`.
#' The same construction is used by models_Monitor (`uncOutlineLAEA()`), so "Germany" is the same polygon everywhere.
#'
#' @param gadmDir Character. Folder of the geodata cache (inputs/predictors/raw/gadm).
#' @param crs Character or CRS. Label of the result (the CRS of the rasters it is used with, EPSG:3035).
#' @return SpatVector, the German outline.
germanyOutlineLAEA <- function(gadmDir, crs = "EPSG:3035") {
  if (!requireNamespace("geodata", quietly = TRUE)) stop("germanyOutlineLAEA(): the 'geodata' package is needed for the national outline.")
  dir.create(gadmDir, recursive = TRUE, showWarnings = FALSE)
  g <- geodata::gadm(country = "DEU", level = 0, path = gadmDir)                  # lon/lat
  v <- terra::geom(g); xy <- lonLatToLAEA(v[, "x"], v[, "y"])
  out <- terra::vect(cbind(v[, c("geom", "part")], x = xy[, "x"], y = xy[, "y"], hole = v[, "hole"]), type = "polygons", crs = crs)
  e <- as.vector(terra::ext(out))
  if (!(e[1] > 3.9e6 && e[2] < 4.8e6 && e[3] > 2.6e6 && e[4] < 3.7e6))
    stop("German outline outside Germany's EPSG:3035 box: x ", round(e[1]), "-", round(e[2]), ", y ", round(e[3]), "-", round(e[4]))
  out
}

#' Which point records (EPSG:3035 x/y) lie inside the German outline?
#'
#' A point-in-polygon test of millions of records against the detailed outline takes tens of minutes (measured: 3 million points, more than 10 min), so the
#' outline is rasterised once on a grid of `res` m (cells touched by the outline count as inside, as in the masks of the layers and maps: `touches = TRUE`)
#' and the records look up their cell. The border is therefore accurate to about one cell (200 m by default, the habitat grid).
#'
#' @param xy Matrix or data.frame with the columns x and y (EPSG:3035, m).
#' @param outline SpatVector from `germanyOutlineLAEA()`.
#' @param res Numeric. Cell size (m) of the lookup grid.
#' @return Logical vector, TRUE = inside (or on the border of) Germany.
insideGermany <- function(xy, outline, res = 200) {
  xy <- as.matrix(xy[, c("x", "y")])
  if (!nrow(xy)) return(logical(0))
  tmpl <- terra::rast(terra::ext(outline), resolution = res, crs = terra::crs(outline))
  mk <- terra::rasterize(outline, tmpl, field = 1, background = NA, touches = TRUE)
  !is.na(terra::extract(mk, xy)[, 1])
}
