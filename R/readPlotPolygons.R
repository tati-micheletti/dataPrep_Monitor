#' Plot (sample area) polygons: sampleareas.csv first, the old shapefile as FALLBACK (2026-11-27)
#'
#' Decision by Tati, confirmed by Lukas Seemann (DDA, 2026-11-26): the locations of the sample areas come from dbird's `sampleareas.csv` (most recent, the older shapefiles are to be ignored);
#' `sampleareas.csv` lacks 509 plots of the old 2005-2024 list, so the shapefile (MhB_Probeflaechen_DE_S2637_epsg25832) stays as the fallback for plots that are not in it. Without this the plots
#' that are new in 2025-2026 (40 surveyed plots in 2025, 36 in 2026) have no coordinates and drop out of the landscape training tables (inner join on the route code) and out of the habitat absences.
#' For the plots in both sources the centroids agree to < 2.1 m (checked 2026-11-26); the function STOPS if they differ by more than `maxCentroidDiffM` for any plot (a sign of a wrong CRS or axis order).
#' The returned object keeps the shapefile's CRS (EPSG:25832) and its id column; plots taken from sampleareas.csv carry only the id (other shapefile attributes are NA, the preparation does not use them).
#' @param shpPath Character. Path to the Probeflaechen shapefile.
#' @param sampleAreasPath Character or NULL. Path to dbird's sampleareas.csv (columns code, protocol, area = EWKB hex polygon, EPSG:4326). NULL / NA / "" = the shapefile only (the old behaviour).
#' @param idCols Character. Candidate id columns of the shapefile (the first present is used).
#' @param maxCentroidDiffM Numeric. Largest accepted centroid difference (m) between the two sources for a plot in both.
#' @return sf data.frame of polygons.
readPlotPolygons <- function(shpPath, sampleAreasPath = NULL, idCols = c("ROUTENCODE", "Routencode", "routencode", "ROUTE_CODE", "SITE_ID"), maxCentroidDiffM = 50) {
  pf <- sf::st_read(shpPath, quiet = TRUE)
  if (is.null(sampleAreasPath) || is.na(sampleAreasPath) || !nzchar(sampleAreasPath)) return(pf)
  if (!file.exists(sampleAreasPath)) stop("readPlotPolygons(): ", sampleAreasPath, " not found")
  idCol <- intersect(idCols, names(pf))[1]; if (is.na(idCol)) stop("readPlotPolygons(): the shapefile has none of the id columns ", paste(idCols, collapse = ", "))
  sa <- data.table::fread(sampleAreasPath, select = c("code", "protocol", "area"), colClasses = "character")
  sa <- sa[sa$protocol == "CBBM" & nzchar(sa$area) & nzchar(sa$code), ]
  sa <- sa[!duplicated(tolower(sa$code)), ]
  g <- sf::st_as_sfc(structure(as.list(sa$area), class = "WKB"), EWKB = TRUE); sf::st_crs(g) <- 4326
  g <- suppressWarnings(sf::st_transform(g, sf::st_crs(pf)))
  sap <- sf::st_sf(code = sa$code, geometry = g); names(sap)[1] <- idCol; sf::st_geometry(sap) <- "geometry"
  kS <- tolower(sap[[idCol]]); kP <- tolower(pf[[idCol]]); both <- intersect(kS, kP)
  worst <- NA_real_
  if (length(both)) {
    cS <- sf::st_coordinates(suppressWarnings(sf::st_centroid(sf::st_geometry(sap)[match(both, kS)])))[, 1:2, drop = FALSE]
    cP <- sf::st_coordinates(suppressWarnings(sf::st_centroid(sf::st_geometry(pf)[match(both, kP)])))[, 1:2, drop = FALSE]
    d <- sqrt(rowSums((cS - cP)^2)); worst <- max(d)
    if (worst > maxCentroidDiffM) stop("readPlotPolygons(): the centroids of ", sum(d > maxCentroidDiffM), " plot(s) differ by up to ", round(worst), " m between ", basename(sampleAreasPath), " and the shapefile (limit ",
                                       maxCentroidDiffM, " m): wrong CRS or axis order? Nothing used.")
  }
  fallback <- pf[!(kP %in% kS), ]
  out <- dplyr::bind_rows(fallback, sap)
  message("readPlotPolygons(): ", nrow(sap), " plots from ", basename(sampleAreasPath), " + ", nrow(fallback), " only in the shapefile (fallback) = ", nrow(out), " plots; ", length(both),
          " plots in both, largest centroid difference ", if (is.na(worst)) "n/a" else paste0(round(worst, 2), " m"))
  out
}
