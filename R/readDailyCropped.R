#' Read one daily CHELSA raster for the study area, robustly
#'
#' CHELSA's daily files are read remotely (`/vsicurl/`). A read can fail PARTLY -- GDAL then reports
#' "Cannot read offset/size for strile ..." (April 2022 tasmin, day 6: found 2026-10-07) -- and the
#' result is a raster full of zeros or fill values that looks like data. This function retries a failed
#' read and removes fill values (temperature outside 150-350 K, precipitation outside 0-2000 mm), so a
#' damaged block ends up as NA, which `completeDailyLayers()` then detects.
#'
#' @param url Character. `/vsicurl/` URL of the daily file.
#' @param europeBbox A `terra::ext` object to crop to.
#' @param variable Character. "tasmin", "tasmax" or "prec".
#' @param maxTries Integer. Attempts before giving up.
#' @return A SpatRaster (cropped, fill values = NA) or NULL.
readDailyCropped <- function(url, europeBbox, variable, maxTries = 4L) {
  lo <- if (variable == "prec") 0 else 150
  hi <- if (variable == "prec") 2000 else 350
  for (a in seq_len(maxTries)) {
    r <- tryCatch({
      x <- terra::crop(terra::rast(url), europeBbox)
      terra::clamp(x, lower = lo, upper = hi, values = FALSE)
    }, error = function(e) NULL)
    if (!is.null(r)) return(r)
    Sys.sleep(2 * a)
  }
  NULL
}

#' Keep only the daily layers that were read completely
#'
#' A partly failed read leaves a layer with far fewer valid cells than its neighbours. Layers whose number of valid cells is below
#' `minShare` of the month's median are re-read (`reread()`, up to `maxRereads` times); if they stay incomplete they are dropped (the
#' month then has fewer days, which the caller reports and, below 90% of the days, refuses to use).
#'
#' @param layers List of SpatRasters (NULL entries allowed = file unavailable).
#' @param reread Function(index) returning a fresh SpatRaster or NULL, for the day at `index` of `layers`.
#' @return List of complete SpatRasters (NULL entries removed).
completeDailyLayers <- function(layers, reread, minShare = 0.9, maxRereads = 3L) {
  nValid <- function(x) if (is.null(x)) NA_real_ else terra::global(x, "notNA")[1, 1]
  counts <- vapply(layers, nValid, numeric(1))
  if (all(is.na(counts))) return(list())
  ref <- stats::median(counts, na.rm = TRUE)
  for (i in which(!is.na(counts) & counts < minShare * ref)) {
    for (k in seq_len(maxRereads)) {
      x <- reread(i); n <- nValid(x)
      if (!is.na(n) && n >= minShare * ref) { layers[[i]] <- x; counts[i] <- n; break }
    }
  }
  keep <- !is.na(counts) & counts >= minShare * ref
  if (any(!is.na(counts) & !keep))
    warning(sum(!is.na(counts) & !keep), " daily layer(s) stayed incomplete after re-reading and were dropped")
  layers[keep]
}
