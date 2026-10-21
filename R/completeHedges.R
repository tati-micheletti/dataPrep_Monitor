#' Fill the `hedges` layer of the years without hedge data IN THE FILES (not on the fly)
#'
#' `hedges` (Thuenen hedge data) exists only for some years (2017-2021, 2024-2025 at the time of writing); in the other land-use files the layer is entirely NA. The covariate loaders used to
#' fill it on the fly, and the occurrence step exempted `hedges` from its "no NA" rule. A predictor layer must be complete by construction (DECISIONS.md 2026-10-21), so the gap is filled
#' once, here, in the processed land-use files: a year without data takes the hedge layer of the nearest PRECEDING year with data (no future information for past years); years before the first
#' year with data take the first one. Which years have data is read from the files (a layer with at least one non-NA cell), nothing is hard-coded. ALL land-use files of the scale are completed, also those of years that are not requested in this run (they would otherwise fail the completeness check). A file that already has hedge data is not touched.
#' The filled layer is resampled (bilinear) onto the grid of the file, as the loaders did. The sidecar `.studyarea` of a changed file is removed so that it is cropped and masked again.
#'
#' @param processedRoot Character. `predictors/processed`.
#' @param landuseYears Integer vector of years.
#' @param habitatResolutions,landscapeResolutions Numeric vectors (m).
#' @param dryRun Logical. Only report.
#' @return Invisibly, a data.frame (scale, year, action, from).
completeHedges <- function(processedRoot, landuseYears, habitatResolutions, landscapeResolutions, dryRun = FALSE) {
  scales <- c(lapply(habitatResolutions, function(r) list(scaleName = "habitat", res = r)), lapply(landscapeResolutions, function(r) list(scaleName = "landscape", res = r)))
  rows <- list()
  for (s in scales) {
    d <- file.path(processedRoot, scaleLabel(s$res))
    f <- function(y) file.path(d, paste0("landuse_", y, "_", s$scaleName, "_", basename(d), ".tif"))
    onDisk <- as.integer(sub(paste0("^landuse_([0-9]{4})_", s$scaleName, "_", basename(d), "[.]tif$"), "\\1",
                             list.files(d, pattern = paste0("^landuse_[0-9]{4}_", s$scaleName, "_", basename(d), "[.]tif$"))))       # EVERY land-use file of the scale, not only the requested years: a file left from an earlier run
    years <- sort(unique(c(landuseYears, onDisk))); years <- years[file.exists(vapply(years, f, character(1)))]      # must be complete too (the check after the preparation looks at all files)
    hasData <- vapply(years, function(y) { r <- terra::rast(f(y)); "hedges" %in% names(r) && isTRUE(terra::global(r[["hedges"]], function(x) any(!is.na(x)))[1, 1] > 0) }, logical(1))
    real <- years[hasData]
    if (!length(real)) stop("completeHedges(): no year has hedge data at ", basename(d), "; nothing to fill from.")
    for (y in years[!hasData]) {
      ref <- if (any(real < y)) max(real[real < y]) else min(real)
      if (dryRun) { rows[[length(rows) + 1]] <- data.frame(scale = basename(d), year = y, action = "would fill", from = ref); next }
      r <- terra::rast(f(y)); refH <- terra::rast(f(ref))[["hedges"]]
      refH <- terra::resample(refH, r[[1]], method = "bilinear"); names(refH) <- "hedges"
      lyr <- setdiff(names(r), "hedges"); out <- terra::rast(lapply(names(r), function(n) if (n == "hedges") refH else r[[n]])); names(out) <- names(r)
      part <- paste0(f(y), ".part")
      terra::writeRaster(out, part, filetype = "GTiff", overwrite = TRUE, datatype = terra::datatype(r)[1], gdal = "COMPRESS=DEFLATE")
      rm(r, out, refH); invisible(gc())
      file.remove(f(y)); file.rename(part, f(y)); sf <- paste0(f(y), ".studyarea"); if (file.exists(sf)) file.remove(sf)
      message("hedges ", basename(d), " ", y, ": filled from ", ref)
      rows[[length(rows) + 1]] <- data.frame(scale = basename(d), year = y, action = "filled", from = ref)
    }
  }
  invisible(if (length(rows)) do.call(rbind, rows) else data.frame(scale = character(), year = integer(), action = character(), from = integer()))
}

#' Stop if any processed predictor layer has NA inside the study area
#'
#' Predictors must be complete inside the study area by construction (DECISIONS.md 2026-10-21): a missing value is fixed where the layer is made (e.g. `landscape_heterogeneity` gives 0 where no class
#' is mapped), never by dropping records later. This walks `processedRoot/scale_*/*.tif` (the climate TRAINING window excepted: it covers Europe) and counts the NA cells inside the strict study area
#' (no buffer) for every layer; with `stopIfNA` it stops and lists them.
#'
#' @param processedRoot Character. `predictors/processed`. @param area SpatVector from `buildStudyArea()`.
#' @param climateScaleLabel,ebba2TrainingYear Identify the climate training window. @param stopIfNA Logical.
#' @return Invisibly, a data.frame (file, layer, naInsideStudyArea) of the layers with NA.
checkNoNAInsideStudyArea <- function(processedRoot, area, climateScaleLabel = "scale_50", ebba2TrainingYear = 2017, stopIfNA = TRUE) {
  dirs <- list.dirs(processedRoot, recursive = FALSE, full.names = TRUE); dirs <- dirs[grepl("^scale_", basename(dirs))]
  bad <- list(); n <- 0L
  for (d in dirs) for (f in list.files(d, pattern = "[.]tif$", full.names = TRUE)) {
    if (basename(d) == climateScaleLabel && grepl(paste0("bioclim_[0-9]{4}-", ebba2TrainingYear, "_"), basename(f))) next
    r <- terra::rast(f); na <- terra::mask(is.na(r), area, touches = TRUE)             # 1 = NA inside the area, 0 = value inside, NA outside
    cnt <- terra::global(na, "sum", na.rm = TRUE)[, 1]; n <- n + 1L
    if (any(cnt > 0)) bad[[length(bad) + 1]] <- data.frame(file = file.path(basename(d), basename(f)), layer = names(r)[cnt > 0], naInsideStudyArea = cnt[cnt > 0])
  }
  out <- if (length(bad)) do.call(rbind, bad) else data.frame(file = character(), layer = character(), naInsideStudyArea = numeric())
  message("checkNoNAInsideStudyArea: ", n, " layer files checked, ", nrow(out), " layer(s) with NA inside the study area '", area$label[1], "'")
  if (nrow(out) && stopIfNA) stop("Predictor layers must be complete inside the study area, but these have NA (fix them where the layer is made):\n",
                                   paste(utils::capture.output(print(utils::head(out, 25), row.names = FALSE)), collapse = "\n"), call. = FALSE)
  invisible(out)
}
