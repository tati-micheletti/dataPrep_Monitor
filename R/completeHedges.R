#' Complete the `hedges` layer of every land-use file IN THE FILES (not on the fly): ONE FIXED layer (`mode = "mean"`, since 2026-11-03) or the nearest preceding year (`mode = "nearest"`, the old rule)
#'
#' `mode = "mean"`: the layer of EVERY land-use file becomes the mean of the hedge layers of the years that have hedge data (2017-2021, 2024-2025 at the time of writing). The year-to-year
#' changes of the Thuenen hedge layer are not real hedge dynamics but differences between map versions (country mean 3.3% in 2017, 3.7-3.8% in 2018-2021, 2.8% in 2024, 3.6% in 2025; the spatial pattern of
#' 2024/2025 correlates only 0.85-0.91 with 2018-2021, 2018-2021 with each other 0.96-0.97), so a yearly layer would put artefacts into the predictions (DECISIONS.md 2026-11-03). Every file gets a sidecar
#' `<file>.hedgesfixed` naming the years the mean was made from: files that carry it are not read as data again, so a second call changes nothing; a file recomputed by `computeLanduse()` has its raw
#' hedges again and the mean is made anew over the years that have raw data (recompute ALL years for a mean over all of them).
#'
#' `mode = "nearest"` (the rule before 2026-11-03):
#' `hedges` (Thuenen hedge data) exists only for some years (2017-2021, 2024-2025 at the time of writing); in the other land-use files the layer is entirely NA. The covariate loaders used to
#' fill it on the fly, and the occurrence step exempted `hedges` from its "no NA" rule. A predictor layer must be complete by construction (DECISIONS.md 2026-10-21), so the gap is filled
#' once, here, in the processed land-use files. `mode = "nearest"` (the OLD rule, no longer the default): a year without data takes the hedge layer of the nearest PRECEDING year with data (no future information for past years); years before the first
#' year with data take the first one. Which years have data is read from the files (a layer with at least one non-NA cell), nothing is hard-coded. ALL land-use files of the scale are completed, also those of years that are not requested in this run (they would otherwise fail the completeness check). A file that already has hedge data is not touched.
#' The filled layer is resampled (bilinear) onto the grid of the file, as the loaders did. The sidecar `.studyarea` of a changed file is removed so that it is cropped and masked again.
#'
#' `mode = "mean"` also runs `fillHedgeGaps()` on every land-use file afterwards (2026-11-05): the files of years on a larger grid (2024-2025, CTM) have a few rim cells that lie beyond the grid the mean was made on (see there).
#'
#' @param processedRoot Character. `predictors/processed`.
#' @param landuseYears Integer vector of years.
#' @param habitatResolutions,landscapeResolutions Numeric vectors (m).
#' @param dryRun Logical. Only report.
#' @return Invisibly, a data.frame (scale, year, action, from).
completeHedges <- function(processedRoot, landuseYears, habitatResolutions, landscapeResolutions, dryRun = FALSE, mode = c("mean", "nearest"), repairGaps = TRUE, allowPartialMean = FALSE) {
  mode <- match.arg(mode)
  scales <- c(lapply(habitatResolutions, function(r) list(scaleName = "habitat", res = r)), lapply(landscapeResolutions, function(r) list(scaleName = "landscape", res = r)))
  rows <- list()
  for (s in scales) {
    d <- file.path(processedRoot, scaleLabel(s$res))
    f <- function(y) file.path(d, paste0("landuse_", y, "_", s$scaleName, "_", basename(d), ".tif"))
    onDisk <- as.integer(sub(paste0("^landuse_([0-9]{4})_", s$scaleName, "_", basename(d), "[.]tif$"), "\\1",
                             list.files(d, pattern = paste0("^landuse_[0-9]{4}_", s$scaleName, "_", basename(d), "[.]tif$"))))       # EVERY land-use file of the scale, not only the requested years: a file left from an earlier run
    years <- sort(unique(c(landuseYears, onDisk))); years <- years[file.exists(vapply(years, f, character(1)))]      # must be complete too (the check after the preparation looks at all files)
    hasData <- vapply(years, function(y) { r <- terra::rast(f(y)); "hedges" %in% names(r) && isTRUE(terra::global(r[["hedges"]], function(x) any(!is.na(x)))[1, 1] > 0) }, logical(1))
    if (mode == "mean") {
      fixedOf <- function(y) { sf <- paste0(f(y), ".hedgesfixed"); if (file.exists(sf)) readLines(sf, warn = FALSE)[1] else "" }
      isFixed <- vapply(years, function(y) nzchar(fixedOf(y)), logical(1)); realM <- years[hasData & !isFixed]
      if (!length(realM)) {
        if (all(isFixed)) { message("hedges ", basename(d), ": all files already carry the fixed layer, nothing to do"); next }
        stop("completeHedges(): no year with raw hedge data at ", basename(d), " and not every file carries the fixed layer; recompute the land use (rerunLanduse).")
      }
      # 2026-11-06 guard (audit S2-28): the fixed layer was made from several years; if only some of them have raw hedge data now (a partial recompute of the land use), a new mean would be made from those years only and written into ALL files.
      oldYears <- suppressWarnings(as.integer(unlist(strsplit(sub("^.*hedge layers of ", "", unique(vapply(years[isFixed], fixedOf, character(1)))), ","))))
      oldYears <- unique(oldYears[!is.na(oldYears)])
      if (!allowPartialMean && length(oldYears) && !all(oldYears %in% realM)) stop("completeHedges(): the fixed hedge layer was made from the years ", paste(oldYears, collapse = ","), " but only ", paste(realM, collapse = ","),
           " have raw hedge data now: a partial recompute would give a mean of those years only. Recompute the land use of ALL these years (delete their landuse_<year>_* files and sidecars) or call with allowPartialMean = TRUE.")
      tag <- paste0("fixed mean of the hedge layers of ", paste(realM, collapse = ","))
      todo <- years[vapply(years, fixedOf, character(1)) != tag]
      if (dryRun) { for (y in todo) rows[[length(rows) + 1]] <- data.frame(scale = basename(d), year = y, action = "would fix", from = length(realM)); next }
      if (!length(todo)) next
      lay <- lapply(realM, function(y) terra::rast(f(y))[["hedges"]]); g <- lay[[1]]
      lay <- lapply(lay, function(l) if (isTRUE(terra::compareGeom(l, g, stopOnError = FALSE))) l else terra::resample(l, g, method = "bilinear"))
      meanH <- terra::mean(terra::rast(lay), na.rm = TRUE); names(meanH) <- "hedges"; rm(lay); invisible(gc())
      for (y in todo) {
        r <- terra::rast(f(y)); mh <- terra::resample(meanH, r[[1]], method = "bilinear"); names(mh) <- "hedges"
        out <- terra::rast(lapply(names(r), function(n) if (n == "hedges") mh else r[[n]])); names(out) <- names(r)
        part <- paste0(f(y), ".part")
        terra::writeRaster(out, part, filetype = "GTiff", overwrite = TRUE, datatype = terra::datatype(r)[1], gdal = "COMPRESS=DEFLATE")
        rm(r, out, mh); invisible(gc())
        file.remove(f(y)); file.rename(part, f(y)); sf <- paste0(f(y), ".studyarea"); if (file.exists(sf)) file.remove(sf)
        writeLines(tag, paste0(f(y), ".hedgesfixed"))
        message("hedges ", basename(d), " ", y, ": fixed layer (mean of ", paste(realM, collapse = ","), ")")
        rows[[length(rows) + 1]] <- data.frame(scale = basename(d), year = y, action = "fixed mean", from = length(realM))
      }
      next
    }
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
  # 2026-11-05: the rim of the larger grids (see fillHedgeGaps). 2026-11-06: in the pipeline this runs AFTER the study-area crop (repairGaps = FALSE here, repairHedgeGaps() in the applyStudyArea event): on uncropped files it would see the whole 15 km margin as gaps.
  if (mode == "mean" && repairGaps) { g <- repairHedgeGaps(processedRoot, habitatResolutions, landscapeResolutions, dryRun); if (!is.null(g) && nrow(g)) rows[[length(rows) + 1]] <- g }
  invisible(if (length(rows)) do.call(rbind, rows) else data.frame(scale = character(), year = integer(), action = character(), from = integer()))
}

#' Repair the rim gaps (`fillHedgeGaps()`) of every land-use file of every scale; runs after the study-area crop (2026-11-06)
#' @return data.frame (scale, year, action, from) of the files repaired, or NULL.
repairHedgeGaps <- function(processedRoot, habitatResolutions, landscapeResolutions, dryRun = FALSE) {
  scales <- c(lapply(habitatResolutions, function(r) list(scaleName = "habitat", res = r)), lapply(landscapeResolutions, function(r) list(scaleName = "landscape", res = r))); rows <- list()
  for (s in scales) {
    d <- file.path(processedRoot, scaleLabel(s$res))
    for (f in list.files(d, pattern = paste0("^landuse_[0-9]{4}_", s$scaleName, "_", basename(d), "[.]tif$"), full.names = TRUE)) { g <- fillHedgeGaps(f, dryRun = dryRun); if (!is.null(g)) rows[[length(rows) + 1]] <- g }
  }
  if (length(rows)) do.call(rbind, rows) else NULL
}

#' Fill the empty cells of the `hedges` layer of ONE land-use file from the nearest valid cell (2026-11-05)
#'
#' Why: the land-use files of different years do not share one grid. The classes of 1990-2023 come from the HCTM map, whose footprint is about Germany's bounding box (no buffer); 2024-2025 come from CTM, whose grid is large,
#' so these files are cropped to the full study area + buffer and are a few cells larger on every side. The fixed hedge mean is made on the grid of the first hedge year (HCTM footprint) and resampled onto each file, so the
#' few cells of the larger grids that lie beyond that footprint get no value (v3 run, 5 Nov 2026: 2 to 5 cells per file at the four scales, all in the 2024 and 2025 files, found by `checkNoNAInsideStudyArea()`).
#' The raw CTM hedge maps themselves cover the whole area. A cell is filled only if it is empty in `hedges` AND the first other layer of the file has a value there (so the masked outside of the study area stays empty);
#' it takes the value of the nearest valid cell within `maxDistM` metres PLUS 1.5 cell sizes (the mean of the cells at equal distance; the larger grids overhang by up to about 1.1 km and a gap cell can lie a cell beyond that: in the v3 prep a 1 km cell north of Germany had its nearest valid neighbour 2,000 m away, and the neighbouring cell of a 5 km map is 5,000 m away). More than `maxCells` empty cells, or a cell with no valid neighbour within `maxDistM`, is NOT a rim effect and stops.
#' A sidecar `<file>.hedgegaps` records the check (it is skipped while the file is not newer than the sidecar), so a second call changes nothing. The sidecar `.studyarea` is kept: the filled cells lie inside the mask.
#'
#' @param file Character. A processed land-use file. @param dryRun Logical. Only report. @param maxDistM Numeric. How far (m) to look for a valid neighbour, before the 1.5 cell sizes that are added. @param maxCells Integer. Safety limit.
#' @return NULL if nothing had to be filled, else a one-row data.frame (scale, year, action, from = number of cells).
fillHedgeGaps <- function(file, dryRun = FALSE, maxDistM = 1500, maxCells = 5000L) {
  sc <- paste0(file, ".hedgegaps")
  if (!dryRun && file.exists(sc) && file.mtime(sc) >= file.mtime(file)) return(NULL)
  r <- terra::rast(file); nm <- names(r); ref <- setdiff(nm, "hedges")
  if (!"hedges" %in% nm || !length(ref)) return(NULL)
  h <- r[["hedges"]]
  cl <- which(terra::values(is.na(h) & !is.na(r[[ref[1]]]), mat = FALSE) == 1)
  scl <- basename(dirname(file)); yr <- suppressWarnings(as.integer(sub("^landuse_([0-9]{4})_.*$", "\\1", basename(file))))
  if (!length(cl)) { if (!dryRun) writeLines("0 gap cells", sc); return(NULL) }
  if (length(cl) > maxCells) stop("fillHedgeGaps(): ", length(cl), " empty hedge cells in ", scl, "/", basename(file), " (limit ", maxCells, "): this is not a rim effect; recompute the land use (rerunLanduse).")
  if (dryRun) return(data.frame(scale = scl, year = yr, action = "would fill gaps", from = length(cl)))
  nr <- terra::nrow(h); nc <- terra::ncol(h); rc <- terra::rowColFromCell(h, cl); xy <- terra::xyFromCell(h, cl)
  resM <- terra::res(h)[1]; capM <- maxDistM + 1.5 * resM; window <- as.integer(ceiling(capM / resM))             # the larger grids overhang by up to about 1.1 km, and a gap cell can lie one cell beyond that, so the search distance is maxDistM PLUS 1.5 cells: 200 m cells 1,800 m; 700 m 2,550 m; 1 km 3,000 m; 5 km 9,000 m
  val <- rep(NA_real_, length(cl)); dst <- rep(NA_real_, length(cl))
  for (k in seq_along(cl)) {
    r0 <- max(1L, rc[k, 1] - window); r1 <- min(nr, rc[k, 1] + window); c0 <- max(1L, rc[k, 2] - window); c1 <- min(nc, rc[k, 2] + window); ncw <- c1 - c0 + 1L
    w <- as.vector(terra::values(h, mat = FALSE, row = r0, nrows = r1 - r0 + 1L, col = c0, ncols = ncw)); ok <- which(!is.na(w))
    d <- sqrt(((ok - 1L) %/% ncw + r0 - rc[k, 1])^2 + ((ok - 1L) %% ncw + c0 - rc[k, 2])^2) * resM       # distance in metres to every valid cell of the window
    keep <- d <= capM + 1e-9; ok <- ok[keep]; d <- d[keep]
    if (!length(ok)) stop("fillHedgeGaps(): the empty hedge cell at x = ", round(xy[k, 1]), ", y = ", round(xy[k, 2]), " (", scl, "/", basename(file), ") has no valid cell within ", capM, " m: not a rim effect.")
    val[k] <- mean(w[ok][d <= min(d) + 1e-9]); dst[k] <- min(d)
  }
  v <- terra::values(h, mat = FALSE); v[cl] <- val; hn <- terra::rast(h); terra::values(hn) <- v; names(hn) <- "hedges"; rm(v)            # a fresh in-memory layer: `h[cl] <- val` kept a link to the file, which Windows then refuses to replace
  out <- terra::rast(lapply(nm, function(n) if (n == "hedges") hn else r[[n]])); names(out) <- nm
  part <- paste0(file, ".part")
  terra::writeRaster(out, part, filetype = "GTiff", overwrite = TRUE, datatype = terra::datatype(r)[1], gdal = "COMPRESS=DEFLATE")
  rm(r, h, hn, out); invisible(gc())
  for (i in 1:6) { if (suppressWarnings(file.remove(file))) break; invisible(gc()); Sys.sleep(0.5) }                      # Windows keeps the old file open until terra lets go; on EVE the first try works
  if (file.exists(file) || !file.rename(part, file)) stop("fillHedgeGaps(): could not replace ", file, " by the repaired copy ", part, "; the original is untouched or the repaired copy is left beside it.")
  sfStudy <- paste0(file, ".studyarea"); if (file.exists(sfStudy)) Sys.setFileTime(sfStudy, Sys.time())      # the crop is still valid (only cells inside the mask were filled): refresh the marker, or applyStudyAreaToProcessed() would see a file newer than its marker and crop it again
  writeLines(paste0(length(cl), " gap cell(s) filled, farthest source ", format(max(dst), digits = 3), " m away; ", Sys.Date()), sc)
  first <- seq_len(min(6L, length(cl)))
  message("hedges gaps ", scl, "/", basename(file), ": ", length(cl), " empty cell(s) filled from the nearest valid cell (farthest source ", format(max(dst), digits = 3), " m away); first at (x, y) = ",
          paste(sprintf("(%.0f, %.0f)", xy[first, 1], xy[first, 2]), collapse = " "))
  data.frame(scale = scl, year = yr, action = "gap filled", from = length(cl))
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
  terra::terraOptions(memmax = 8)                                                       # terra sizes its memory from the whole node, not from the Slurm limit: without a cap the masks of hundreds of files piled up and the job was killed (OOM, 29 Oct 2026)
  for (d in dirs) for (f in list.files(d, pattern = "[.]tif$", full.names = TRUE)) {
    if (basename(d) == climateScaleLabel && grepl(paste0("bioclim_[0-9]{4}-", ebba2TrainingYear, "_"), basename(f))) next
    r <- terra::rast(f); na <- terra::mask(is.na(r), area, touches = TRUE)             # 1 = NA inside the area, 0 = value inside, NA outside
    cnt <- terra::global(na, "sum", na.rm = TRUE)[, 1]; n <- n + 1L
    if (any(cnt > 0)) bad[[length(bad) + 1]] <- data.frame(file = file.path(basename(d), basename(f)), layer = names(r)[cnt > 0], naInsideStudyArea = cnt[cnt > 0])
    rm(r, na); invisible(gc(verbose = FALSE))                                           # free the memory of this file before the next one
  }
  out <- if (length(bad)) do.call(rbind, bad) else data.frame(file = character(), layer = character(), naInsideStudyArea = numeric())
  message("checkNoNAInsideStudyArea: ", n, " layer files checked, ", nrow(out), " layer(s) with NA inside the study area '", area$label[1], "'")
  if (nrow(out) && stopIfNA) stop("Predictor layers must be complete inside the study area, but these have NA (fix them where the layer is made):\n",
                                   paste(utils::capture.output(print(utils::head(out, 25), row.names = FALSE)), collapse = "\n"), call. = FALSE)
  invisible(out)
}
