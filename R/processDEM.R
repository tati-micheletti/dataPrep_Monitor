#' Process downloaded GLO-30 DEM tiles to terrain derivatives
#'
#' Builds a virtual mosaic from the downloaded tiles, reprojects to
#' `targetCRS` at native ~30m, computes elevation/slope/solar radiation
#' aspect index (trasp, Roberts & Cooper 1989) at 30m, then aggregates
#' each derivative to every distinct habitat/landscape resolution
#' actually needed (usually just one of each -- the shared default -- but
#' more than one when a species has its own `resolution_m` override; see
#' DECISIONS.md's 2026-09-28 entry).
#'
#' NOTE: solar radiation uses `spatialEco::trasp()` -- a dimensionless
#' index 0-1, not Wh/m^2.
#'
#' @param demRawDir Character. Directory containing downloaded DEM tiles.
#' @param processedDir Character. Directory for intermediate 30m rasters.
#' @param processedRoot Character. `predictors/processed` directory
#'   (without the scale_X leaf -- each resolution's own leaf, via
#'   `scaleLabel()`, is appended internally).
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param habitatResolutions,landscapeResolutions Numeric vectors. Every
#'   distinct resolution (m) actually needed at that scale.
#' @param force Logical. If TRUE, recompute and overwrite every intermediate
#'   and output step even if a valid cached file already exists (e.g. a bug
#'   was found in the raw DEM tiles).
#' @return Invisibly, a named list of output file paths per scale-entry/layer.
processDEM <- function(demRawDir, processedDir, processedRoot, targetCRS,
                        habitatResolutions, landscapeResolutions,
                        force = FALSE) {

  dir.create(processedDir, recursive = TRUE, showWarnings = FALSE)

  dem30mPath <- file.path(processedDir, "dem_30m_laea.tif")
  vrtPath <- file.path(processedDir, "dem_mosaic.vrt")

  # Steps 1-2 only matter when the 30m DEM has to be (re)built. If it is already
  # there (e.g. copied to EVE without the 41 GB of raw tiles), skip both: the
  # mosaic and the raw tiles are never touched.
  if (force || !isValidRasterFile(dem30mPath)) {
    # Step 1: Virtual mosaic
    message("Step 1: Building virtual mosaic from GLO-30 tiles...")
    if (force || !file.exists(vrtPath)) {
      tileFiles <- list.files(demRawDir, pattern = "[.]tif$", full.names = TRUE)
      message("  Found ", length(tileFiles), " tiles.")
      terra::vrt(tileFiles, vrtPath)
      message("  VRT created: ", vrtPath)
    } else {
      message("  Already exists, skipping: ", vrtPath)
    }
    demVrt <- terra::rast(vrtPath)

    # Step 2: Reproject to target CRS at 30m
    message("Step 2: Reprojecting to ", targetCRS, " at 30m...")
    dem30m <- terra::project(demVrt, targetCRS, res = 30, method = "bilinear")
    names(dem30m) <- "elevation"
    terra::writeRaster(dem30m, dem30mPath, overwrite = TRUE)
    message("  Saved: ", dem30mPath)
  } else {
    message("Steps 1-2: 30m DEM already exists, skipping: ", dem30mPath)
  }

  dem30m <- terra::rast(dem30mPath)
  message("  30m DEM dimensions: ", nrow(dem30m), " rows x ", ncol(dem30m), " cols")

  # Step 3: Terrain derivatives at 30m
  message("Step 3: Computing terrain derivatives at 30m...")

  slope30mPath <- file.path(processedDir, "slope_30m_laea.tif")
  if (force || !isValidRasterFile(slope30mPath)) {
    message("  Computing slope...")
    slope30m <- terra::terrain(dem30m, v = "slope", unit = "degrees")
    names(slope30m) <- "slope"
    terra::writeRaster(slope30m, slope30mPath, overwrite = TRUE)
    message("  Saved: ", slope30mPath)
  } else {
    message("  Already exists, skipping: ", slope30mPath)
  }
  slope30m <- terra::rast(slope30mPath)

  # Solar radiation is NOT a predictor (DECISIONS.md, 2026-09-26); it is only
  # kept as the reference grid at HABITAT resolution. So it is produced for
  # habitat scales only, and the 30m solar file (50 GB) is only needed when one
  # of those habitat outputs is missing -- landscape scales never need it.
  habitatSolarFiles <- vapply(habitatResolutions, function(r) {
    leaf <- scaleLabel(r)
    file.path(processedRoot, leaf, paste0("solar_radiation_habitat_", leaf, ".tif"))
  }, character(1))
  needSolar <- force || !all(vapply(habitatSolarFiles, isValidRasterFile, logical(1)))

  solar30m <- NULL
  if (needSolar) {
    solar30mPath <- file.path(processedDir, "solar_rad_30m_laea.tif")
    if (force || !isValidRasterFile(solar30mPath)) {
      message("  Computing solar radiation aspect index (trasp)...")
      message("  (May take 20-40 minutes at 30m)")
      solar30m <- spatialEco::trasp(dem30m)
      names(solar30m) <- "solar_radiation"
      terra::writeRaster(solar30m, solar30mPath, overwrite = TRUE)
      message("  Saved: ", solar30mPath)
    } else {
      message("  Already exists, skipping: ", solar30mPath)
    }
    solar30m <- terra::rast(solar30mPath)
  } else {
    message("  Habitat-scale solar radiation outputs already exist -- 30m solar not needed.")
  }

  # Step 4: Aggregate to every distinct resolution actually needed, at
  # each scale -- usually 2 total (one habitat, one landscape default),
  # more when a species has its own resolution_m override.
  message("Step 4: Aggregating to habitat and landscape scales...")

  scales <- c(
    lapply(habitatResolutions, function(r) list(scaleName = "habitat", res = r)),
    lapply(landscapeResolutions, function(r) list(scaleName = "landscape", res = r))
  )
  scales <- lapply(scales, function(s) {
    s$dir <- file.path(processedRoot, scaleLabel(s$res))
    s
  })
  names(scales) <- vapply(scales, function(s) paste0(s$scaleName, "_", s$res), character(1))
  for (s in scales) dir.create(s$dir, recursive = TRUE, showWarnings = FALSE)

  derivatives <- list(elevation = dem30m, slope = slope30m)
  if (!is.null(solar30m)) derivatives$solar_radiation <- solar30m
  # Layers per scale: solar radiation only at habitat scale (see above).
  layersForScale <- function(scaleName) {
    if (scaleName == "habitat") names(derivatives) else setdiff(names(derivatives), "solar_radiation")
  }

  outFiles <- list()
  for (scaleKey in names(scales)) {
    scaleCfg <- scales[[scaleKey]]
    message("\n  Scale: ", scaleKey, " (", scaleCfg$res, "m)")

    for (layerName in layersForScale(scaleCfg$scaleName)) {
      outFiles[[paste(scaleKey, layerName)]] <- aggregateAndSave(
        rast30m = derivatives[[layerName]],
        scaleName = scaleCfg$scaleName,
        layerName = layerName,
        outputDir = scaleCfg$dir,
        targetResM = scaleCfg$res,
        targetCRS = targetCRS,
        force = force)
    }
  }

  # Step 5: Sanity check summaries
  message("\nStep 5: Output summaries...")
  for (scaleKey in names(scales)) {
    scaleCfg <- scales[[scaleKey]]
    message("\n  --- ", toupper(scaleKey), " (", scaleCfg$res, "m) ---")

    for (layerName in layersForScale(scaleCfg$scaleName)) {
      outFile <- file.path(scaleCfg$dir, paste0(layerName, "_", scaleCfg$scaleName, "_", basename(scaleCfg$dir), ".tif"))
      if (file.exists(outFile)) {
        r <- terra::rast(outFile)
        vals <- terra::values(r, na.rm = TRUE)
        message("  ", layerName,
                ": min=", round(min(vals), 2),
                " max=", round(max(vals), 2),
                " mean=", round(mean(vals), 2))
      }
    }
  }

  invisible(outFiles)
}
