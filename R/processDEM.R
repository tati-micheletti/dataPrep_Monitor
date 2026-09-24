#' Process downloaded GLO-30 DEM tiles to terrain derivatives
#'
#' Builds a virtual mosaic from the downloaded tiles, reprojects to
#' `targetCRS` at native ~30m, computes elevation/slope/solar radiation
#' aspect index (trasp, Roberts & Cooper 1989) at 30m, then aggregates
#' each derivative to habitat and landscape scales.
#'
#' NOTE: solar radiation uses `spatialEco::trasp()` -- a dimensionless
#' index 0-1, not Wh/m^2.
#'
#' @param demRawDir Character. Directory containing downloaded DEM tiles.
#' @param processedDir Character. Directory for intermediate 30m rasters.
#' @param habitatOutputDir Character. Directory for habitat-scale outputs.
#' @param landscapeOutputDir Character. Directory for landscape-scale outputs.
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param habitatResolutionM Numeric. Habitat scale resolution in metres.
#' @param landscapeResolutionM Numeric. Landscape scale resolution in metres.
#' @param force Logical. If TRUE, recompute and overwrite every intermediate
#'   and output step even if a valid cached file already exists (e.g. a bug
#'   was found in the raw DEM tiles).
#' @return Invisibly, a named list of output file paths per scale/layer.
processDEM <- function(demRawDir, processedDir, habitatOutputDir,
                        landscapeOutputDir, targetCRS,
                        habitatResolutionM, landscapeResolutionM,
                        force = FALSE) {

  dir.create(processedDir, recursive = TRUE, showWarnings = FALSE)
  dir.create(habitatOutputDir, recursive = TRUE, showWarnings = FALSE)
  dir.create(landscapeOutputDir, recursive = TRUE, showWarnings = FALSE)

  # Step 1: Virtual mosaic
  message("Step 1: Building virtual mosaic from GLO-30 tiles...")
  vrtPath <- file.path(processedDir, "dem_mosaic.vrt")

  if (force || !file.exists(vrtPath)) {
    tileFiles <- list.files(demRawDir, pattern = "\\.tif$", full.names = TRUE)
    message("  Found ", length(tileFiles), " tiles.")
    terra::vrt(tileFiles, vrtPath)
    message("  VRT created: ", vrtPath)
  } else {
    message("  Already exists, skipping: ", vrtPath)
  }

  demVrt <- terra::rast(vrtPath)

  # Step 2: Reproject to target CRS at 30m
  message("Step 2: Reprojecting to ", targetCRS, " at 30m...")
  dem30mPath <- file.path(processedDir, "dem_30m_laea.tif")

  if (force || !isValidRasterFile(dem30mPath)) {
    dem30m <- terra::project(demVrt, targetCRS, res = 30, method = "bilinear")
    names(dem30m) <- "elevation"
    terra::writeRaster(dem30m, dem30mPath, overwrite = TRUE)
    message("  Saved: ", dem30mPath)
  } else {
    message("  Already exists, skipping: ", dem30mPath)
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

  # Step 4: Aggregate to habitat and landscape scales
  message("Step 4: Aggregating to habitat and landscape scales...")

  scales <- list(habitat = list(dir = habitatOutputDir, res = habitatResolutionM),
                 landscape = list(dir = landscapeOutputDir, res = landscapeResolutionM))

  derivatives <- list(elevation = dem30m, slope = slope30m, solar_radiation = solar30m)

  outFiles <- list()
  for (scaleName in names(scales)) {
    scaleCfg <- scales[[scaleName]]
    message("\n  Scale: ", scaleName, " (", scaleCfg$res, "m)")

    for (layerName in names(derivatives)) {
      outFiles[[paste(scaleName, layerName)]] <- aggregateAndSave(
        rast30m = derivatives[[layerName]],
        scaleName = scaleName,
        layerName = layerName,
        outputDir = scaleCfg$dir,
        targetResM = scaleCfg$res,
        targetCRS = targetCRS,
        force = force)
    }
  }

  # Step 5: Sanity check summaries
  message("\nStep 5: Output summaries...")
  for (scaleName in names(scales)) {
    scaleCfg <- scales[[scaleName]]
    message("\n  --- ", toupper(scaleName), " scale (", scaleCfg$res, "m) ---")

    for (layerName in names(derivatives)) {
      outFile <- file.path(scaleCfg$dir, paste0(layerName, "_", scaleName, ".tif"))
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
