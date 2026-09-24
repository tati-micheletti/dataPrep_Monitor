#' Compute the 14-category land use proportion layers for one year
#'
#' Detects which raw dataset (CTM or HCTM) covers `year`, reprojects it,
#' and computes a proportion-of-category layer for each of the 14
#' consistent output categories at both habitat and landscape scales.
#' HCTM years get an all-NA hedges layer (not mapped in that dataset).
#'
#' @param year Integer.
#' @param landuseRawDir Character. Directory containing raw crop type maps.
#' @param habitatOutputDir Character. Directory for habitat-scale outputs.
#' @param landscapeOutputDir Character. Directory for landscape-scale outputs.
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param habitatResolutionM Numeric. Habitat scale resolution in metres.
#' @param landscapeResolutionM Numeric. Landscape scale resolution in metres.
#' @param force Logical. If TRUE, recompute and overwrite even if a valid
#'   cached output already exists (e.g. a bug was found in the raw data).
#' @return Invisibly, a list with `habitat` and `landscape` output paths,
#'   or NULL if no raw file was found for `year`.
computeLanduse <- function(year, landuseRawDir, habitatOutputDir,
                            landscapeOutputDir, targetCRS,
                            habitatResolutionM, landscapeResolutionM,
                            force = FALSE) {

  # Find raw file for this year -- check CTM datasets first (Schwieder/Tetteh v302)
  ctmPatterns <- c(file.path(landuseRawDir, sprintf("CTM_GER_%d_rst_v202_COG.tif", year)),
                    file.path(landuseRawDir, sprintf("CTM_GER_%d_rst_v302_COG.tif", year)),
                    # 2025 has a month suffix in the filename
                    file.path(landuseRawDir, sprintf("CTM_GER_%d_rst_v302_2025_08_COG.tif", year)))
  hctmPath <- file.path(landuseRawDir, sprintf("HCTM_GER_%d_rst_v101_COG.tif", year))

  rawFile <- NA
  for (p in ctmPatterns) {
    if (file.exists(p)) { rawFile <- p; break }
  }
  if (is.na(rawFile) && file.exists(hctmPath)) {
    rawFile <- hctmPath
  }

  if (is.na(rawFile)) {
    warning("No raw land use file found for year ", year, " -- skipping.")
    return(invisible(NULL))
  }

  message("  Source file: ", basename(rawFile))

  scheme <- getLanduseCategories(basename(rawFile))
  categories <- scheme$cats
  hasHedges <- scheme$has_hedges

  outHabitat <- file.path(habitatOutputDir, paste0("landuse_", year, "_habitat.tif"))
  outLandscape <- file.path(landscapeOutputDir, paste0("landuse_", year, "_landscape.tif"))

  if (!force && isValidRasterFile(outHabitat) && isValidRasterFile(outLandscape)) {
    message("  Cache hit -- skipping year ", year)
    return(invisible(list(habitat = outHabitat, landscape = outLandscape)))
  }

  message("  Loading and reprojecting crop type map...")
  cropMap <- terra::rast(rawFile)
  # method = "near" critical for categorical data
  cropMap <- terra::project(cropMap, targetCRS, method = "near")

  message("  Computing ", length(categories), " category proportions + hedges layer...")

  allCatNames <- c("grassland", "winter_cereals", "summer_cereals", "maize",
                    "root_crops", "rapeseed", "sunflower", "vegetables",
                    "legumes", "hedges", "fallow", "grapevine", "hops",
                    "orchards_and_berries")

  habitatLayers <- list()
  landscapeLayers <- list()

  for (catName in allCatNames) {

    if (catName == "hedges" && !hasHedges) {
      # HCTM years: hedges not mapped -- fill with NA
      message("    hedges -- NA (not mapped in HCTM dataset)")

      naHab <- makeCategoryProportionLayer(cropMap, -9999, habitatResolutionM, "hedges", targetCRS)
      naLand <- makeCategoryProportionLayer(cropMap, -9999, landscapeResolutionM, "hedges", targetCRS)
      naHab[] <- NA
      naLand[] <- NA

      habitatLayers[["hedges"]] <- naHab
      landscapeLayers[["hedges"]] <- naLand
      next
    }

    codes <- categories[[catName]]
    if (is.null(codes)) next

    message("    ", catName)
    habitatLayers[[catName]] <- makeCategoryProportionLayer(cropMap, codes, habitatResolutionM, catName, targetCRS)
    landscapeLayers[[catName]] <- makeCategoryProportionLayer(cropMap, codes, landscapeResolutionM, catName, targetCRS)
  }

  habitatStack <- terra::rast(habitatLayers)
  landscapeStack <- terra::rast(landscapeLayers)

  terra::writeRaster(habitatStack, outHabitat, overwrite = TRUE)
  terra::writeRaster(landscapeStack, outLandscape, overwrite = TRUE)
  message("  Saved habitat:   ", outHabitat)
  message("  Saved landscape: ", outLandscape)

  invisible(list(habitat = outHabitat, landscape = outLandscape))
}
