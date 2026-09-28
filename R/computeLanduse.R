#' Compute the 14-category land use proportion layers for one year
#'
#' Detects which raw dataset (CTM or HCTM) covers `year`, reprojects it,
#' and computes a proportion-of-category layer for each of the 14
#' consistent output categories at every distinct habitat/landscape
#' resolution actually needed (usually just one of each -- the shared
#' default -- but more than one when a species has its own
#' `resolution_m` override; see DECISIONS.md's 2026-09-28 entry). HCTM
#' years get an all-NA hedges layer (not mapped in that dataset).
#'
#' @param year Integer.
#' @param landuseRawDir Character. Directory containing raw crop type maps.
#' @param processedRoot Character. `predictors/processed` directory
#'   (without the scale_X leaf -- each resolution's own leaf, via
#'   `scaleLabel()`, is appended internally).
#' @param habitatResolutions,landscapeResolutions Numeric vectors. Every
#'   distinct resolution (m) actually needed at that scale, across all
#'   species (usually a length-1 vector, the shared default).
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @param force Logical. If TRUE, recompute and overwrite even if a valid
#'   cached output already exists (e.g. a bug was found in the raw data).
#' @return Invisibly, a named list of output file paths, keyed
#'   `"habitat_<resM>"`/`"landscape_<resM>"`, or NULL if no raw file was
#'   found for `year`.
computeLanduse <- function(year, landuseRawDir, processedRoot,
                            habitatResolutions, landscapeResolutions,
                            targetCRS, force = FALSE) {

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

  # One entry per distinct resolution actually needed, at each scale --
  # usually 2 total (one habitat, one landscape default), more when a
  # species has its own resolution_m override.
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

  # Resolution appended to the filename itself (second safety layer beyond
  # the containing scaleLabel()-named folder) -- see aggregateAndSave.R.
  outFiles <- lapply(scales, function(s) {
    file.path(s$dir, paste0("landuse_", year, "_", s$scaleName, "_", basename(s$dir), ".tif"))
  })

  # Independent per-scale-entry cache check (Lisa Hildebrand's v2 pattern,
  # generalized beyond exactly 2 scales) -- an entry whose cached output is
  # already valid is skipped even when another entry needs recomputing.
  need <- vapply(outFiles, function(f) force || !isValidRasterFile(f), logical(1))

  if (!any(need)) {
    message("  Cache hit -- skipping year ", year)
    return(invisible(outFiles))
  }
  for (nm in names(scales)[!need]) message("  ", nm, " cache hit -- skipping")
  for (nm in names(scales)[need]) message("  ", nm, " needs (re)computation")

  message("  Loading and reprojecting crop type map...")
  cropMap <- terra::rast(rawFile)
  # method = "near" critical for categorical data
  cropMap <- terra::project(cropMap, targetCRS, method = "near")

  message("  Computing ", length(categories), " category proportions + hedges layer...")

  allCatNames <- c("grassland", "winter_cereals", "summer_cereals", "maize",
                    "root_crops", "rapeseed", "sunflower", "vegetables",
                    "legumes", "hedges", "fallow", "grapevine", "hops",
                    "orchards_and_berries")

  layersByScale <- stats::setNames(vector("list", length(scales)), names(scales))
  neededNames <- names(scales)[need]

  for (catName in allCatNames) {

    if (catName == "hedges" && !hasHedges) {
      # HCTM years: hedges not mapped -- fill with NA
      message("    hedges -- NA (not mapped in HCTM dataset)")

      for (nm in neededNames) {
        s <- scales[[nm]]
        na <- makeCategoryProportionLayer(cropMap, -9999, s$res, "hedges", targetCRS)
        na[] <- NA
        layersByScale[[nm]][["hedges"]] <- na
      }
      next
    }

    codes <- categories[[catName]]
    if (is.null(codes)) next

    message("    ", catName)
    for (nm in neededNames) {
      s <- scales[[nm]]
      layersByScale[[nm]][[catName]] <- makeCategoryProportionLayer(cropMap, codes, s$res, catName, targetCRS)
    }
  }

  for (nm in neededNames) {
    terra::writeRaster(terra::rast(layersByScale[[nm]]), outFiles[[nm]], overwrite = TRUE)
    message("  Saved ", nm, ": ", outFiles[[nm]])
  }

  invisible(outFiles)
}
