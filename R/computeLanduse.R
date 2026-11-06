#' Compute the 14-category land use proportion layers for one year
#'
#' Picks the raw crop type map(s) for `year`, reprojects them, and computes a
#' proportion-of-category layer for each of the 14 consistent output
#' categories at every distinct habitat/landscape resolution actually needed
#' (usually just one of each -- the shared default -- but more than one when
#' a species has its own `resolution_m` override; see DECISIONS.md's
#' 2026-09-28 entry).
#'
#' Which map is used (`preferHCTM = TRUE`, the rule since 2026-11-03):
#' the classes come from HCTM wherever it exists (1990-2023), so the time
#' series is not a patchwork of two products that map the area differently
#' (HCTM maps about 6 percentage points more grassland than CTM, 6 times more
#' orchards and 1.8 times more fallow: the grassland share jumped in 2017, 2022
#' and 2024). Where only CTM exists (2024-2025) its class shares are turned into
#' the shares HCTM would have mapped, with `recodeFile` (see
#' `tools/buildLanduseRecode.R`, `applyLanduseRecode()`). `hedges` exist only in
#' CTM and come from the CTM map of the same year whenever one exists (2017-2021,
#' 2024-2025), otherwise the layer is all NA (filled later by `completeHedges()`).
#' `preferHCTM = FALSE` is the old rule (CTM first, no recode), kept to reproduce earlier runs.
#'
#' A cached output is reused only if it was made with the same rule: next to each
#' output file a small `<file>.scheme.txt` records the source files and the recode
#' used; files without it count as made by the old rule.
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
#' @param preferHCTM Logical. TRUE (default): HCTM first, CTM only where HCTM does not exist, recoded to HCTM classes. FALSE: the old rule.
#' @param recodeFile Character. CTM -> HCTM recode matrix (`data/landuseRecodeCTMtoHCTM.csv`); only read for a year that needs it.
#' @return Invisibly, a named list of output file paths, keyed
#'   `"habitat_<resM>"`/`"landscape_<resM>"`, or NULL if no raw file was
#'   found for `year`.
computeLanduse <- function(year, landuseRawDir, processedRoot,
                            habitatResolutions, landscapeResolutions,
                            targetCRS, force = FALSE,
                            preferHCTM = TRUE, recodeFile = file.path("data", "landuseRecodeCTMtoHCTM.csv")) {

  # Raw files for this year: the CTM maps (Schwieder v202, Tetteh v302) and the HCTM map (Tetteh 2026)
  ctmPatterns <- c(file.path(landuseRawDir, sprintf("CTM_GER_%d_rst_v202_COG.tif", year)),
                    file.path(landuseRawDir, sprintf("CTM_GER_%d_rst_v302_COG.tif", year)),
                    # 2025 has a month suffix in the filename
                    file.path(landuseRawDir, sprintf("CTM_GER_%d_rst_v302_2025_08_COG.tif", year)))
  hctmPath <- file.path(landuseRawDir, sprintf("HCTM_GER_%d_rst_v101_COG.tif", year))

  ctmFile <- NA_character_
  for (p in ctmPatterns) {
    if (file.exists(p)) { ctmFile <- p; break }
  }
  hctmFile <- if (file.exists(hctmPath)) hctmPath else NA_character_

  # classFile = the map the classes come from; hedgeFile = the map the hedges come from (only CTM has them)
  classFile <- if (preferHCTM) { if (!is.na(hctmFile)) hctmFile else ctmFile } else { if (!is.na(ctmFile)) ctmFile else hctmFile }
  hedgeFile <- ctmFile
  rawFile <- classFile
  recode <- preferHCTM && !is.na(classFile) && startsWith(basename(classFile), "CTM_GER")   # CTM classes used as HCTM classes: recode

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

  # What this output is made from: the cache below is valid only for the same rule, same source files, same recode
  schemeTag <- if (!preferHCTM) "legacy-ctm-first" else
    paste0("hctm-first|classes=", if (is.na(classFile)) "NA" else basename(classFile), "|hedges=", if (is.na(hedgeFile)) "NA" else basename(hedgeFile),
           if (recode && !is.null(recodeFile) && file.exists(recodeFile)) paste0("|recode=", unname(tools::md5sum(recodeFile))) else "")
  schemeMatches <- function(f) {
    sc <- paste0(f, ".scheme.txt")
    if (!file.exists(sc)) return(startsWith(schemeTag, "legacy"))                            # files without a record were made by the old rule
    identical(readLines(sc, warn = FALSE)[1], schemeTag)
  }

  # Independent per-scale-entry cache check (Lisa Hildebrand's v2 pattern,
  # generalized beyond exactly 2 scales) -- an entry whose cached output is
  # already valid is skipped even when another entry needs recomputing.
  need <- vapply(outFiles, function(f) force || !isValidRasterFile(f) || !schemeMatches(f), logical(1))

  if (!any(need)) {
    message("  Cache hit -- skipping year ", year)
    return(invisible(outFiles))
  }
  for (nm in names(scales)[!need]) message("  ", nm, " cache hit -- skipping")
  for (nm in names(scales)[need]) message("  ", nm, " needs (re)computation")

  # Raw file only matters when something must actually be computed (on EVE
  # the raw maps may be absent while every output already exists).
  if (is.na(rawFile)) {
    warning("No raw land use file found for year ", year, " -- skipping.")
    return(invisible(NULL))
  }

  message("  Source file: ", basename(rawFile),
          if (recode) " (CTM classes recoded to HCTM classes)" else "",
          if (!is.na(hedgeFile) && hedgeFile != rawFile) paste0("; hedges from ", basename(hedgeFile)) else "")

  ctmCats <- landuseCategoriesCTM()
  classScheme <- getLanduseCategories(basename(classFile))
  P <- if (recode) loadLanduseRecode(recodeFile) else NULL

  message("  Loading and reprojecting crop type map...")
  # method = "near" critical for categorical data
  cropMap <- projectIfNeeded(terra::rast(classFile), targetCRS, method = "near")
  hedgeMap <- if (is.na(hedgeFile)) NULL else if (hedgeFile == classFile) cropMap else projectIfNeeded(terra::rast(hedgeFile), targetCRS, method = "near")

  allCatNames <- c("grassland", "winter_cereals", "summer_cereals", "maize",
                    "root_crops", "rapeseed", "sunflower", "vegetables",
                    "legumes", "hedges", "fallow", "grapevine", "hops",
                    "orchards_and_berries")
  neededNames <- names(scales)[need]

  message("  Computing ", length(allCatNames), " category proportions (hedges ", if (is.null(hedgeMap)) "NA: not mapped for this year" else "from CTM", ")...")

  for (nm in neededNames) {
    s <- scales[[nm]]
    layers <- list()

    if (recode) {
      # CTM class shares of the cell -> the class shares HCTM would have mapped (hedges stay the CTM hedges)
      fr <- lapply(names(ctmCats), function(cn) makeCategoryProportionLayer(cropMap, ctmCats[[cn]], s$res, cn, targetCRS))
      names(fr) <- names(ctmCats)
      mapped <- if (all(c("other_mapped", "nodata") %in% rownames(P))) makeMappedShareLayer(cropMap, s$res, targetCRS) else NULL      # 2026-11-06
      layers <- applyLanduseRecode(fr, P, mapped = mapped)
      layers[["hedges"]] <- fr[["hedges"]]
    } else {
      for (cn in setdiff(names(classScheme$cats), "hedges")) {
        message("    ", nm, ": ", cn)
        layers[[cn]] <- makeCategoryProportionLayer(cropMap, classScheme$cats[[cn]], s$res, cn, targetCRS)
      }
      if (is.null(hedgeMap)) {
        # no CTM map this year: hedges not mapped -- fill with NA
        na <- makeCategoryProportionLayer(cropMap, -9999, s$res, "hedges", targetCRS)
        na[] <- NA
        layers[["hedges"]] <- na
      } else {
        hl <- makeCategoryProportionLayer(hedgeMap, ctmCats[["hedges"]], s$res, "hedges", targetCRS)
        # The hedges come from CTM (10 m), the classes from HCTM (30 m): the two maps have another extent and another grid origin,
        # so the hedge layer is put on the grid of the class layers before they are stacked (found 2026-11-04: "[rast] extents do not match" in 2017)
        if (!isTRUE(terra::compareGeom(hl, layers[[1]], stopOnError = FALSE))) {
          hl <- terra::resample(hl, layers[[1]], method = "bilinear")
          names(hl) <- "hedges"
        }
        layers[["hedges"]] <- hl
      }
    }

    layers <- layers[allCatNames]
    terra::writeRaster(terra::rast(layers), outFiles[[nm]], overwrite = TRUE)
    writeLines(schemeTag, paste0(outFiles[[nm]], ".scheme.txt"))
    sfx <- paste0(outFiles[[nm]], ".hedgesfixed"); if (file.exists(sfx)) file.remove(sfx)         # raw hedges again: completeHedges() must see this file as data, not as already fixed
    message("  Saved ", nm, ": ", outFiles[[nm]])
  }

  invisible(outFiles)
}
