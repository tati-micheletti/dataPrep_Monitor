#' Prepare German point count data for the habitat SDM (200m scale)
#'
#' Filters raw MhB point count observations to the breeding season and
#' focal species/years, overlays them on the habitat reference grid,
#' builds presences/absences per route x year x species, spatially thins,
#' extracts habitat covariates, and saves one RDS per species/year.
#'
#' Species are grouped by their resolved habitat resolution
#' (`resolutionConfig`, falling back to `sharedResolutionM` -- see
#' DECISIONS.md's 2026-09-28 entry); the reference-grid/cell-extraction
#' pass (which genuinely depends on the grid's resolution) runs once per
#' distinct resolution group, not once globally. With no species
#' overridden (today's default), there is exactly one group and behavior
#' is identical to the single-shared-grid version this replaced.
#'
#' NOTE: expects `landcover_<corineYear>_habitat_<scaleLabel>.tif` to
#' already exist in each resolution group's own processed folder --
#' CORINE land cover processing is out of scope for this module and must
#' be supplied separately.
#'
#' Follows Wiedenroth et al. 03a_occurrence-prep_200m.R with adaptations
#' for multi-year data.
#'
#' @param mhbObsPath Character. Path to the raw MhB point count CSV.
#' @param probeflaechenShpPath Character. Path to the Probeflaechen shapefile.
#' @param processedRoot Character. `predictors/processed` directory
#'   (without the scale_X leaf -- each resolution group's own leaf, via
#'   `scaleLabel()`, is appended internally).
#' @param outputDir Character. Directory to save per-species-year RDS files in.
#' @param species Character vector of Latin species names to process.
#' @param habitatYears Integer vector of years to process.
#' @param resolutionConfig Named list, species -> scale -> resolution (m), or
#'   NULL (default). See `dataPrep_Monitor`'s parameter of the same name.
#' @param sharedResolutionM Numeric. Shared default habitat resolution (m),
#'   used for any species absent from `resolutionConfig`.
#' @param localeCtype Character. Locale for German special characters.
#' @param thinDist Numeric. Default spatial thinning distance in metres, used
#'   for any species without its own entry in `perSpeciesThinDist`.
#' @param perSpeciesThinDist Named numeric vector/list, or NULL (default).
#'   Per-species thinning distance overrides, keyed by species Latin name --
#'   e.g. sourced from `speciesConfig_general.csv`'s `thinning_dist_m`
#'   column (habitat rows) via `loadSpeciesGeneralConfig()`.
#' @param useThinning Logical. Should occurrence points be spatially thinned?
#'   Does NOT restore abundance data when FALSE -- `TOTAL_COUNT` is already
#'   discarded (deduplicated to one presence per cell) upstream of thinning.
#' @param brutzeitcodeFilter Named character vector/list, or NULL (default).
#'   Per-species ATLAS_CODE PREFIX filter, keyed by species Latin name (e.g.
#'   `"C"` keeps only confirmed-breeding codes C10/C11a/.../C16, matching the
#'   German atlas A=possible/B=probable/C=confirmed convention). Applied ON
#'   TOP OF the existing global ATLAS_CODE filter below (which already
#'   excludes the weakest "A"-with-no-number tier for everyone) -- a species
#'   without an entry here keeps that global filter's full range unchanged.
#'   Sourced from `speciesConfig_general.csv`'s `brutzeitcode_filter` column
#'   (habitat rows only -- DDA territories/MhB-landscape have no ATLAS_CODE).
#' @param cachePath Character, or NULL (default). Directory for
#'   `reproducible::Cache()`'s per-species-year cache (see
#'   `buildHabitatSpeciesYear()`) -- e.g. `cachePath(sim)` when run via
#'   dataPrep_Monitor, a stable location shared across runs. NULL falls
#'   back to a temp directory, for standalone/test calls.
#' @return Invisibly, a named character vector of output file paths.
occurrencePrepGerHabitat <- function(mhbObsPath, probeflaechenShpPath,
                                      processedRoot, outputDir, species,
                                      habitatYears, resolutionConfig = NULL,
                                      sharedResolutionM, localeCtype = "de_DE.UTF-8",
                                      thinDist = 400, perSpeciesThinDist = NULL,
                                      useThinning = TRUE, brutzeitcodeFilter = NULL,
                                      cachePath = NULL) {

  if (is.null(cachePath)) cachePath <- file.path(tempdir(), "birdMonitor_cache")

  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  Sys.setlocale("LC_CTYPE", localeCtype)

  message("Loading raw point count data...")
  # fileEncoding is explicit (not left to the process's ambient locale) because
  # this file is UTF-8 -- see the matching note in sharedSpeciesCanonical.R's
  # loadSpeciesCanonical() for why leaving this to locale guessing is unsafe.
  mhbRaw <- read.csv(mhbObsPath, header = TRUE, fileEncoding = "UTF-8")

  mhbRaw <- mhbRaw |>
    dplyr::mutate(date = as.Date(DATE_TIME, "%Y-%m-%d"),
                   year = as.integer(format(date, "%Y")),
                   month = format(date, "%m"))

  message("Filtering to ", length(habitatYears), " years, ", length(species), " species...")

  # Filters directly on the raw MhB CSV's own SPECIES_NAME_SCIENTIFIC column
  # (it has both German and scientific names natively) -- deliberately NOT
  # routed through a Latin<->German lookup at all, unlike landscape scale
  # (occurrencePrepGerLandscape(), which has no choice: the raw DDA data has
  # no Latin-name column). One less place a name-lookup mismatch can
  # silently drop a species (see speciesCanonical.csv's docstring for the
  # 2026-09-26 incident this simplification is a direct response to).
  df <- mhbRaw |>
    dplyr::filter(year %in% habitatYears,
                   SPECIES_NAME_SCIENTIFIC %in% species,
                   TOTAL_COUNT > 0,
                   month %in% c("04", "05", "06"),
                   ATLAS_CODE %in% c("A1", "A2",
                                      "B3", "B4", "B5", "B6", "B7", "B8", "B9",
                                      "C10", "C11a", "C11b", "C12", "C13a", "C13b",
                                      "C14a", "C14b", "C15", "C16")) |>
    dplyr::mutate(AREA_NATIONAL_CODE = tolower(AREA_NATIONAL_CODE)) |>
    dplyr::rename(latin_name = SPECIES_NAME_SCIENTIFIC)

  message("Records after filtering: ", nrow(df))
  message("Species: ", paste(unique(df$latin_name), collapse = ", "))
  message("Years: ", paste(sort(unique(df$year)), collapse = ", "))

  message("\nLoading Probeflaechen shapefile...")
  pf <- sf::st_read(probeflaechenShpPath, quiet = TRUE) |>
    sf::st_transform(3035) |>
    dplyr::rename(AREA_NATIONAL_CODE = SITE_ID) |>
    dplyr::mutate(AREA_NATIONAL_CODE = tolower(AREA_NATIONAL_CODE))

  pfCentroids <- sf::st_centroid(pf)
  pfCoords <- sf::st_coordinates(pfCentroids)
  pf$x_cent <- pfCoords[, 1]
  pf$y_cent <- pfCoords[, 2]

  message("Routes in shapefile: ", nrow(pf))

  message("\nReprojecting presences to EPSG:3035...")
  dfSf <- sf::st_as_sf(df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) |>
    sf::st_transform(3035)

  coords3035 <- sf::st_coordinates(dfSf)
  df$x <- coords3035[, 1]
  df$y <- coords3035[, 2]

  # Species-independent: any species' qualifying detection at a route/year
  # counts as "surveyed" -- computed once from the FULL (all-species) table
  # so a resolution group never sees a route as "unsurveyed" just because
  # the detection that actually surveyed it belongs to a species in a
  # DIFFERENT resolution group.
  surveyedByYear <- stats::setNames(
    lapply(habitatYears, function(yr) unique(df$AREA_NATIONAL_CODE[df$year == yr])),
    as.character(habitatYears))

  # Group species by resolved habitat resolution (usually just one group,
  # the shared default -- more than one only when a species has its own
  # resolution_m override; see DECISIONS.md's 2026-09-28 entry).
  resolvedRes <- vapply(species, function(sp) {
    v <- if (!is.null(resolutionConfig)) resolutionConfig[[sp]]$habitat else NULL
    if (is.null(v) || is.na(v)) sharedResolutionM else v
  }, numeric(1))
  resGroups <- split(species, resolvedRes)

  message("\nProcessing ", length(species), " species x ", length(habitatYears),
          " years, in ", length(resGroups), " resolution group(s)...")

  outFiles <- list()

  for (resKey in names(resGroups)) {
    resM <- as.numeric(resKey)
    groupSpecies <- resGroups[[resKey]]
    habitatOutputDir <- file.path(processedRoot, scaleLabel(resM))

    message("\n=== Resolution group ", scaleLabel(resM), " (", resM, "m): ",
            paste(groupSpecies, collapse = ", "), " ===")

    # Reference raster: solar radiation at this group's own resolution --
    # has the most NAs at borders, giving a conservative exclusion of edge
    # cells with incomplete covariate data. Following Wiedenroth et al.
    # Resolution appended to the filename (second safety layer beyond the
    # containing scaleLabel()-named folder, habitatOutputDir itself).
    refRaster <- terra::rast(file.path(habitatOutputDir,
                                        paste0("solar_radiation_habitat_", basename(habitatOutputDir), ".tif")))
    message("Reference raster (", resM, "m solar radiation):")
    message("CRS: ", terra::crs(refRaster, describe = TRUE)$code)
    message("Resolution: ", paste(terra::res(refRaster), collapse = " x "), "m")
    message("Extent: ", paste(as.vector(terra::ext(refRaster)), collapse = " "))

    # Cell references depend on this group's own grid -- scoped to just
    # this group's species rows (a species in another group is skipped,
    # not extracted against the wrong grid).
    dfGroup <- df[df$latin_name %in% groupSpecies, ]
    cellRefs <- terra::extract(refRaster, dfGroup[, c("x", "y")], cells = TRUE)
    dfGroup$cell <- cellRefs$cell
    dfGroup$ref_val <- cellRefs[, 2]

    nBefore <- nrow(dfGroup)
    dfGroup <- dfGroup[!is.na(dfGroup$ref_val), ]
    message("Removed ", nBefore - nrow(dfGroup), " observations outside study area (this group)")

    message("\nBuilding full PA matrix...")
    surveyedRoutes <- dfGroup |> dplyr::distinct(AREA_NATIONAL_CODE, year)
    message("Surveyed route x year combinations: ", nrow(surveyedRoutes))

    for (yr in habitatYears) {

      # Hoisted per-year (not per-species-year, as before): the covariate
      # stack doesn't depend on the focal species, so it's computed once
      # and shared across this group's species.
      surveyedThisYr <- surveyedByYear[[as.character(yr)]]

      covStack <- buildHabitatCovStackOneYear(yr, habitatOutputDir)
      if (is.null(covStack)) next

      for (sp in groupSpecies) {
        spClean <- gsub(" ", "_", sp)
        outFile <- file.path(outputDir, paste0(spClean, "_habitat_", yr, ".rds"))

        spPres <- dfGroup |> dplyr::filter(latin_name == sp, year == yr)

        if (!is.null(brutzeitcodeFilter) && sp %in% names(brutzeitcodeFilter)) {
          prefix <- brutzeitcodeFilter[[sp]]
          nBeforeCode <- nrow(spPres)
          spPres <- spPres[startsWith(spPres$ATLAS_CODE, prefix), ]
          message("  ATLAS_CODE filter '", prefix, "*' for ", sp, ": ",
                  nBeforeCode, " -> ", nrow(spPres), " records")
        }

        spThinDist <- if (!is.null(perSpeciesThinDist) && sp %in% names(perSpeciesThinDist)) {
          perSpeciesThinDist[[sp]]
        } else {
          thinDist
        }

        result <- reproducible::Cache(
          buildHabitatSpeciesYear, sp = sp, yr = yr, spPres = spPres,
          surveyedThisYr = surveyedThisYr, pf = pf, refRaster = refRaster,
          covStack = covStack, useThinning = useThinning, thinDist = spThinDist,
          cachePath = cachePath,
          userTags = c("occurrencePrepGerHabitat", spClean, as.character(yr)))

        if (isCachedNull(result)) next

        saveRDS(result, outFile)
        message("Saved -> ", outFile)
        outFiles[[paste(sp, yr)]] <- outFile
      }
    }
  }

  message("\nDone. Output files in: ", outputDir)
  invisible(unlist(outFiles))
}

#' Build one year's habitat covariate stack (hoisted out of the species loop)
#'
#' @param yr Integer. Year.
#' @param habitatOutputDir Character. Directory with habitat-scale covariate rasters.
#' @return SpatRaster, or NULL if required files are missing.
buildHabitatCovStackOneYear <- function(yr, habitatOutputDir) {
  corineYr <- corineYear(yr)
  # Resolution appended to every filename below (second safety layer beyond
  # the containing scaleLabel()-named folder, habitatOutputDir itself).
  resSuffix <- basename(habitatOutputDir)

  lcFile <- file.path(habitatOutputDir, paste0("landcover_", corineYr, "_habitat_", resSuffix, ".tif"))
  luFile <- file.path(habitatOutputDir, paste0("landuse_", yr, "_habitat_", resSuffix, ".tif"))
  demFiles <- c(file.path(habitatOutputDir, paste0("elevation_habitat_", resSuffix, ".tif")),
                file.path(habitatOutputDir, paste0("slope_habitat_", resSuffix, ".tif")),
                file.path(habitatOutputDir, paste0("solar_radiation_habitat_", resSuffix, ".tif")))

  missingFiles <- c(lcFile, luFile, demFiles)[!file.exists(c(lcFile, luFile, demFiles))]
  if (length(missingFiles) > 0) {
    warning("Missing covariate files:\n", paste(" ", missingFiles, collapse = "\n"))
    return(NULL)
  }

  lu <- terra::rast(luFile)
  lc <- terra::rast(lcFile)
  elev <- terra::rast(demFiles[1])
  slope <- terra::rast(demFiles[2])
  solar <- terra::rast(demFiles[3])

  luRef <- lu[[1]]
  lc <- terra::resample(lc, luRef, method = "bilinear")
  elev <- terra::resample(elev, luRef, method = "bilinear")
  slope <- terra::resample(slope, luRef, method = "bilinear")
  solar <- terra::resample(solar, luRef, method = "bilinear")

  # dist_to_woodland/landscape_heterogeneity (2026-09-29, hedges-backfill
  # alternatives) -- optional, same convention as loadHabitatCovariates()/
  # loadCovariates() (models_Monitor): a missing file just means that
  # candidate predictor isn't offered this run. This function is a
  # SEPARATE, parallel covariate-stack builder from those two (used here
  # for occurrence-prep training-point extraction, not prediction-raster
  # generation) -- confirmed 2026-09-30 it was missed when those two were
  # updated, causing a real "[PREDICTOR ERROR] ... NOT found in occurrence
  # data" failure for any species requesting either predictor at habitat
  # scale.
  extraLayers <- list()
  distFile <- file.path(habitatOutputDir, paste0("dist_to_woodland_", corineYr, "_habitat_", resSuffix, ".tif"))
  if (file.exists(distFile)) {
    dist <- terra::resample(terra::rast(distFile), luRef, method = "bilinear")
    terra::values(dist) <- terra::values(dist)
    extraLayers$dist_to_woodland <- dist
  }
  heteroFile <- file.path(habitatOutputDir, paste0("landscape_heterogeneity_", yr, "_habitat_", resSuffix, ".tif"))
  if (file.exists(heteroFile)) {
    extraLayers$landscape_heterogeneity <- terra::rast(heteroFile)
  }

  # combineLayersSafely() rather than a bare c() -- lc/elev/slope/solar/
  # dist are all resampled (derived, not read-straight-from-disk) rasters,
  # which can silently corrupt when c()-combined in some terra versions/
  # session states (see combineLayersSafely.R's docstring / DECISIONS.md
  # 2026-09-28) -- the same latent bug already fixed in loadCovariates()/
  # loadHabitatCovariates(), never previously applied here.
  covStack <- combineLayersSafely(c(as.list(lu), as.list(lc), list(elev, slope, solar), extraLayers))
  names(covStack) <- gsub("_\\d{4}$", "", names(covStack))

  hedgeCol <- names(covStack)[grepl("^hedges", names(covStack))]
  if (length(hedgeCol) > 0) {
    hedgeVals <- terra::values(covStack[[hedgeCol]])
    if (all(is.na(hedgeVals))) {
      refYear <- if (yr <= 2016) 2017L else
        if (yr %in% c(2022, 2023)) 2021L else
          NULL
      if (!is.null(refYear)) {
        message("Hedges NA -- backfilling from ", refYear)
        luRefYr <- terra::rast(file.path(habitatOutputDir,
                                          paste0("landuse_", refYear, "_habitat_", resSuffix, ".tif")))
        hedgeRef <- luRefYr[["hedges"]]
        hedgeRef <- terra::resample(hedgeRef, covStack[[1]], method = "bilinear")
        names(hedgeRef) <- hedgeCol
        covStack[[hedgeCol]] <- hedgeRef
      }
    }
  }

  covStack
}

#' Build one species+year's model-ready habitat occurrence table
#'
#' The actual per-unit computation `occurrencePrepGerHabitat()` wraps in
#' `reproducible::Cache()` -- see `buildLandscapeSpeciesYear()` in
#' `occurrencePrepGerLandscape.R` for the rationale (same pattern: Cache()'s
#' digest covers exactly this species' own data, not the whole
#' multi-species `df`, so an isolated config change only invalidates that
#' one species). `spPres` is already brutzeitcodeFilter-applied by the
#' caller, so that effect is baked into the digest too.
#'
#' @param sp Character. Species Latin name.
#' @param yr Integer. Year.
#' @param spPres data.frame. This species+year's raw presence records
#'   (already ATLAS_CODE-filtered by the caller).
#' @param surveyedThisYr Character vector. Routes surveyed this year
#'   (any species), for the absence side.
#' @param pf sf data.frame. Probeflaechen shapefile with route centroids.
#' @param refRaster SpatRaster. Reference grid for cell lookups.
#' @param covStack SpatRaster. This year's habitat covariate stack.
#' @param useThinning Logical. Should occurrence points be spatially thinned?
#' @param thinDist Numeric. This species' resolved thinning distance (m).
#' @return A data.frame ready to save, or NULL if there isn't enough data.
buildHabitatSpeciesYear <- function(sp, yr, spPres, surveyedThisYr, pf, refRaster,
                                     covStack, useThinning, thinDist) {
  spPresNodup <- spPres[!duplicated(spPres$cell), ]

  nPres <- nrow(spPresNodup)
  message("  ", sp, " ", yr, ": ", nPres, " presence cells")

  if (nPres < 10) {
    warning("Too few presences (", nPres, ") for ", sp, " ", yr, " -- skipping")
    return(NULL)
  }

  detectedRoutes <- unique(spPres$AREA_NATIONAL_CODE)
  absentRoutes <- setdiff(surveyedThisYr, detectedRoutes)

  absCoords <- pf |>
    dplyr::filter(AREA_NATIONAL_CODE %in% absentRoutes) |>
    sf::st_drop_geometry() |>
    dplyr::select(AREA_NATIONAL_CODE, x = x_cent, y = y_cent)

  absCellRefs <- terra::extract(refRaster, absCoords[, c("x", "y")], cells = TRUE)
  absCoords$cell <- absCellRefs$cell
  absCoords$ref_val <- absCellRefs[, 2]

  absCoords <- absCoords[!is.na(absCoords$ref_val), ]
  absCoords <- absCoords[!absCoords$cell %in% spPresNodup$cell, ]

  nAbs <- nrow(absCoords)
  message("Absences: ", nAbs)

  presDf <- spPresNodup |>
    dplyr::select(AREA_NATIONAL_CODE, x, y, cell, year) |>
    dplyr::mutate(occurrence = 1L, latin_name = sp)

  absDf <- absCoords |>
    dplyr::mutate(occurrence = 0L, latin_name = sp, year = yr)

  paDf <- dplyr::bind_rows(presDf, absDf)

  message("Total PA: ", nrow(paDf), " (", nPres, " pres / ", nAbs, " abs)")

  if (useThinning) {
    message("Spatial thinning at ", thinDist, "m...")
    paSf <- sf::st_as_sf(paDf, coords = c("x", "y"), crs = 3035)

    paThinned <- thin(paSf, thinDist = thinDist, runs = 5)
    thinnedCoords <- sf::st_coordinates(paThinned)
    paThinnedDf <- sf::st_drop_geometry(paThinned)
    paThinnedDf$x <- thinnedCoords[, 1]
    paThinnedDf$y <- thinnedCoords[, 2]

    message("After thinning: ", nrow(paThinnedDf),
            " (", sum(paThinnedDf$occurrence == 1), " pres / ",
            sum(paThinnedDf$occurrence == 0), " abs)")
  } else {
    message("Spatial thinning disabled -- keeping all ", nrow(paDf), " rows")
    paThinnedDf <- paDf
  }

  message("  Extracting habitat covariates...")

  coordsMat <- as.matrix(paThinnedDf[, c("x", "y")])
  envVals <- terra::extract(covStack, coordsMat)

  paEnv <- cbind(paThinnedDf, envVals)

  nonHedge <- names(covStack)[!grepl("^hedges", names(covStack))]
  nBefore <- nrow(paEnv)
  paEnv <- paEnv[stats::complete.cases(paEnv[, nonHedge]), ]
  if (nBefore > nrow(paEnv)) {
    message("Removed ", nBefore - nrow(paEnv), " rows with NA covariates")
  }

  if (sum(paEnv$occurrence == 1) < 10) {
    warning("Too few presences after NA removal -- skipping")
    return(NULL)
  }

  paEnv
}
