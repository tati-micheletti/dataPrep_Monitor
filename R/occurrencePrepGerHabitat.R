#' Prepare German point count data for the habitat SDM (200m scale)
#'
#' Filters raw MhB point count observations to the breeding season and
#' focal species/years, overlays them on the 200m reference grid, builds
#' presences/absences per route x year x species, spatially thins at
#' 400m, extracts habitat covariates, and saves one RDS per species/year.
#'
#' NOTE: expects `landcover_<corineYear>_habitat.tif` to already exist in
#' `habitatOutputDir` -- CORINE land cover processing is out of scope for
#' this module and must be supplied separately.
#'
#' Follows Wiedenroth et al. 03a_occurrence-prep_200m.R with adaptations
#' for multi-year data.
#'
#' @param mhbObsPath Character. Path to the raw MhB point count CSV.
#' @param probeflaechenShpPath Character. Path to the Probeflaechen shapefile.
#' @param habitatOutputDir Character. Directory with habitat-scale covariate
#'   rasters (landuse/landcover/DEM derivatives, and where outputs are read).
#' @param outputDir Character. Directory to save per-species-year RDS files in.
#' @param species Character vector of Latin species names to process.
#' @param habitatYears Integer vector of years to process.
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
#' @return Invisibly, a named character vector of output file paths.
occurrencePrepGerHabitat <- function(mhbObsPath, probeflaechenShpPath,
                                      habitatOutputDir, outputDir, species,
                                      habitatYears, localeCtype = "de_DE.UTF-8",
                                      thinDist = 400, perSpeciesThinDist = NULL,
                                      useThinning = TRUE, brutzeitcodeFilter = NULL) {

  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  Sys.setlocale("LC_CTYPE", localeCtype)

  # Reference raster: solar radiation at 200m -- has the most NAs at
  # borders, giving a conservative exclusion of edge cells with
  # incomplete covariate data. Following Wiedenroth et al.
  refRaster <- terra::rast(file.path(habitatOutputDir, "solar_radiation_habitat.tif"))
  message("Reference raster (200m solar radiation):")
  message("CRS: ", terra::crs(refRaster, describe = TRUE)$code)
  message("Resolution: ", paste(terra::res(refRaster), collapse = " x "), "m")
  message("Extent: ", paste(as.vector(terra::ext(refRaster)), collapse = " "))

  message("Loading raw point count data...")
  mhbRaw <- read.csv(mhbObsPath, header = TRUE)

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

  message("Extracting 200m grid cell references...")
  cellRefs <- terra::extract(refRaster, df[, c("x", "y")], cells = TRUE)
  df$cell <- cellRefs$cell
  df$ref_val <- cellRefs[, 2]

  nBefore <- nrow(df)
  df <- df[!is.na(df$ref_val), ]
  message("Removed ", nBefore - nrow(df), " observations outside study area")

  message("\nBuilding full PA matrix...")
  surveyedRoutes <- df |> dplyr::distinct(AREA_NATIONAL_CODE, year)
  message("Surveyed route x year combinations: ", nrow(surveyedRoutes))

  message("\nProcessing ", length(species), " species x ", length(habitatYears), " years...")

  outFiles <- list()

  for (sp in species) {
    spClean <- gsub(" ", "_", sp)

    message("\n  -- ", sp, " --------------------------")

    for (yr in habitatYears) {

      outFile <- file.path(outputDir, paste0(spClean, "_habitat_", yr, ".rds"))

      if (isValidRDSFile(outFile)) {
        message("Cache hit -- skipping: ", sp, " ", yr)
        outFiles[[paste(sp, yr)]] <- outFile
        next
      }

      spPres <- df |> dplyr::filter(latin_name == sp, year == yr)

      if (!is.null(brutzeitcodeFilter) && sp %in% names(brutzeitcodeFilter)) {
        prefix <- brutzeitcodeFilter[[sp]]
        nBeforeCode <- nrow(spPres)
        spPres <- spPres[startsWith(spPres$ATLAS_CODE, prefix), ]
        message("  ATLAS_CODE filter '", prefix, "*' for ", sp, ": ",
                nBeforeCode, " -> ", nrow(spPres), " records")
      }

      spPresNodup <- spPres[!duplicated(spPres$cell), ]

      nPres <- nrow(spPresNodup)
      message("  ", sp, " ", yr, ": ", nPres, " presence cells")

      if (nPres < 10) {
        warning("Too few presences (", nPres, ") for ", sp, " ", yr, " -- skipping")
        next
      }

      surveyedThisYr <- unique(df$AREA_NATIONAL_CODE[df$year == yr])
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
        spThinDist <- if (!is.null(perSpeciesThinDist) && sp %in% names(perSpeciesThinDist)) {
          perSpeciesThinDist[[sp]]
        } else {
          thinDist
        }
        message("Spatial thinning at ", spThinDist, "m...")
        paSf <- sf::st_as_sf(paDf, coords = c("x", "y"), crs = 3035)

        paThinned <- thin(paSf, thinDist = spThinDist, runs = 5)
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

      corineYr <- corineYear(yr)

      lcFile <- file.path(habitatOutputDir, paste0("landcover_", corineYr, "_habitat.tif"))
      luFile <- file.path(habitatOutputDir, paste0("landuse_", yr, "_habitat.tif"))
      demFiles <- c(file.path(habitatOutputDir, "elevation_habitat.tif"),
                    file.path(habitatOutputDir, "slope_habitat.tif"),
                    file.path(habitatOutputDir, "solar_radiation_habitat.tif"))

      missingFiles <- c(lcFile, luFile, demFiles)[!file.exists(c(lcFile, luFile, demFiles))]
      if (length(missingFiles) > 0) {
        warning("Missing covariate files:\n", paste(" ", missingFiles, collapse = "\n"))
        next
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

      covStack <- c(lu, lc, elev, slope, solar)

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
                                              paste0("landuse_", refYear, "_habitat.tif")))
            hedgeRef <- luRefYr[["hedges"]]
            hedgeRef <- terra::resample(hedgeRef, covStack[[1]], method = "bilinear")
            names(hedgeRef) <- hedgeCol
            covStack[[hedgeCol]] <- hedgeRef
          }
        }
      }

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
        next
      }

      saveRDS(paEnv, outFile)
      message("Saved -> ", outFile)
      outFiles[[paste(sp, yr)]] <- outFile
    }
  }

  message("\nDone. Output files in: ", outputDir)
  invisible(unlist(outFiles))
}
