#' Prepare German landscape-scale occurrence data (1km scale), from DDA
#' territory data by default, or from MhB point counts for specific species
#'
#' For species using the default data source: loads DDA territory and
#' visited-route data, builds the full presence/absence matrix
#' (Reviere > 0 -> presence). For species in `perSpeciesDataSource` marked
#' `"MhB point counts"` instead (e.g. Buteo buteo/Sturnus vulgaris -- see
#' DECISIONS.md's "Buteo/Sturnus landscape data source" entry for why):
#' builds presence/absence from the same raw MhB point-count CSV
#' `occurrencePrepGerHabitat()` uses, but aggregated to the ROUTE level
#' instead of the 200m cell level -- any qualifying breeding-season
#' detection at a route in a year is a presence; any OTHER route surveyed
#' that year (a qualifying detection of any OTHER MhB-routed species there)
#' with no detection of this species is an absence. Both pipelines then
#' join to the same Probeflaechen shapefile, extract landscape covariates
#' per year via `loadCovariates()`, spatially thin, and save one RDS per
#' species/year -- identical downstream handling regardless of data source.
#'
#' NOTE: expects `landcover_<corineYear>_landscape.tif` to already exist
#' in `landscapeOutputDir` (via `loadCovariates()`) -- CORINE land cover
#' processing is out of scope for this module and must be supplied
#' separately.
#'
#' @param ddaTerritoriesXlsxPath Character. Path to the DDA territories xlsx.
#'   Only read if at least one species uses the default DDA data source.
#' @param ddaVisitsXlsxPath Character. Path to the DDA visited-routes xlsx.
#'   Only read if at least one species uses the default DDA data source.
#' @param probeflaechenShpPath Character. Path to the Probeflaechen shapefile.
#' @param landscapeOutputDir Character. Directory with landscape-scale
#'   covariate rasters, and where outputs are read.
#' @param habitatOutputDir Character. Directory with habitat-scale rasters
#'   (passed through to `loadCovariates()`; currently unused there).
#' @param outputDir Character. Directory to save per-species-year RDS files in.
#' @param species Character vector of Latin species names to process.
#' @param landscapeYears Integer vector of years to process.
#' @param localeCtype Character. Locale for German special characters.
#' @param thinDist Numeric. Default spatial thinning distance in metres, used
#'   for any species without its own entry in `perSpeciesThinDist`.
#' @param perSpeciesThinDist Named numeric vector/list, or NULL (default).
#'   Per-species thinning distance overrides, keyed by species Latin name --
#'   e.g. sourced from `speciesConfig_general.csv`'s `thinning_dist_m`
#'   column (landscape rows) via `loadSpeciesGeneralConfig()`.
#' @param useThinning Logical. Should occurrence points be spatially thinned?
#'   Does NOT restore abundance data when FALSE -- both data sources are
#'   already binarized to presence/absence upstream of thinning.
#' @param mhbObsPath Character, or NULL (default). Path to the raw MhB point
#'   count CSV -- required if any species uses the `"MhB point counts"` data
#'   source below.
#' @param perSpeciesDataSource Named character vector/list, or NULL (default,
#'   every species uses DDA territories). species -> `"DDA territories"`/
#'   `"MhB point counts"` -- e.g. sourced from `speciesConfig_general.csv`'s
#'   `data_source` column (landscape rows) via `loadSpeciesGeneralConfig()`.
#' @return Invisibly, a named character vector of output file paths.
occurrencePrepGerLandscape <- function(ddaTerritoriesXlsxPath, ddaVisitsXlsxPath,
                                        probeflaechenShpPath, landscapeOutputDir,
                                        habitatOutputDir, outputDir, species,
                                        landscapeYears, localeCtype = "de_DE.UTF-8",
                                        thinDist = 2000, perSpeciesThinDist = NULL,
                                        useThinning = TRUE, mhbObsPath = NULL,
                                        perSpeciesDataSource = NULL) {

  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  Sys.setlocale("LC_CTYPE", localeCtype)

  lookup <- speciesLookup()

  mhbRoutedSpecies <- if (!is.null(perSpeciesDataSource)) {
    intersect(species, names(perSpeciesDataSource)[unlist(perSpeciesDataSource) == "MhB point counts"])
  } else {
    character(0)
  }
  ddaRoutedSpecies <- setdiff(species, mhbRoutedSpecies)

  if (length(mhbRoutedSpecies) > 0 && is.null(mhbObsPath)) {
    stop("perSpeciesDataSource lists \"MhB point counts\" for ",
         paste(mhbRoutedSpecies, collapse = ", "), " but mhbObsPath was not supplied.")
  }

  message("Loading Probeflaechen shapefile...")
  probeflaechen <- sf::st_read(probeflaechenShpPath, quiet = TRUE)
  probeflaechen <- sf::st_transform(probeflaechen, 3035)
  coords <- sf::st_coordinates(sf::st_centroid(probeflaechen))
  probeflaechen$x <- coords[, 1]
  probeflaechen$y <- coords[, 2]

  shpCols <- names(probeflaechen)
  routeCol <- intersect(c("ROUTENCODE", "Routencode", "routencode",
                           "ROUTE_CODE", "SITE_ID"), shpCols)[1]
  probeflaechen <- probeflaechen |>
    dplyr::rename(ROUTENCODE = dplyr::all_of(routeCol)) |>
    dplyr::mutate(ROUTENCODE = tolower(ROUTENCODE))

  message("  Probeflaechen: ", nrow(probeflaechen), " routes")

  occSpatial <- NULL
  if (length(ddaRoutedSpecies) > 0) {
    message("Loading DDA territory data (", paste(ddaRoutedSpecies, collapse = ", "), ")...")
    territories <- readxl::read_xlsx(ddaTerritoriesXlsxPath)

    message("Loading DDA visited routes data...")
    visits <- readxl::read_xlsx(ddaVisitsXlsxPath)

    message("Cleaning DDA data...")
    territories <- territories |>
      dplyr::mutate(species = enc2utf8(`Art deutsch`),
                     ROUTENCODE = tolower(ROUTENCODE),
                     Jahr = as.character(format(Countdate, "%Y")))

    visits <- visits |>
      dplyr::mutate(ROUTENCODE = tolower(ROUTENCODE),
                     Jahr = as.character(Jahr)) |>
      tidyr::unite("route_yr", ROUTENCODE, Jahr, remove = FALSE)

    visits <- visits |> dplyr::filter(Jahr %in% as.character(landscapeYears))
    territories <- territories |> dplyr::filter(Jahr %in% as.character(landscapeYears))

    message("Building presence/absence matrix...")
    focalGerman <- lookup$german[lookup$latin %in% ddaRoutedSpecies]

    allCombos <- expand.grid(route_yr = unique(visits$route_yr),
                              species = focalGerman,
                              stringsAsFactors = FALSE) |>
      tidyr::separate(route_yr, into = c("ROUTENCODE", "Jahr"), sep = "_", remove = FALSE) |>
      dplyr::left_join(territories |>
                          dplyr::select(ROUTENCODE, Jahr, species, Reviere),
                        by = c("ROUTENCODE", "Jahr", "species")) |>
      dplyr::mutate(Reviere = tidyr::replace_na(Reviere, 0),
                     occurrence = as.integer(Reviere > 0)) |>
      dplyr::left_join(lookup, by = c("species" = "german")) |>
      dplyr::rename(latin_name = latin, german_name = species)

    occSpatial <- allCombos |>
      dplyr::inner_join(probeflaechen |>
                           dplyr::select(ROUTENCODE, x, y) |>
                           sf::st_drop_geometry(),
                         by = "ROUTENCODE")

    message("  DDA-routed PA records: ", nrow(occSpatial))
  }

  occSpatialMhB <- NULL
  if (length(mhbRoutedSpecies) > 0) {
    message("Loading raw MhB point count data for landscape-scale routing (",
            paste(mhbRoutedSpecies, collapse = ", "), ")...")
    mhbRaw <- read.csv(mhbObsPath, header = TRUE)
    mhbRaw <- mhbRaw |>
      dplyr::mutate(date = as.Date(DATE_TIME, "%Y-%m-%d"),
                     year = as.integer(format(date, "%Y")),
                     month = format(date, "%m"))

    focalGermanMhB <- lookup$german[lookup$latin %in% mhbRoutedSpecies]

    # Same breeding-season/evidence-quality filter occurrencePrepGerHabitat()
    # applies by default (excludes the weakest "A"-with-no-number tier),
    # kept consistent since this reuses the exact same raw dataset.
    dfMhB <- mhbRaw |>
      dplyr::filter(year %in% landscapeYears,
                     month %in% c("04", "05", "06"),
                     ATLAS_CODE %in% c("A1", "A2",
                                        "B3", "B4", "B5", "B6", "B7", "B8", "B9",
                                        "C10", "C11a", "C11b", "C12", "C13a", "C13b",
                                        "C14a", "C14b", "C15", "C16")) |>
      dplyr::mutate(ROUTENCODE = tolower(AREA_NATIONAL_CODE))

    # "Surveyed" proxy: a route/year is treated as surveyed if it has a
    # qualifying detection of ANY MhB-routed species -- the same convention
    # occurrencePrepGerHabitat() uses (MhB has no separate "visited, saw
    # nothing" log; a recorded detection of anything is the only signal
    # that a route was actually surveyed that year).
    surveyedRouteYr <- dfMhB |> dplyr::distinct(ROUTENCODE, year)

    presenceRouteYr <- dfMhB |>
      dplyr::filter(SPECIES_NAME_GERMAN %in% focalGermanMhB) |>
      dplyr::left_join(lookup, by = c("SPECIES_NAME_GERMAN" = "german")) |>
      dplyr::rename(latin_name = latin) |>
      dplyr::distinct(ROUTENCODE, year, latin_name)

    allCombosMhB <- tidyr::crossing(surveyedRouteYr, latin_name = mhbRoutedSpecies) |>
      dplyr::left_join(presenceRouteYr |> dplyr::mutate(present = 1L),
                        by = c("ROUTENCODE", "year", "latin_name")) |>
      dplyr::mutate(occurrence = tidyr::replace_na(present, 0L)) |>
      dplyr::select(-present) |>
      dplyr::rename(Jahr = year) |>
      dplyr::mutate(Jahr = as.character(Jahr))

    occSpatialMhB <- allCombosMhB |>
      dplyr::inner_join(probeflaechen |>
                           dplyr::select(ROUTENCODE, x, y) |>
                           sf::st_drop_geometry(),
                         by = "ROUTENCODE")

    message("  MhB-routed PA records: ", nrow(occSpatialMhB))
  }

  occSpatial <- dplyr::bind_rows(occSpatial, occSpatialMhB)

  message("  Total PA records: ", nrow(occSpatial))

  message("\nProcessing ", length(species), " species x ", length(landscapeYears), " years...")

  outFiles <- list()

  for (spLatin in species) {
    spClean <- gsub(" ", "_", spLatin)

    message("\n  -- ", spLatin, " --------------------")

    for (yr in landscapeYears) {

      outFile <- file.path(outputDir, paste0(spClean, "_landscape_", yr, ".rds"))

      if (isValidRDSFile(outFile)) {
        message("  Cache hit -- skipping: ", spLatin, " ", yr)
        outFiles[[paste(spLatin, yr)]] <- outFile
        next
      }

      spYr <- occSpatial |>
        dplyr::filter(latin_name == spLatin, Jahr == as.character(yr))

      nPres <- sum(spYr$occurrence == 1)
      nAbs <- sum(spYr$occurrence == 0)

      if (nrow(spYr) == 0) {
        message("  No records for ", spLatin, " ", yr, " -- skipping")
        next
      }
      if (nPres < 10) {
        warning("  Too few presences (", nPres, ") for ", spLatin, " in ", yr, " -- skipping")
        next
      }

      message("  ", spLatin, " ", yr, ": ", nPres, " pres / ", nAbs, " abs")

      covStack <- loadCovariates(yr, landscapeOutputDir, habitatOutputDir)
      if (is.null(covStack)) {
        warning("  Covariates unavailable for ", yr, " -- skipping")
        next
      }

      coordsMat <- as.matrix(spYr[, c("x", "y")])
      envVals <- terra::extract(covStack, coordsMat)

      spYrEnv <- cbind(spYr, envVals)

      nBefore <- nrow(spYrEnv)
      spYrEnv <- spYrEnv[stats::complete.cases(spYrEnv[, names(covStack)]), ]
      nAfter <- nrow(spYrEnv)

      if (nBefore > nAfter) {
        message("  Removed ", nBefore - nAfter, " rows with NA covariates")
      }

      if (sum(spYrEnv$occurrence == 1) < 10) {
        warning("  Too few presences after NA removal for ", spLatin, " ", yr, " -- skipping")
        next
      }

      if (useThinning) {
        spThinDist <- if (!is.null(perSpeciesThinDist) && spLatin %in% names(perSpeciesThinDist)) {
          perSpeciesThinDist[[spLatin]]
        } else {
          thinDist
        }
        message("  Thinning at ", spThinDist / 1000, "km...")
        spSf <- sf::st_as_sf(spYrEnv, coords = c("x", "y"), crs = 3035)

        spThinned <- thin(spSf, thinDist = spThinDist, runs = 5)
        thinnedCoords <- sf::st_coordinates(spThinned)
        spThinnedDf <- sf::st_drop_geometry(spThinned)
        spThinnedDf$x <- thinnedCoords[, 1]
        spThinnedDf$y <- thinnedCoords[, 2]

        message("  After thinning: ", nrow(spThinnedDf),
                " (", sum(spThinnedDf$occurrence == 1), " pres / ",
                sum(spThinnedDf$occurrence == 0), " abs)")
      } else {
        message("  Spatial thinning disabled -- keeping all ", nrow(spYrEnv), " rows")
        spThinnedDf <- spYrEnv
      }

      saveRDS(spThinnedDf, outFile)
      message("  Saved -> ", outFile)
      outFiles[[paste(spLatin, yr)]] <- outFile
    }
  }

  message("\nDone. Output files in: ", outputDir)
  invisible(unlist(outFiles))
}
