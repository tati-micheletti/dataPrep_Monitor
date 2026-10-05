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
#' NOTE: expects `landcover_<corineYear>_landscape_<scaleLabel>.tif` to
#' already exist in each species' own resolved processed folder (via
#' `loadCovariates()`) -- CORINE land cover processing is out of scope for
#' this module and must be supplied separately.
#'
#' Each species' own landscape covariate stack is read from its own
#' resolved resolution's folder (`resolutionConfig`, falling back to
#' `sharedResolutionM` -- see DECISIONS.md's 2026-09-28 entry); species
#' sharing a resolution also share one cached covariate stack per year.
#' Unlike habitat scale, the presence/absence CONSTRUCTION here never
#' touches a resolution-specific grid (coordinates come straight from the
#' Probeflaechen shapefile's route centroids), so only the covariate-stack
#' lookup needs to vary per species -- no reference-grid regrouping needed.
#'
#' @param ddaTerritoriesXlsxPath Character. Path to the DDA territories xlsx.
#'   Only read if at least one species uses the default DDA data source.
#' @param ddaVisitsXlsxPath Character. Path to the DDA visited-routes xlsx.
#'   Only read if at least one species uses the default DDA data source.
#' @param probeflaechenShpPath Character. Path to the Probeflaechen shapefile.
#' @param processedRoot Character. `predictors/processed` directory
#'   (without the scale_X leaf -- each species' own leaf, via
#'   `scaleLabel()`, is appended internally).
#' @param outputDir Character. Directory to save per-species-year RDS files in.
#' @param species Character vector of Latin species names to process.
#' @param landscapeYears Named list, species -> integer vector of years to
#'   process for that species (e.g. from `resolveYearsPerSpecies()`,
#'   `sharedSpeciesConfig.R`) -- every species' own years can differ (e.g.
#'   Buteo buteo/Sturnus vulgaris's real MhB point-count data is negligible
#'   before ~2020, while DDA-territories species genuinely span the full
#'   shared default range). The raw DDA/MhB filters below operate on the
#'   UNION of every species' years; each species' own loop further down
#'   only visits its own years.
#' @param resolutionConfig Named list, species -> scale -> resolution (m), or
#'   NULL (default). See `dataPrep_Monitor`'s parameter of the same name.
#' @param sharedResolutionM Numeric. Shared default landscape resolution (m),
#'   used for any species absent from `resolutionConfig`.
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
#' @param brutzeitcodeFilter Named character vector/list, or NULL (default).
#'   Per-species ATLAS_CODE prefix filter, ONLY meaningful for species in
#'   `perSpeciesDataSource` marked `"MhB point counts"` (ignored for
#'   DDA-routed species, which have no ATLAS_CODE at all) -- e.g. sourced
#'   from `speciesConfig_general.csv`'s `brutzeitcode_filter` column
#'   (landscape rows) via `loadSpeciesGeneralConfig()`. Narrows which MhB
#'   detections count as a PRESENCE for that species (same semantics as
#'   `occurrencePrepGerHabitat()`'s filter of the same name); does NOT
#'   affect which routes count as "surveyed" for the absence side -- that's
#'   still any qualifying detection of any MhB-routed species, unfiltered.
#' @param perSpeciesDataSource Named character vector/list, or NULL (default,
#'   every species uses DDA territories). species -> `"DDA territories"`/
#'   `"MhB point counts"` -- e.g. sourced from `speciesConfig_general.csv`'s
#'   `data_source` column (landscape rows) via `loadSpeciesGeneralConfig()`.
#' @param germanNames Named character vector, `species` -> German name --
#'   required (both the raw DDA territories data and the raw MhB CSV's
#'   `SPECIES_NAME_GERMAN` column identify species in German only, no Latin
#'   text column DDA can be filtered on directly). Sourced from
#'   `speciesCanonical.csv` (repo root) via `canonicalGermanNames()` in
#'   `sharedSpeciesCanonical.R` -- the single canonical name lookup, see
#'   that file's docstring for why this replaced the old, separately
#'   maintained `speciesLookup()`.
#' @param cachePath Character, or NULL (default). Directory for
#'   `reproducible::Cache()`'s per-species-year cache (see
#'   `buildLandscapeSpeciesYear()`) -- e.g. `cachePath(sim)` when run via
#'   dataPrep_Monitor, a stable location shared across runs (NOT the
#'   per-run timestamped output folder), so a config change for one
#'   species only triggers recompute for that species. NULL falls back to
#'   a temp directory, for standalone/test calls where persistence across
#'   sessions doesn't matter.
#' ASCII-fold German umlauts/eszett so German-name matching survives even if
#' speciesCanonical.csv's german_name column (routinely hand-edited) ends up
#' with a mis-encoded or lost special character -- confirmed 2026-10-01 as a
#' real, recurring failure mode (Buteo buteo's "ae" and Lanius collurio's
#' "oe" each broke this way, independently, on separate occasions). Applied
#' to BOTH sides of every German-name comparison in this file, so it doesn't
#' matter whether the mismatch originates from speciesCanonical.csv or from
#' the raw DDA/MhB data (which always use real UTF-8 umlauts and are not
#' ours to change). \u escapes are used instead of literal accented
#' characters so this function's own source stays pure ASCII and can never
#' itself be corrupted by a future save in the wrong encoding.
foldGermanUmlauts <- function(x) {
  x <- gsub("ä", "ae", x); x <- gsub("Ä", "Ae", x)
  x <- gsub("ö", "oe", x); x <- gsub("Ö", "Oe", x)
  x <- gsub("ü", "ue", x); x <- gsub("Ü", "Ue", x)
  x <- gsub("ß", "ss", x)
  x
}

#' @return Invisibly, a named character vector of output file paths.
occurrencePrepGerLandscape <- function(ddaTerritoriesXlsxPath, ddaVisitsXlsxPath,
                                        probeflaechenShpPath, processedRoot,
                                        outputDir, species, landscapeYears,
                                        resolutionConfig = NULL, sharedResolutionM,
                                        localeCtype = "de_DE.UTF-8",
                                        thinDist = 2000, perSpeciesThinDist = NULL,
                                        useThinning = TRUE, mhbObsPath = NULL,
                                        perSpeciesDataSource = NULL, germanNames,
                                        brutzeitcodeFilter = NULL, cachePath = NULL) {

  if (is.null(cachePath)) cachePath <- file.path(tempdir(), "birdMonitor_cache")

  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  Sys.setlocale("LC_CTYPE", localeCtype)

  # Union of every species' own years -- the raw DDA/MhB filters below need
  # every year ANY species actually uses; each species' own loop further
  # down then only visits its own years.
  unionYears <- sort(unique(unlist(landscapeYears[species], use.names = FALSE)))

  missingGerman <- setdiff(species, names(germanNames))
  if (length(missingGerman) > 0) {
    stop("occurrencePrepGerLandscape(): no germanNames entry for: ",
         paste(missingGerman, collapse = ", "), " -- every species passed in `species` ",
         "must have a German name (see speciesCanonical.csv).")
  }
  # Reproduces speciesLookup()'s old data.frame shape (german/latin columns)
  # so the rest of this function -- written against that shape -- needs no
  # further changes beyond this one substitution.
  lookup <- data.frame(german = foldGermanUmlauts(unname(germanNames[species])), latin = species,
                        stringsAsFactors = FALSE)

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
      dplyr::mutate(species = foldGermanUmlauts(enc2utf8(`Art deutsch`)),
                     ROUTENCODE = tolower(ROUTENCODE),
                     Jahr = as.character(format(Countdate, "%Y")))

    visits <- visits |>
      dplyr::mutate(ROUTENCODE = tolower(ROUTENCODE),
                     Jahr = as.character(Jahr)) |>
      tidyr::unite("route_yr", ROUTENCODE, Jahr, remove = FALSE)

    visits <- visits |> dplyr::filter(Jahr %in% as.character(unionYears))
    territories <- territories |> dplyr::filter(Jahr %in% as.character(unionYears))

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
    # fileEncoding is explicit (not left to the process's ambient locale) --
    # see loadSpeciesCanonical() in sharedSpeciesCanonical.R for why: this is
    # the exact read whose SPECIES_NAME_GERMAN column gets joined against
    # german_name from speciesCanonical.csv (a DIFFERENT encoding, Latin-1),
    # and a locale-dependent mismatch here is what silently zeroed out every
    # Buteo buteo presence at landscape scale.
    mhbRaw <- read.csv(mhbObsPath, header = TRUE, fileEncoding = "UTF-8")
    mhbRaw <- mhbRaw |>
      dplyr::mutate(date = as.Date(DATE_TIME, "%Y-%m-%d"),
                     year = as.integer(format(date, "%Y")),
                     month = format(date, "%m"),
                     SPECIES_NAME_GERMAN = foldGermanUmlauts(SPECIES_NAME_GERMAN))

    focalGermanMhB <- lookup$german[lookup$latin %in% mhbRoutedSpecies]

    # Same breeding-season/evidence-quality filter occurrencePrepGerHabitat()
    # applies by default (excludes the weakest "A"-with-no-number tier),
    # kept consistent since this reuses the exact same raw dataset.
    dfMhB <- mhbRaw |>
      dplyr::filter(year %in% unionYears,
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
      dplyr::rename(latin_name = latin)

    if (!is.null(brutzeitcodeFilter)) {
      speciesPrefix <- rep(NA_character_, nrow(presenceRouteYr))
      known <- presenceRouteYr$latin_name %in% names(brutzeitcodeFilter)
      speciesPrefix[known] <- unlist(brutzeitcodeFilter)[presenceRouteYr$latin_name[known]]
      keepRow <- is.na(speciesPrefix) | startsWith(presenceRouteYr$ATLAS_CODE, speciesPrefix)
      if (any(known)) {
        message("  ATLAS_CODE filter applied to MhB-routed presences: ",
                nrow(presenceRouteYr), " -> ", sum(keepRow))
      }
      presenceRouteYr <- presenceRouteYr[keepRow, ]
    }

    presenceRouteYr <- presenceRouteYr |>
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

  message("\nProcessing ", length(species), " species x ", length(unionYears),
          " years (union; each species uses its own subset)...")

  # Keyed by resolved resolution (m), then year -- species sharing a
  # resolution share one cached covariate stack per year, built lazily
  # (per resolution+year, the first time any species actually needs it) so
  # species sharing a resolution but NOT the same year list still only pay
  # for the years they individually use. Also lets each stack's content
  # flow into Cache()'s digest below via `covStack`, instead of only a
  # directory path (which wouldn't change if the raster CONTENT changes
  # but the path doesn't).
  covStacksByResolution <- list()

  outFiles <- list()

  for (spLatin in species) {
    spClean <- gsub(" ", "_", spLatin)

    message("\n  -- ", spLatin, " --------------------")

    resM <- if (!is.null(resolutionConfig) && !is.null(resolutionConfig[[spLatin]]$landscape) &&
                !is.na(resolutionConfig[[spLatin]]$landscape)) {
      resolutionConfig[[spLatin]]$landscape
    } else {
      sharedResolutionM
    }
    landscapeOutputDir <- file.path(processedRoot, scaleLabel(resM))

    resKey <- as.character(resM)
    if (is.null(covStacksByResolution[[resKey]])) {
      covStacksByResolution[[resKey]] <- list()
    }

    spThinDist <- if (!is.null(perSpeciesThinDist) && spLatin %in% names(perSpeciesThinDist)) {
      perSpeciesThinDist[[spLatin]]
    } else {
      thinDist
    }

    for (yr in landscapeYears[[spLatin]]) {

      outFile <- file.path(outputDir, paste0(spClean, "_landscape_", yr, ".rds"))

      yrKey <- as.character(yr)
      if (is.null(covStacksByResolution[[resKey]][[yrKey]])) {
        covStacksByResolution[[resKey]][[yrKey]] <- loadCovariates(yr, landscapeOutputDir, NULL)
      }
      covStack <- covStacksByResolution[[resKey]][[yrKey]]
      if (is.null(covStack)) {
        warning("  Covariates unavailable for ", yr, " -- skipping")
        next
      }

      # spYr is this species' own subset of the pooled presence/absence
      # table -- computed here (cheap) rather than inside the cached
      # helper, so Cache()'s digest only reflects THIS species' actual
      # data (which already bakes in any brutzeitcodeFilter/
      # perSpeciesDataSource effect from upstream), not the whole
      # multi-species occSpatial table. That's what gives per-species
      # cache selectivity: changing Buteo's config changes Buteo's own
      # spYr content, leaving every other species' digest untouched.
      spYr <- occSpatial |>
        dplyr::filter(latin_name == spLatin, Jahr == as.character(yr))

      result <- reproducible::Cache(
        buildLandscapeSpeciesYear, spLatin = spLatin, yr = yr, spYr = spYr,
        covStack = covStack, useThinning = useThinning, thinDist = spThinDist,
        cachePath = cachePath,
        userTags = c("occurrencePrepGerLandscape", spClean, as.character(yr)))

      if (isCachedNull(result)) next

      saveRDS(result, outFile)
      message("  Saved -> ", outFile)
      outFiles[[paste(spLatin, yr)]] <- outFile
    }
  }

  message("\nDone. Output files in: ", outputDir)
  invisible(unlist(outFiles))
}

#' Build one species+year's model-ready landscape occurrence table
#'
#' The actual per-unit computation `occurrencePrepGerLandscape()` wraps in
#' `reproducible::Cache()` -- pulled out into its own function so Cache()'s
#' digest covers exactly `spYr`/`covStack`/`useThinning`/`thinDist` (what
#' actually determines the result) rather than the whole enclosing
#' function's environment. Returns NULL (nothing to cache as a "result",
#' nothing written) for any of the normal "not enough data" skip
#' conditions -- `occurrencePrepGerLandscape()` treats a NULL return the
#' same as its old `next` used to.
#'
#' @param spLatin Character. Species Latin name (for messages only).
#' @param yr Integer. Year (for messages only).
#' @param spYr data.frame. This species+year's own presence/absence rows
#'   (already filtered from the pooled table), with `x`/`y` columns.
#' @param covStack SpatRaster. This year's landscape covariate stack.
#' @param useThinning Logical. Should occurrence points be spatially thinned?
#' @param thinDist Numeric. This species' resolved thinning distance (m).
#' @return A data.frame ready to save, or NULL if there isn't enough data.
buildLandscapeSpeciesYear <- function(spLatin, yr, spYr, covStack, useThinning, thinDist) {
  nPres <- sum(spYr$occurrence == 1)
  nAbs <- sum(spYr$occurrence == 0)

  if (nrow(spYr) == 0) {
    message("  No records for ", spLatin, " ", yr, " -- skipping")
    return(NULL)
  }
  if (nPres < 10) {
    warning("  Too few presences (", nPres, ") for ", spLatin, " in ", yr, " -- skipping")
    return(NULL)
  }

  message("  ", spLatin, " ", yr, ": ", nPres, " pres / ", nAbs, " abs")

  coordsMat <- as.matrix(spYr[, c("x", "y")])
  envVals <- terra::extract(covStack, coordsMat)

  spYrEnv <- cbind(spYr, envVals)

  nBefore <- nrow(spYrEnv)
  spYrEnv <- spYrEnv[stats::complete.cases(spYrEnv[, names(covStack)]), ]
  nAfter <- nrow(spYrEnv)

  if (nBefore > nAfter) {
    message("  Removed ", nBefore - nAfter, " rows with NA covariates")
    reportDroppedRows(nBefore, nAfter, envVals, coordsMat, paste(spLatin, yr, "(landscape)"))
  }

  if (sum(spYrEnv$occurrence == 1) < 10) {
    warning("  Too few presences after NA removal for ", spLatin, " ", yr, " -- skipping")
    return(NULL)
  }

  if (useThinning) {
    message("  Thinning at ", thinDist / 1000, "km...")
    spSf <- sf::st_as_sf(spYrEnv, coords = c("x", "y"), crs = 3035)

    spThinned <- thin(spSf, thinDist = thinDist, runs = 5)
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

  # Confirmed 2026-09-30: unlike buildHabitatSpeciesYear() (where thinning
  # happens BEFORE covariate extraction, so its own <10 NA-removal check
  # already covers post-thinning counts too), thinning here is the LAST
  # step, with nothing checking its own effect. A thinning distance
  # aggressive enough to remove most presences would otherwise silently
  # return a technically-valid data.frame with too little real signal --
  # no warning, no skip -- and only surface later as a much harder-to-
  # diagnose failure downstream (a degenerate spatial-CV fold, an unstable
  # BRT fit). Same 10-presence floor as the two earlier checks in this
  # function, for consistency.
  if (sum(spThinnedDf$occurrence == 1) < 10) {
    warning("  Too few presences after thinning (", sum(spThinnedDf$occurrence == 1),
            ") for ", spLatin, " ", yr, " -- skipping")
    return(NULL)
  }

  spThinnedDf
}
