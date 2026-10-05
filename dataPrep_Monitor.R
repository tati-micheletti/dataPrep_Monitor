defineModule(sim, list(
  name = "dataPrep_Monitor",
  description = paste("Downloads and prepares all input data for the bird monitor pipeline:",
                       "climate (CHELSA bioclim), DEM (Copernicus GLO-30), land use",
                       "(CTM/HCTM crop type maps), and occurrence data (EBBA2 and German",
                       "MhB/DDA point counts and territories)."),
  keywords = c("bird monitor", "data preparation", "bioclim", "DEM", "land use", "occurrence"),
  authors = structure(list(list(given = "Tati", family = "Micheletti", role = c("aut", "cre"),
                                 email = "tati.micheletti@gmail.com", comment = NULL),
                           list(given = "Lisa", family = "Hildebrand", role = "aut",
                                email = "lisa.hildebrand@ufz.de", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(dataPrep_Monitor = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "dataPrep_Monitor.Rmd"),
  reqdPkgs = list("PredictiveEcology/SpaDES.core@development (>= 3.2.0)",
                   "PredictiveEcology/reproducible@development",
                   "terra", "dismo", "raster", "spatialEco",
                   "sf", "dplyr", "tidyr", "purrr", "readxl", "reticulate"),
  parameters = bindrows(
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),

    ## Spatial / temporal -------------------------------------------------------
    defineParameter("targetCRS", "character", "EPSG:3035", NA, NA,
                    "Output CRS for all prepared rasters (ETRS89-LAEA)."),
    defineParameter("europeBbox", "numeric", c(72, -25, 34, 45), NA, NA,
                    "European bounding box in WGS84 as c(N, W, S, E). Used for both",
                    "the climate window and the DEM download extent."),
    defineParameter("climateResolutionM", "numeric", 50000, NA, NA,
                    "Output resolution (m) for the climate (bioclim) rasters. Also used,",
                    "via scaleLabel(), to name that scale's inputs/outputs subfolders --",
                    "must match inputs_Monitor's/models_Monitor's copies of this value."),
    defineParameter("habitatResolutionM", "numeric", 200, NA, NA,
                    "Output resolution (m) for habitat-scale rasters. Also used, via",
                    "scaleLabel(), to name that scale's inputs/outputs subfolders -- must",
                    "match inputs_Monitor's/models_Monitor's copies of this value."),
    defineParameter("landscapeResolutionM", "numeric", 1000, NA, NA,
                    "Output resolution (m) for landscape-scale rasters. Also used, via",
                    "scaleLabel(), to name that scale's inputs/outputs subfolders -- must",
                    "match inputs_Monitor's/models_Monitor's copies of this value."),
    defineParameter("resolutionConfig", "list", NULL, NA, NA,
                    "NULL (default): every species uses the shared *ResolutionM parameters",
                    "above. Otherwise a named list, species -> scale -> resolution (m),",
                    "produced by extractResolutionConfig() (sharedSpeciesConfig.R, repo",
                    "root) from speciesConfig_general.csv's resolution_m column. Used to",
                    "look up each species' own resolution when building its occurrence-",
                    "extraction covariate stack."),
    defineParameter("distinctClimateResolutions", "numeric", NULL, NA, NA,
                    "Every distinct climate-scale resolution (m) actually needed across",
                    "all species (sort(unique(...)) of resolutionConfig's climate values,",
                    "falling back to climateResolutionM where unset) -- computed once by",
                    "the orchestrating script (runMe.R) so this scale's rasters are",
                    "generated exactly once per distinct resolution, not once per",
                    "species. NULL (default): falls back to climateResolutionM alone."),
    defineParameter("distinctHabitatResolutions", "numeric", NULL, NA, NA,
                    "Same as distinctClimateResolutions, for the habitat scale. NULL",
                    "(default): falls back to habitatResolutionM alone."),
    defineParameter("distinctLandscapeResolutions", "numeric", NULL, NA, NA,
                    "Same as distinctClimateResolutions, for the landscape scale. NULL",
                    "(default): falls back to landscapeResolutionM alone."),

    ## Climate --------------------------------------------------------------------
    defineParameter("climateTargetYears", "numeric", 2005:2025, NA, NA,
                    "Target years to compute rolling-window bioclim climatologies for."),
    defineParameter("climateWindowLength", "numeric", 6, NA, NA,
                    "Number of years in the rolling window (Y-5 to Y) used to compute",
                    "each target year's bioclim climatology."),
    defineParameter("ebba2TrainingYear", "numeric", 2017, NA, NA,
                    "Target year whose bioclim window (e.g. 2012-2017) is used to train",
                    "the European EBBA2 climate SDM."),

    ## Land use / land cover years -------------------------------------------------
    defineParameter("landuseYears", "numeric", 2005:2025, NA, NA,
                    "Years to process land use (crop type) maps for."),
    defineParameter("habitatYears", "list", NULL, NA, NA,
                    "Named list, species -> integer vector of years to prepare German",
                    "habitat-scale (200m) occurrence data for that species. Per-species",
                    "since 2026-10-01 (e.g. Buteo buteo/Sturnus vulgaris's real MhB",
                    "point-count data is negligible before ~2020, while other species",
                    "genuinely span a wider range) -- see resolveYearsPerSpecies()",
                    "(sharedSpeciesConfig.R). No default -- runMe.R must always supply a",
                    "fully-resolved list."),
    defineParameter("landscapeYears", "list", NULL, NA, NA,
                    "Named list, species -> integer vector of years to prepare German",
                    "landscape-scale occurrence data for that species. Same per-species",
                    "rationale/mechanism as habitatYears above. No default -- runMe.R",
                    "must always supply a fully-resolved list."),

    ## Species / locale -------------------------------------------------------------
    defineParameter("species", "character", NA_character_, NA, NA,
                    "Latin names of focal species to prepare occurrence data for -- no",
                    "default (errors if unset); supply sharedSpecies from sharedConfig.R",
                    "(repo root) so this and inputs_Monitor's roster can never silently",
                    "drift apart."),
    defineParameter("localeCtype", "character", "de_DE.UTF-8", NA, NA,
                    "Locale used for correct handling of German special characters."),

    ## Raw external data locations (relative to inputPath(sim)) --------------------
    ## These raw datasets cannot be downloaded programmatically and must be
    ## supplied by the user at these locations before prepareOccurrenceData runs.
    ## Deliberately under inputPath(sim), never outputPath(sim): raw,
    ## irreplaceable survey data must not live inside a tree that's
    ## conceptually disposable pipeline output (a fresh runName or an
    ## outputs/ cleanup must never risk it).
    defineParameter("ebba2CSVSubpath", "character",
                    "response/raw/ornitho/ebba2_data_occurrence_50km.csv", NA, NA,
                    "Path (relative to inputPath(sim)) to the EBBA2 occurrence CSV."),
    defineParameter("ebba2ShpSubpath", "character",
                    "response/raw/ornitho/ebba2_grid50x50_v1.shp", NA, NA,
                    "Path (relative to inputPath(sim)) to the EBBA2 grid shapefile."),
    defineParameter("mhbObsSubpath", "character",
                    "response/raw/MhB/dbird_observations_CBBM.csv", NA, NA,
                    "Path (relative to inputPath(sim)) to the raw MhB point count CSV."),
    defineParameter("probeflaechenShpSubpath", "character",
                    "response/raw/MhB/MhB_Probeflaechen_DE_S2637_epsg25832.shp", NA, NA,
                    "Path (relative to inputPath(sim)) to the Probeflaechen shapefile."),
    defineParameter("ddaTerritoriesXlsxSubpath", "character",
                    "response/raw/territories/BirdStats_Daten2005-2024D_alle.xlsx", NA, NA,
                    "Path (relative to inputPath(sim)) to the DDA territories xlsx."),
    defineParameter("ddaVisitsXlsxSubpath", "character",
                    "response/raw/territories/BirdStats_Visits2005-2024D.xlsx", NA, NA,
                    "Path (relative to inputPath(sim)) to the DDA visited-routes xlsx."),

    ## CORINE Land Cover (CLMS API) ----------------------------------------------------
    defineParameter("clmsTokenJSONPath", "character", "clms_token.json", NA, NA,
                    "Path to your personal CLMS API token JSON file",
                    "(client_id/private_key/user_id/token_uri). Either absolute (recommended --",
                    "e.g. somewhere in your home directory, well outside any git-tracked project,",
                    "since this file holds a private key), or relative to outputPath(sim). You must",
                    "create this file yourself at https://land.copernicus.eu -- see",
                    "python/download_landcover.py for exact setup steps. Never commit this file."),

    ## Rerun control ------------------------------------------------------------------
    defineParameter("rerunClimateData", "logical", FALSE, NA, NA,
                    "Should prepareClimateData be re-run even if sim$bioclimPaths exists?"),
    defineParameter("rerunDEM", "logical", FALSE, NA, NA,
                    "Should prepareDEM be re-run even if sim$demPaths exists?"),
    defineParameter("rerunLanduse", "logical", FALSE, NA, NA,
                    "Should prepareLanduse be re-run even if sim$landusePaths exists?"),
    defineParameter("rerunLandcover", "logical", FALSE, NA, NA,
                    "Should prepareLandcover be re-run even if sim$landcoverPaths exists?"),
    defineParameter("rerunDerivedCovariates", "logical", FALSE, NA, NA,
                    "Should prepareDerivedCovariates (dist_to_woodland, landscape_",
                    "heterogeneity) be re-run even if sim$derivedCovariatePaths exists?"),
    defineParameter("rastersOnly", "logical", FALSE, NA, NA,
                    "If TRUE, build ONLY the predictor rasters (climate, DEM, landuse,",
                    "landcover, derived covariates) and skip prepareOccurrenceData. Used",
                    "to pre-build new-resolution layers on a local machine."),
    defineParameter("rerunOccurrenceData", "logical", FALSE, NA, NA,
                    "Should prepareOccurrenceData be re-run even if sim$occurrenceData exists?"),
    defineParameter("useSpatialThinning", "logical", TRUE, NA, NA,
                    "Should occurrence points be spatially thinned (thin.R, following",
                    "Wiedenroth et al.) before saving? Does NOT restore abundance data",
                    "when FALSE -- occurrence is already binarized to presence/absence",
                    "upstream of thinning in all three occurrencePrep* functions."),
    defineParameter("thinDistEuropeM", "numeric", 100000, NA, NA,
                    "Spatial thinning distance (m) at the European climate scale.",
                    "Default 100000 (2x the 50km climate resolution)."),
    defineParameter("thinDistHabitatM", "numeric", 400, NA, NA,
                    "Spatial thinning distance (m) at the German habitat scale.",
                    "Default 400 (2x the 200m habitat resolution)."),
    defineParameter("thinDistLandscapeM", "numeric", 2000, NA, NA,
                    "Spatial thinning distance (m) at the German landscape scale.",
                    "Default 2000 (2x the 1km landscape resolution)."),
    defineParameter("perSpeciesThinDist", "list", NULL, NA, NA,
                    "NULL (default): every species uses the shared thinDist*M parameters",
                    "above at every scale. Otherwise a named list, species -> scale ->",
                    "numeric, overriding the thinning distance for just that species+scale.",
                    "Sourced from speciesConfig_general.csv's thinning_dist_m column (repo",
                    "root) via loadSpeciesGeneralConfig() in sharedSpeciesConfig.R -- resolved",
                    "once by runMe.R/the orchestrating script and passed in as a plain value,",
                    "same pattern as sharedConfig.R's other shared values."),
    defineParameter("brutzeitcodeFilter", "list", NULL, NA, NA,
                    "NULL (default): no ATLAS_CODE filtering beyond occurrencePrepGerHabitat()'s/",
                    "GerLandscape()'s existing global filter (already excludes the weakest",
                    "\"A\"-with-no-number tier for everyone). Otherwise a named list, species ->",
                    "scale -> ATLAS_CODE prefix (e.g. \"C\" for confirmed-breeding-only), applied",
                    "ON TOP of that global filter for just the named species+scale. Meaningful at",
                    "habitat scale for any species, and at landscape scale ONLY for species routed",
                    "through MhB point counts there (see perSpeciesDataSource below) -- DDA-",
                    "territories-routed species have no ATLAS_CODE at all, so a landscape entry",
                    "for them is simply ignored. Sourced from speciesConfig_general.csv's",
                    "brutzeitcode_filter column via loadSpeciesGeneralConfig() (same species ->",
                    "scale nesting as perSpeciesThinDist)."),
    defineParameter("perSpeciesDataSource", "character", NULL, NA, NA,
                    "NULL (default): every species uses DDA territories at landscape scale.",
                    "Otherwise a named list, species -> \"DDA territories\"/\"MhB point counts\" --",
                    "a species set to \"MhB point counts\" is routed through the raw MhB CSV at",
                    "landscape scale too (route-level presence/absence instead of DDA's",
                    "territory counts) -- see occurrencePrepGerLandscape()'s docstring and",
                    "DECISIONS.md. Sourced from speciesConfig_general.csv's data_source column",
                    "(landscape rows)."),
    defineParameter("germanNames", "character", NULL, NA, NA,
                    "Named character vector, species -> German name -- REQUIRED (landscape",
                    "scale has no other way to identify species in the raw DDA data, which has",
                    "no Latin-name column at all). Sourced from speciesCanonical.csv (repo root)",
                    "via canonicalGermanNames() in sharedSpeciesCanonical.R -- the single",
                    "canonical name lookup; resolved once by runMe.R/the orchestrating script",
                    "and passed in as a plain value, same pattern as sharedConfig.R's other",
                    "shared values. See sharedSpeciesCanonical.R's docstring for why this",
                    "replaced the old, separately maintained speciesLookup().")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
  ),
  outputObjects = bindrows(
    createsOutput("bioclimPaths", "list",
                  "Named list of bioclim raster paths, one per climate target year."),
    createsOutput("demPaths", "list",
                  "Named list of DEM derivative raster paths (elevation/slope/solar",
                  "radiation) per scale."),
    createsOutput("landusePaths", "list",
                  "Named list of land use raster paths (habitat/landscape) per year."),
    createsOutput("landcoverPaths", "list",
                  "Named list of land cover raster paths (habitat/landscape) per CORINE",
                  "snapshot year (2006/2012/2018)."),
    createsOutput("derivedCovariatePaths", "list",
                  "Named list of dist_to_woodland (per CORINE snapshot year) and",
                  "landscape_heterogeneity (per land use year) raster paths, keyed",
                  "\"<year>_<scaleName>_<resM>\" -- computed from landcoverPaths/",
                  "landusePaths' own outputs, so always scheduled after both."),
    createsOutput("occurrenceData", "list",
                  "List with europe/gerHabitat/gerLandscape vectors of per-species(-year)",
                  "occurrence RDS paths.")
  )
))

doEvent.dataPrep_Monitor = function(sim, eventTime, eventType) {
  # Manually-supplied bird survey data (EBBA2/MhB/DDA) lives under
  # inputPath(sim)/response/raw/<type>/ -- see the "Raw external data
  # locations" parameter block above. Everything the pipeline
  # downloads/creates for itself (DEM, landuse, landcover, the CHELSA
  # cache, and every scale's final processed covariates) lives under
  # inputPath(sim)/predictors/{raw,processed}/. Nothing pipeline-input
  # lives under outputPath(sim) -- that's reserved for model fitting/
  # prediction results (models_Monitor).
  switch(
    eventType,
    init = {
      warmUpSfProj()   # sf must touch GDAL/PROJ before terra does (axis-order issue on EVE; see warmUpSfProj.R)
      if (identical(P(sim)$species, NA_character_)) {
        stop("dataPrep_Monitor's species parameter must be supplied explicitly ",
             "(e.g. sharedSpecies from sharedConfig.R) -- no default roster.")
      }
      sim <- scheduleEvent(sim, time(sim), "dataPrep_Monitor", "prepareClimateData")
      sim <- scheduleEvent(sim, time(sim), "dataPrep_Monitor", "prepareDEM")
      sim <- scheduleEvent(sim, time(sim), "dataPrep_Monitor", "prepareLanduse")
      sim <- scheduleEvent(sim, time(sim), "dataPrep_Monitor", "prepareLandcover")
      sim <- scheduleEvent(sim, time(sim), "dataPrep_Monitor", "prepareDerivedCovariates")
      if (!isTRUE(P(sim)$rastersOnly))
        sim <- scheduleEvent(sim, time(sim), "dataPrep_Monitor", "prepareOccurrenceData")
    },

    prepareClimateData = {
      reportAxisState("start of prepareClimateData")
      # ! ----- EDIT BELOW ----- ! #
      if (is.null(sim$bioclimPaths) || P(sim)$rerunClimateData) {
        sim$bioclimPaths <- prepareClimateData(
          climateTargetYears = P(sim)$climateTargetYears,
          climateWindowLength = P(sim)$climateWindowLength,
          europeBboxVec = P(sim)$europeBbox,
          targetCRS = P(sim)$targetCRS,
          climateResolutionM = P(sim)$climateResolutionM,
          chelsaMonthlyDir = file.path(inputPath(sim), "predictors", "raw", "chelsa_monthly", "europe"),
          climateOutputDir = file.path(inputPath(sim), "predictors", "processed",
                                        scaleLabel(P(sim)$climateResolutionM)))
      }
      # ! ----- STOP EDITING ----- ! #
    },

    prepareDEM = {
      reportAxisState("start of prepareDEM")
      # ! ----- EDIT BELOW ----- ! #
      if (is.null(sim$demPaths) || P(sim)$rerunDEM) {
        habitatResolutions <- if (is.null(P(sim)$distinctHabitatResolutions)) P(sim)$habitatResolutionM else P(sim)$distinctHabitatResolutions
        landscapeResolutions <- if (is.null(P(sim)$distinctLandscapeResolutions)) P(sim)$landscapeResolutionM else P(sim)$distinctLandscapeResolutions
        sim$demPaths <- prepareDEM(
          demRawDir = file.path(inputPath(sim), "predictors", "raw", "dem"),
          processedDir = file.path(inputPath(sim), "predictors", "processed", "dem"),
          processedRoot = file.path(inputPath(sim), "predictors", "processed"),
          bboxVec = P(sim)$europeBbox,
          targetCRS = P(sim)$targetCRS,
          habitatResolutions = habitatResolutions,
          landscapeResolutions = landscapeResolutions,
          pythonScriptPath = file.path(modulePath(sim), currentModule(sim), "python", "download_dem.py"),
          requirementsPath = file.path(modulePath(sim), currentModule(sim), "python", "requirements.txt"),
          force = P(sim)$rerunDEM)
      }
      # ! ----- STOP EDITING ----- ! #
    },

    prepareLanduse = {
      reportAxisState("start of prepareLanduse")
      # ! ----- EDIT BELOW ----- ! #
      if (is.null(sim$landusePaths) || P(sim)$rerunLanduse) {
        habitatResolutions <- if (is.null(P(sim)$distinctHabitatResolutions)) P(sim)$habitatResolutionM else P(sim)$distinctHabitatResolutions
        landscapeResolutions <- if (is.null(P(sim)$distinctLandscapeResolutions)) P(sim)$landscapeResolutionM else P(sim)$distinctLandscapeResolutions
        sim$landusePaths <- prepareLanduse(
          landuseRawDir = file.path(inputPath(sim), "predictors", "raw", "landuse"),
          processedRoot = file.path(inputPath(sim), "predictors", "processed"),
          landuseYears = P(sim)$landuseYears,
          targetCRS = P(sim)$targetCRS,
          habitatResolutions = habitatResolutions,
          landscapeResolutions = landscapeResolutions,
          pythonScriptPath = file.path(modulePath(sim), currentModule(sim), "python", "download_landuse.py"),
          requirementsPath = file.path(modulePath(sim), currentModule(sim), "python", "requirements.txt"),
          force = P(sim)$rerunLanduse)
      }
      # ! ----- STOP EDITING ----- ! #
    },

    prepareLandcover = {
      reportAxisState("start of prepareLandcover")
      # ! ----- EDIT BELOW ----- ! #
      if (is.null(sim$landcoverPaths) || P(sim)$rerunLandcover) {
        habitatResolutions <- if (is.null(P(sim)$distinctHabitatResolutions)) P(sim)$habitatResolutionM else P(sim)$distinctHabitatResolutions
        landscapeResolutions <- if (is.null(P(sim)$distinctLandscapeResolutions)) P(sim)$landscapeResolutionM else P(sim)$distinctLandscapeResolutions
        sim$landcoverPaths <- prepareLandcover(
          landcoverRawDir = file.path(inputPath(sim), "predictors", "raw", "landcover"),
          processedRoot = file.path(inputPath(sim), "predictors", "processed"),
          bboxVec = P(sim)$europeBbox,
          tokenJSONPath = resolvePath(outputPath(sim), P(sim)$clmsTokenJSONPath),
          targetCRS = P(sim)$targetCRS,
          habitatResolutions = habitatResolutions,
          landscapeResolutions = landscapeResolutions,
          pythonScriptPath = file.path(modulePath(sim), currentModule(sim), "python", "download_landcover.py"),
          requirementsPath = file.path(modulePath(sim), currentModule(sim), "python", "requirements.txt"),
          force = P(sim)$rerunLandcover)
      }
      # ! ----- STOP EDITING ----- ! #
    },

    prepareDerivedCovariates = {
      reportAxisState("start of prepareDerivedCovariates")
      # ! ----- EDIT BELOW ----- ! #
      if (is.null(sim$derivedCovariatePaths) || P(sim)$rerunDerivedCovariates) {
        habitatResolutions <- if (is.null(P(sim)$distinctHabitatResolutions)) P(sim)$habitatResolutionM else P(sim)$distinctHabitatResolutions
        landscapeResolutions <- if (is.null(P(sim)$distinctLandscapeResolutions)) P(sim)$landscapeResolutionM else P(sim)$distinctLandscapeResolutions
        processedRoot <- file.path(inputPath(sim), "predictors", "processed")

        distPaths <- prepareDistToWoodland(
          landcoverRawDir = file.path(inputPath(sim), "predictors", "raw", "landcover"),
          processedRoot = processedRoot,
          targetCRS = P(sim)$targetCRS,
          habitatResolutions = habitatResolutions,
          landscapeResolutions = landscapeResolutions,
          force = P(sim)$rerunDerivedCovariates)

        heterogeneityPaths <- prepareHeterogeneityIndex(
          processedRoot = processedRoot,
          landuseYears = P(sim)$landuseYears,
          habitatResolutions = habitatResolutions,
          landscapeResolutions = landscapeResolutions,
          force = P(sim)$rerunDerivedCovariates)

        sim$derivedCovariatePaths <- list(distToWoodland = distPaths,
                                           landscapeHeterogeneity = heterogeneityPaths)
      }
      # ! ----- STOP EDITING ----- ! #
    },

    prepareOccurrenceData = {
      reportAxisState("start of prepareOccurrenceData")
      # ! ----- EDIT BELOW ----- ! #
      if (is.null(sim$occurrenceData) || P(sim)$rerunOccurrenceData) {
        windowStart <- P(sim)$ebba2TrainingYear - (P(sim)$climateWindowLength - 1)
        climateLabel <- scaleLabel(P(sim)$climateResolutionM)
        # Resolution appended to the filename (second safety layer beyond
        # the containing scaleLabel()-named folder).
        bioclimTrainingFile <- file.path(inputPath(sim), "predictors", "processed",
                                          climateLabel,
                                          paste0("bioclim_", windowStart, "-",
                                                 P(sim)$ebba2TrainingYear, "_", climateLabel, ".tif"))

        sim$occurrenceData <- prepareOccurrenceData(
          ebba2CSVPath = file.path(inputPath(sim), P(sim)$ebba2CSVSubpath),
          ebba2ShpPath = file.path(inputPath(sim), P(sim)$ebba2ShpSubpath),
          bioclimFile = bioclimTrainingFile,
          mhbObsPath = file.path(inputPath(sim), P(sim)$mhbObsSubpath),
          ddaTerritoriesXlsxPath = file.path(inputPath(sim), P(sim)$ddaTerritoriesXlsxSubpath),
          ddaVisitsXlsxPath = file.path(inputPath(sim), P(sim)$ddaVisitsXlsxSubpath),
          probeflaechenShpPath = file.path(inputPath(sim), P(sim)$probeflaechenShpSubpath),
          processedRoot = file.path(inputPath(sim), "predictors", "processed"),
          resolutionConfig = P(sim)$resolutionConfig,
          sharedHabitatResolutionM = P(sim)$habitatResolutionM,
          sharedLandscapeResolutionM = P(sim)$landscapeResolutionM,
          occurrenceOutputDir = file.path(inputPath(sim), "response", "processed"),
          species = P(sim)$species,
          habitatYears = P(sim)$habitatYears,
          landscapeYears = P(sim)$landscapeYears,
          localeCtype = P(sim)$localeCtype,
          useThinning = P(sim)$useSpatialThinning,
          thinDistEuropeM = P(sim)$thinDistEuropeM,
          thinDistHabitatM = P(sim)$thinDistHabitatM,
          thinDistLandscapeM = P(sim)$thinDistLandscapeM,
          perSpeciesThinDist = P(sim)$perSpeciesThinDist,
          brutzeitcodeFilter = P(sim)$brutzeitcodeFilter,
          perSpeciesDataSource = P(sim)$perSpeciesDataSource,
          germanNames = P(sim)$germanNames,
          cachePath = cachePath(sim))
      }
      # ! ----- STOP EDITING ----- ! #
    },

    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", outputPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
