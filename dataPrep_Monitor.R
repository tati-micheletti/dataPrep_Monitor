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
    defineParameter("habitatYears", "numeric", 2022:2025, NA, NA,
                    "Years to prepare German habitat-scale (200m) occurrence data for."),
    defineParameter("landscapeYears", "numeric", 2005:2025, NA, NA,
                    "Years to prepare German landscape-scale (1km) occurrence data for."),

    ## Species / locale -------------------------------------------------------------
    defineParameter("species", "character",
                    c("Vanellus vanellus", "Milvus milvus", "Lanius collurio",
                      "Lullula arborea", "Alauda arvensis", "Saxicola rubetra",
                      "Emberiza calandra", "Emberiza citrinella", "Buteo buteo",
                      "Sturnus vulgaris", "Perdix perdix"), NA, NA,
                    "Latin names of focal species to prepare occurrence data for."),
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
    defineParameter("rerunOccurrenceData", "logical", FALSE, NA, NA,
                    "Should prepareOccurrenceData be re-run even if sim$occurrenceData exists?"),
    defineParameter("useSpatialThinning", "logical", TRUE, NA, NA,
                    "Should occurrence points be spatially thinned (thin.R, following",
                    "Wiedenroth et al.) before saving? Does NOT restore abundance data",
                    "when FALSE -- occurrence is already binarized to presence/absence",
                    "upstream of thinning in all three occurrencePrep* functions.")
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
      sim <- scheduleEvent(sim, time(sim), "dataPrep_Monitor", "prepareClimateData")
      sim <- scheduleEvent(sim, time(sim), "dataPrep_Monitor", "prepareDEM")
      sim <- scheduleEvent(sim, time(sim), "dataPrep_Monitor", "prepareLanduse")
      sim <- scheduleEvent(sim, time(sim), "dataPrep_Monitor", "prepareLandcover")
      sim <- scheduleEvent(sim, time(sim), "dataPrep_Monitor", "prepareOccurrenceData")
    },

    prepareClimateData = {
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
      # ! ----- EDIT BELOW ----- ! #
      if (is.null(sim$demPaths) || P(sim)$rerunDEM) {
        sim$demPaths <- prepareDEM(
          demRawDir = file.path(inputPath(sim), "predictors", "raw", "dem"),
          processedDir = file.path(inputPath(sim), "predictors", "processed", "dem"),
          habitatOutputDir = file.path(inputPath(sim), "predictors", "processed",
                                        scaleLabel(P(sim)$habitatResolutionM)),
          landscapeOutputDir = file.path(inputPath(sim), "predictors", "processed",
                                          scaleLabel(P(sim)$landscapeResolutionM)),
          bboxVec = P(sim)$europeBbox,
          targetCRS = P(sim)$targetCRS,
          habitatResolutionM = P(sim)$habitatResolutionM,
          landscapeResolutionM = P(sim)$landscapeResolutionM,
          pythonScriptPath = file.path(modulePath(sim), currentModule(sim), "python", "download_dem.py"),
          requirementsPath = file.path(modulePath(sim), currentModule(sim), "python", "requirements.txt"),
          force = P(sim)$rerunDEM)
      }
      # ! ----- STOP EDITING ----- ! #
    },

    prepareLanduse = {
      # ! ----- EDIT BELOW ----- ! #
      if (is.null(sim$landusePaths) || P(sim)$rerunLanduse) {
        sim$landusePaths <- prepareLanduse(
          landuseRawDir = file.path(inputPath(sim), "predictors", "raw", "landuse"),
          habitatOutputDir = file.path(inputPath(sim), "predictors", "processed",
                                        scaleLabel(P(sim)$habitatResolutionM)),
          landscapeOutputDir = file.path(inputPath(sim), "predictors", "processed",
                                          scaleLabel(P(sim)$landscapeResolutionM)),
          landuseYears = P(sim)$landuseYears,
          targetCRS = P(sim)$targetCRS,
          habitatResolutionM = P(sim)$habitatResolutionM,
          landscapeResolutionM = P(sim)$landscapeResolutionM,
          pythonScriptPath = file.path(modulePath(sim), currentModule(sim), "python", "download_landuse.py"),
          requirementsPath = file.path(modulePath(sim), currentModule(sim), "python", "requirements.txt"),
          force = P(sim)$rerunLanduse)
      }
      # ! ----- STOP EDITING ----- ! #
    },

    prepareLandcover = {
      # ! ----- EDIT BELOW ----- ! #
      if (is.null(sim$landcoverPaths) || P(sim)$rerunLandcover) {
        sim$landcoverPaths <- prepareLandcover(
          landcoverRawDir = file.path(inputPath(sim), "predictors", "raw", "landcover"),
          habitatOutputDir = file.path(inputPath(sim), "predictors", "processed",
                                        scaleLabel(P(sim)$habitatResolutionM)),
          landscapeOutputDir = file.path(inputPath(sim), "predictors", "processed",
                                          scaleLabel(P(sim)$landscapeResolutionM)),
          bboxVec = P(sim)$europeBbox,
          tokenJSONPath = resolvePath(outputPath(sim), P(sim)$clmsTokenJSONPath),
          targetCRS = P(sim)$targetCRS,
          habitatResolutionM = P(sim)$habitatResolutionM,
          landscapeResolutionM = P(sim)$landscapeResolutionM,
          pythonScriptPath = file.path(modulePath(sim), currentModule(sim), "python", "download_landcover.py"),
          requirementsPath = file.path(modulePath(sim), currentModule(sim), "python", "requirements.txt"),
          force = P(sim)$rerunLandcover)
      }
      # ! ----- STOP EDITING ----- ! #
    },

    prepareOccurrenceData = {
      # ! ----- EDIT BELOW ----- ! #
      if (is.null(sim$occurrenceData) || P(sim)$rerunOccurrenceData) {
        windowStart <- P(sim)$ebba2TrainingYear - (P(sim)$climateWindowLength - 1)
        bioclimTrainingFile <- file.path(inputPath(sim), "predictors", "processed",
                                          scaleLabel(P(sim)$climateResolutionM),
                                          paste0("bioclim_", windowStart, "-",
                                                 P(sim)$ebba2TrainingYear, ".tif"))

        sim$occurrenceData <- prepareOccurrenceData(
          ebba2CSVPath = file.path(inputPath(sim), P(sim)$ebba2CSVSubpath),
          ebba2ShpPath = file.path(inputPath(sim), P(sim)$ebba2ShpSubpath),
          bioclimFile = bioclimTrainingFile,
          mhbObsPath = file.path(inputPath(sim), P(sim)$mhbObsSubpath),
          ddaTerritoriesXlsxPath = file.path(inputPath(sim), P(sim)$ddaTerritoriesXlsxSubpath),
          ddaVisitsXlsxPath = file.path(inputPath(sim), P(sim)$ddaVisitsXlsxSubpath),
          probeflaechenShpPath = file.path(inputPath(sim), P(sim)$probeflaechenShpSubpath),
          habitatOutputDir = file.path(inputPath(sim), "predictors", "processed",
                                        scaleLabel(P(sim)$habitatResolutionM)),
          landscapeOutputDir = file.path(inputPath(sim), "predictors", "processed",
                                          scaleLabel(P(sim)$landscapeResolutionM)),
          occurrenceOutputDir = file.path(inputPath(sim), "response", "processed"),
          species = P(sim)$species,
          habitatYears = P(sim)$habitatYears,
          landscapeYears = P(sim)$landscapeYears,
          localeCtype = P(sim)$localeCtype,
          useThinning = P(sim)$useSpatialThinning)
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
