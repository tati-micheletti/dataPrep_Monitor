#' Prepare all occurrence data for the three bird SDM scales
#'
#' Runs, in order: `occurrencePrepEurope()` (EBBA2, European climate
#' SDM), `occurrencePrepGerHabitat()` (MhB point counts, German 200m
#' habitat SDM), and `occurrencePrepGerLandscape()` (DDA territories,
#' German 1km landscape SDM).
#'
#' @param ebba2CSVPath Character. Path to the EBBA2 occurrence CSV.
#' @param ebba2ShpPath Character. Path to the EBBA2 grid shapefile.
#' @param bioclimFile Character. Path to the training-year bioclim raster.
#' @param mhbObsPath Character. Path to the raw MhB point count CSV.
#' @param ddaTerritoriesXlsxPath Character. Path to the DDA territories xlsx.
#' @param ddaVisitsXlsxPath Character. Path to the DDA visited-routes xlsx.
#' @param probeflaechenShpPath Character. Path to the Probeflaechen shapefile.
#' @param processedRoot Character. `predictors/processed` directory (without
#'   the scale_X leaf -- each species' own resolved leaf is appended
#'   internally by `occurrencePrepGerHabitat()`/`GerLandscape()`).
#' @param resolutionConfig Named list, species -> scale -> resolution (m), or
#'   NULL (default). See `dataPrep_Monitor`'s parameter of the same name.
#' @param sharedHabitatResolutionM,sharedLandscapeResolutionM Numeric. Shared
#'   default resolutions (m), used for any species absent from
#'   `resolutionConfig`.
#' @param occurrenceOutputDir Character. Root directory for occurrence outputs;
#'   `ornitho/` (EBBA2/European), `MhB/`, and `territories/` (DDA) subfolders
#'   are created under it.
#' @param species Character vector of Latin species names to process.
#' @param habitatYears Integer vector of years for the habitat SDM.
#' @param landscapeYears Integer vector of years for the landscape SDM.
#' @param localeCtype Character. Locale for German special characters.
#' @param useThinning Logical. Should occurrence points be spatially thinned
#'   (following Wiedenroth et al.) at all three scales? Does NOT restore
#'   abundance data when FALSE -- occurrence is already binarized to
#'   presence/absence upstream of thinning in each function.
#' @param thinDistEuropeM Numeric. Spatial thinning distance (m) at the
#'   European climate scale. Default 100000 (2x the 50km climate resolution).
#' @param thinDistHabitatM Numeric. Spatial thinning distance (m) at the
#'   German habitat scale. Default 400 (2x the 200m habitat resolution).
#' @param thinDistLandscapeM Numeric. Spatial thinning distance (m) at the
#'   German landscape scale. Default 2000 (2x the 1km landscape resolution).
#' @param perSpeciesThinDist Named list, or NULL (default). species -> scale ->
#'   numeric, per-species thinning distance overrides -- e.g. sourced from
#'   `speciesConfig_general.csv`'s `thinning_dist_m` column via
#'   `loadSpeciesGeneralConfig()`. A species/scale without an entry uses that
#'   scale's `thinDist*M` default above.
#' @param brutzeitcodeFilter Named list, or NULL (default). species -> scale ->
#'   ATLAS_CODE prefix, e.g. sourced from `speciesConfig_general.csv`'s
#'   `brutzeitcode_filter` column via `loadSpeciesGeneralConfig()` (same
#'   nesting shape as `perSpeciesThinDist`). Only "habitat" and "landscape"
#'   scales are meaningful (landscape only for MhB-routed species -- see
#'   `occurrencePrepGerLandscape()`'s docstring); a species+scale with no
#'   entry gets no filter at that scale.
#' @param perSpeciesDataSource Named character vector/list, or NULL (default,
#'   every species uses DDA territories at landscape scale). species ->
#'   `"DDA territories"`/`"MhB point counts"` -- see
#'   `occurrencePrepGerLandscape()`'s docstring. A species using `"MhB point
#'   counts"` here is routed through `mhbObsPath` at landscape scale too.
#' @param germanNames Named character vector, `species` -> German name --
#'   required (landscape scale only; habitat scale filters MhB directly by
#'   scientific name and needs no German name at all). Sourced from
#'   `speciesCanonical.csv` via `canonicalGermanNames()` in
#'   `sharedSpeciesCanonical.R` -- see `occurrencePrepGerLandscape()`'s
#'   docstring for the full rationale.
#' @param cachePath Character, or NULL (default). Directory for
#'   `reproducible::Cache()`'s per-species(-year) cache in each of the 3
#'   occurrence-prep functions -- e.g. `cachePath(sim)`, a stable location
#'   shared across runs (NOT the per-run timestamped output folder).
#' @return Invisibly, a list with `europe`, `gerHabitat`, and
#'   `gerLandscape` output file path vectors.
prepareOccurrenceData <- function(ebba2CSVPath, ebba2ShpPath, bioclimFile,
                                   mhbObsPath, ddaTerritoriesXlsxPath,
                                   ddaVisitsXlsxPath, probeflaechenShpPath,
                                   processedRoot, resolutionConfig = NULL,
                                   sharedHabitatResolutionM, sharedLandscapeResolutionM,
                                   occurrenceOutputDir, species, habitatYears,
                                   landscapeYears, localeCtype = "de_DE.UTF-8",
                                   useThinning = TRUE, thinDistEuropeM = 100000,
                                   thinDistHabitatM = 400, thinDistLandscapeM = 2000,
                                   perSpeciesThinDist = NULL, brutzeitcodeFilter = NULL,
                                   perSpeciesDataSource = NULL, germanNames,
                                   cachePath = NULL) {

  extractScale <- function(nested, scale) {
    if (is.null(nested)) return(NULL)
    out <- lapply(nested, function(sp) sp[[scale]])
    out[!sapply(out, is.null)]
  }

  europeFiles <- occurrencePrepEurope(
    ebba2CSVPath = ebba2CSVPath,
    ebba2ShpPath = ebba2ShpPath,
    bioclimFile = bioclimFile,
    outputDir = file.path(occurrenceOutputDir, "ornitho"),
    species = species,
    useThinning = useThinning,
    thinDist = thinDistEuropeM,
    perSpeciesThinDist = extractScale(perSpeciesThinDist, "climate"),
    cachePath = cachePath)

  gerHabitatFiles <- occurrencePrepGerHabitat(
    mhbObsPath = mhbObsPath,
    probeflaechenShpPath = probeflaechenShpPath,
    processedRoot = processedRoot,
    outputDir = file.path(occurrenceOutputDir, "MhB"),
    species = species,
    habitatYears = habitatYears,
    resolutionConfig = resolutionConfig,
    sharedResolutionM = sharedHabitatResolutionM,
    localeCtype = localeCtype,
    useThinning = useThinning,
    thinDist = thinDistHabitatM,
    perSpeciesThinDist = extractScale(perSpeciesThinDist, "habitat"),
    brutzeitcodeFilter = extractScale(brutzeitcodeFilter, "habitat"),
    cachePath = cachePath)

  gerLandscapeFiles <- occurrencePrepGerLandscape(
    ddaTerritoriesXlsxPath = ddaTerritoriesXlsxPath,
    ddaVisitsXlsxPath = ddaVisitsXlsxPath,
    probeflaechenShpPath = probeflaechenShpPath,
    processedRoot = processedRoot,
    outputDir = file.path(occurrenceOutputDir, "territories"),
    species = species,
    landscapeYears = landscapeYears,
    resolutionConfig = resolutionConfig,
    sharedResolutionM = sharedLandscapeResolutionM,
    localeCtype = localeCtype,
    useThinning = useThinning,
    thinDist = thinDistLandscapeM,
    perSpeciesThinDist = extractScale(perSpeciesThinDist, "landscape"),
    mhbObsPath = mhbObsPath,
    perSpeciesDataSource = perSpeciesDataSource,
    germanNames = germanNames,
    brutzeitcodeFilter = extractScale(brutzeitcodeFilter, "landscape"),
    cachePath = cachePath)

  invisible(list(europe = europeFiles,
                  gerHabitat = gerHabitatFiles,
                  gerLandscape = gerLandscapeFiles))
}
