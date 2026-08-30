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
#' @param habitatOutputDir Character. Directory with habitat-scale covariates.
#' @param landscapeOutputDir Character. Directory with landscape-scale covariates.
#' @param occurrenceOutputDir Character. Root directory for occurrence outputs;
#'   `europe/`, `habitat/`, and `landscape/` subfolders are created under it.
#' @param species Character vector of Latin species names to process.
#' @param habitatYears Integer vector of years for the habitat SDM.
#' @param landscapeYears Integer vector of years for the landscape SDM.
#' @param localeCtype Character. Locale for German special characters.
#' @param useThinning Logical. Should occurrence points be spatially thinned
#'   (following Wiedenroth et al.) at all three scales? Does NOT restore
#'   abundance data when FALSE -- occurrence is already binarized to
#'   presence/absence upstream of thinning in each function.
#' @return Invisibly, a list with `europe`, `gerHabitat`, and
#'   `gerLandscape` output file path vectors.
prepareOccurrenceData <- function(ebba2CSVPath, ebba2ShpPath, bioclimFile,
                                   mhbObsPath, ddaTerritoriesXlsxPath,
                                   ddaVisitsXlsxPath, probeflaechenShpPath,
                                   habitatOutputDir, landscapeOutputDir,
                                   occurrenceOutputDir, species, habitatYears,
                                   landscapeYears, localeCtype = "de_DE.UTF-8",
                                   useThinning = TRUE) {

  europeFiles <- occurrencePrepEurope(
    ebba2CSVPath = ebba2CSVPath,
    ebba2ShpPath = ebba2ShpPath,
    bioclimFile = bioclimFile,
    outputDir = file.path(occurrenceOutputDir, "europe"),
    species = species,
    useThinning = useThinning)

  gerHabitatFiles <- occurrencePrepGerHabitat(
    mhbObsPath = mhbObsPath,
    probeflaechenShpPath = probeflaechenShpPath,
    habitatOutputDir = habitatOutputDir,
    outputDir = file.path(occurrenceOutputDir, "habitat"),
    species = species,
    habitatYears = habitatYears,
    localeCtype = localeCtype,
    useThinning = useThinning)

  gerLandscapeFiles <- occurrencePrepGerLandscape(
    ddaTerritoriesXlsxPath = ddaTerritoriesXlsxPath,
    ddaVisitsXlsxPath = ddaVisitsXlsxPath,
    probeflaechenShpPath = probeflaechenShpPath,
    landscapeOutputDir = landscapeOutputDir,
    habitatOutputDir = habitatOutputDir,
    outputDir = file.path(occurrenceOutputDir, "landscape"),
    species = species,
    landscapeYears = landscapeYears,
    localeCtype = localeCtype,
    useThinning = useThinning)

  invisible(list(europe = europeFiles,
                  gerHabitat = gerHabitatFiles,
                  gerLandscape = gerLandscapeFiles))
}
