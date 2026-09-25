#' Prepare EBBA2 occurrence data for the European climate SDM
#'
#' Loads and cleans the EBBA2 occurrence CSV, aligns the EBBA2 grid to
#' the bioclim raster, extracts bioclim covariates, creates absences from
#' surveyed-but-unoccupied cells, spatially thins at 100km, and saves one
#' RDS per species.
#'
#' Follows the methodology of Wiedenroth et al.
#'
#' @param ebba2CSVPath Character. Path to the EBBA2 occurrence CSV.
#' @param ebba2ShpPath Character. Path to the EBBA2 grid shapefile.
#' @param bioclimFile Character. Path to the training-year bioclim raster
#'   (produced by `prepareClimateData()`).
#' @param outputDir Character. Directory to save per-species RDS files in.
#' @param species Character vector of Latin species names to process.
#' @param thinDist Numeric. Default spatial thinning distance in metres, used
#'   for any species without its own entry in `perSpeciesThinDist`.
#' @param perSpeciesThinDist Named numeric vector/list, or NULL (default).
#'   Per-species thinning distance overrides, keyed by species Latin name --
#'   e.g. sourced from `speciesConfig_general.csv`'s `thinning_dist_m`
#'   column (climate rows) via `loadSpeciesGeneralConfig()`.
#' @param useThinning Logical. Should occurrence points be spatially thinned?
#'   Does NOT restore abundance data when FALSE -- EBBA2 is a presence/absence
#'   atlas regardless of thinning.
#' @return Invisibly, a named character vector of output file paths.
occurrencePrepEurope <- function(ebba2CSVPath, ebba2ShpPath, bioclimFile,
                                  outputDir, species, thinDist = 100000,
                                  perSpeciesThinDist = NULL, useThinning = TRUE) {

  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)

  message("Species to process: ", length(species))
  print(species)

  message("Loading and cleaning EBBA2 occurrence data...")
  data <- read.csv(ebba2CSVPath, header = TRUE)

  data <- tidyr::separate(data,
                           col = names(data)[1],
                           into = c("cell50x50", "birdlife_code",
                                    "birdlife_scientific_name", "occurrence"),
                           sep = ";")
  data$occurrence <- as.numeric(data$occurrence)

  message("  Records loaded: ", nrow(data))
  message("  Species found:  ", length(unique(data$birdlife_scientific_name)))

  message("Loading EBBA2 grid shapefile...")
  grid <- terra::vect(ebba2ShpPath)

  message("Loading European bioclim raster...")
  if (!file.exists(bioclimFile)) {
    stop("Bioclim file not found: ", bioclimFile, "\nRun prepareClimateData() first.")
  }
  bioclim <- terra::rast(bioclimFile)
  message("  Layers: ", terra::nlyr(bioclim),
          " | CRS: ", terra::crs(bioclim, describe = TRUE)$code)

  message("Aligning EBBA2 grid to bioclim raster...")
  rTemplate <- terra::rast(grid, resolution = 50000)
  rCells <- terra::rasterize(grid, rTemplate, field = "cell50x50")
  rResampled <- terra::resample(rCells, bioclim, method = "near")
  ebba2Poly <- terra::as.polygons(rResampled, trunc = FALSE, dissolve = FALSE)
  ebba2Pts <- terra::centroids(ebba2Poly, inside = TRUE)

  coords <- terra::geom(ebba2Pts)
  terra::values(ebba2Pts) <- cbind(terra::values(ebba2Pts),
                                    data.frame(x = coords[, 3], y = coords[, 4]))

  ebba2PtsDf <- as.data.frame(terra::values(ebba2Pts))
  names(ebba2PtsDf)[1] <- "cell50x50"
  message("  Aligned grid cells: ", nrow(ebba2PtsDf))

  message("Joining occurrence data to aligned grid...")
  occJoined <- dplyr::inner_join(data, ebba2PtsDf, by = "cell50x50")
  message("  Records after join: ", nrow(occJoined))

  message("Extracting bioclim values at all surveyed cells...")
  uniqueCells <- occJoined[, c("cell50x50", "x", "y")]
  uniqueCells <- uniqueCells[!duplicated(uniqueCells), ]

  allEnv <- cbind(uniqueCells,
                   terra::extract(bioclim, uniqueCells[, c("x", "y")], cells = TRUE))
  allEnvClean <- allEnv[!is.na(allEnv$bio1), ]
  message("  Cells with bioclim data: ", nrow(allEnvClean))

  message("\nProcessing ", length(species), " species...")
  outFiles <- list()

  for (sp in species) {

    spClean <- gsub(" ", "_", sp)
    outFile <- file.path(outputDir, paste0(spClean, "_EBBA2_pa_env.rds"))

    if (isValidRDSFile(outFile)) {
      message("  Cache hit -- skipping: ", sp)
      outFiles[[sp]] <- outFile
      next
    }

    message("\n  Processing: ", sp)

    spOcc <- occJoined |>
      dplyr::filter(birdlife_scientific_name == sp, occurrence == 1)
    message("    Presences: ", nrow(spOcc))

    spEnv <- cbind(spOcc, terra::extract(bioclim, spOcc[, c("x", "y")], cells = TRUE))
    spEnv <- spEnv[!is.na(spEnv$bio1), ]

    # Absences -- surveyed cells where the species was not recorded
    absenceCells <- setdiff(allEnvClean$cell, spEnv$cell)
    absences <- allEnvClean |>
      dplyr::filter(cell %in% absenceCells) |>
      dplyr::mutate(birdlife_scientific_name = sp,
                     birdlife_code = unique(spOcc$birdlife_code)[1],
                     occurrence = 0)
    message("    Absences: ", nrow(absences))

    keepCols <- c("cell50x50", "birdlife_code", "birdlife_scientific_name",
                  "occurrence", "x", "y", "cell", names(bioclim))

    spPaEnv <- dplyr::bind_rows(spEnv[, intersect(keepCols, names(spEnv))],
                                 absences[, intersect(keepCols, names(absences))])
    message("    Total: ", nrow(spPaEnv),
            " (", sum(spPaEnv$occurrence == 1), " pres / ",
            sum(spPaEnv$occurrence == 0), " abs)")

    if (useThinning) {
      spThinDist <- if (!is.null(perSpeciesThinDist) && sp %in% names(perSpeciesThinDist)) {
        perSpeciesThinDist[[sp]]
      } else {
        thinDist
      }
      message("    Spatial thinning at ", spThinDist / 1000, "km...")
      spSf <- sf::st_as_sf(spPaEnv, coords = c("x", "y"), crs = terra::crs(bioclim))

      spThinned <- thin(spSf, thinDist = spThinDist, runs = 5)
      thinnedCoords <- sf::st_coordinates(spThinned)
      spThinnedDf <- sf::st_drop_geometry(spThinned)
      spThinnedDf$x <- thinnedCoords[, 1]
      spThinnedDf$y <- thinnedCoords[, 2]

      message("    After thinning: ", nrow(spThinnedDf),
              " (", sum(spThinnedDf$occurrence == 1), " pres / ",
              sum(spThinnedDf$occurrence == 0), " abs)")
    } else {
      message("    Spatial thinning disabled -- keeping all ", nrow(spPaEnv), " rows")
      spThinnedDf <- spPaEnv
    }

    saveRDS(spThinnedDf, outFile)
    message("    Saved -> ", outFile)
    outFiles[[sp]] <- outFile
  }

  message("\nDone. Output files in: ", outputDir)
  invisible(unlist(outFiles))
}
