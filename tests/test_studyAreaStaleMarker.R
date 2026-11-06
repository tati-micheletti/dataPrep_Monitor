# Local test of the stale-marker rule of applyStudyAreaToProcessed() (2026-11-05; from the repo root: Rscript modules/dataPrep_Monitor/tests/test_studyAreaStaleMarker.R). Synthetic layers, no real data.
# v3 prep: the heterogeneity and unmapped-share files were rewritten (uncropped) but kept the markers (.studyarea) of the 29 October files, which have the same fingerprint, so the crop step took them as "current"
# and left them on the large CTM grid; the habitat covariate stack then stopped with "[rast] extents do not match". A file NEWER than its marker must be cropped again.
suppressMessages(library(terra)); for (f in c("laeaAnalytic.R", "studyArea.R", "completeHedges.R")) source(file.path("modules/dataPrep_Monitor/R", f))
ok <- 0L; fail <- 0L; check <- function(what, cond) { if (isTRUE(cond)) { ok <<- ok + 1L; message("ok   ", what) } else { fail <<- fail + 1L; message("FAIL ", what) } }
area <- terra::vect("POLYGON ((2000 2000, 8000 2000, 8000 8000, 2000 8000, 2000 2000))", crs = "EPSG:3035"); area$label <- "Test"
root <- file.path(tempfile(), "processed"); d <- file.path(root, "scale_02"); dir.create(d, recursive = TRUE)
mk <- function(f) { r <- rast(nrows = 100, ncols = 100, xmin = 0, xmax = 10000, ymin = 0, ymax = 10000, crs = "EPSG:3035"); values(r) <- runif(ncell(r)); names(r) <- "v"; writeRaster(r, f, overwrite = TRUE) }
fA <- file.path(d, "landscape_heterogeneity_2024_habitat_scale_02.tif"); fB <- file.path(d, "landuse_2024_habitat_scale_02.tif"); mk(fA); mk(fB)
big <- ncell(rast(fA))
r1 <- applyStudyAreaToProcessed(root, area, recordsBufferM = 1000, climatePredictBufferM = 50000, climateScaleLabel = "scale_50", ebba2TrainingYear = 2017)
check("first pass: both layers cropped", sum(r1$action == "cropped and masked") == 2 && ncell(rast(fA)) < big && ncell(rast(fB)) < big)
cropped <- ncell(rast(fA))
r2 <- applyStudyAreaToProcessed(root, area, 1000, 50000, "scale_50", 2017); check("second pass changes nothing", !any(r2$action == "cropped and masked"))
# the v3 case: the file is written again, uncropped, AFTER its marker (marker older), same fingerprint inside the marker
mk(fA); Sys.setFileTime(paste0(fA, ".studyarea"), Sys.time() - 3600)
check("precondition: the uncropped file is back and its marker is older than it", ncell(rast(fA)) == big && file.mtime(paste0(fA, ".studyarea")) < file.mtime(fA))
r3 <- applyStudyAreaToProcessed(root, area, 1000, 50000, "scale_50", 2017)
check("stale marker: the file is cropped again, the other layer is left alone", sum(r3$action == "cropped and masked") == 1 && ncell(rast(fA)) == cropped && ncell(rast(fB)) == cropped)
r4 <- applyStudyAreaToProcessed(root, area, 1000, 50000, "scale_50", 2017); check("after that nothing to do (the marker is new again)", !any(r4$action == "cropped and masked"))
# a file that is touched by completeHedges::fillHedgeGaps keeps a fresh marker (no pointless second crop)
r <- rast(fB); h <- rast(r, nlyrs = 2); names(h) <- c("grassland", "hedges"); values(h) <- cbind(runif(ncell(h)), runif(ncell(h))); nm <- file.path(d, "landuse_2025_habitat_scale_02.tif"); writeRaster(h, nm, overwrite = TRUE)
hv <- values(h[["hedges"]], mat = FALSE); hv[c(1, 2)] <- NA; hh <- h; values(hh) <- cbind(values(h[["grassland"]], mat = FALSE), hv); writeRaster(hh, nm, overwrite = TRUE)
writeLines(studyAreaFingerprint(area, 1000), paste0(nm, ".studyarea")); Sys.setFileTime(paste0(nm, ".studyarea"), Sys.time() - 3600)      # a cropped file with an old but valid marker
g <- fillHedgeGaps(nm); check("hedge fill repaired the 2 empty cells", !is.null(g) && g$from == 2)
r5 <- applyStudyAreaToProcessed(root, area, 1000, 50000, "scale_50", 2017); check("a file repaired by the hedge fill is not cropped again (its marker was refreshed)", !any(grepl("landuse_2025", r5$file[r5$action == "cropped and masked"])))
message("\n", ok, " passed, ", fail, " failed"); unlink(dirname(root), recursive = TRUE); if (fail) quit(status = 1)
