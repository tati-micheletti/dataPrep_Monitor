# Local test of the hedge-gap repair (2026-11-05; from the repo root: Rscript modules/dataPrep_Monitor/tests/test_completeHedgesGap.R).
# The v3 run stopped in checkNoNAInsideStudyArea(): the land-use files of 2024-2025 (CTM, large grid) have rim cells beyond the grid of the first hedge year (HCTM footprint), where the fixed mean has no value.
# test_completeHedges.R used identical grids for every year and could not show this. Here: (1) two different grids through completeHedges(mode = "mean"), (2) the filling rules of fillHedgeGaps(), (3) the stops.
suppressMessages(library(terra)); for (f in c("laeaAnalytic.R", "studyArea.R", "completeHedges.R")) source(file.path("modules/dataPrep_Monitor/R", f))
scaleLabel <- function(r) c("200" = "scale_02", "1000" = "scale_1")[as.character(r)]
set.seed(7)
mkR <- function(file, ext, hedges) { r <- rast(xmin = ext[1], xmax = ext[2], ymin = ext[3], ymax = ext[4], resolution = 100, crs = "EPSG:3035", nlyrs = 2); names(r) <- c("crop", "hedges")
  values(r) <- cbind(runif(ncell(r)), if (hedges) runif(ncell(r)) else NA); writeRaster(r, file, overwrite = TRUE) }

# ---- (1) two grids: 2005-2023 on grid A (like the HCTM footprint), 2024-2025 on grid B (larger, shifted by half a cell, like the CTM-based files) ----
d <- tempfile(); dir.create(file.path(d, "scale_02"), recursive = TRUE); fl <- function(y) file.path(d, "scale_02", paste0("landuse_", y, "_habitat_scale_02.tif"))
gridA <- c(0, 1000, 0, 1000); gridB <- c(-150, 1050, -150, 1050); big <- 2024:2025; withData <- c(2017:2021, big)
for (y in 2005:2025) mkR(fl(y), if (y %in% big) gridB else gridA, y %in% withData)
res <- completeHedges(d, 2022:2025, 200, numeric(0), mode = "mean"); print(table(res$action)); print(res[res$action == "gap filled", ])
stopifnot("gap filled" %in% res$action, all(res$year[res$action == "gap filled"] %in% big), sum(res$action == "fixed mean") == 21)
for (y in 2005:2025) { r <- rast(fl(y)); stopifnot(global(is.na(r[["hedges"]]) & !is.na(r[["crop"]]), "sum")[1, 1] == 0) }      # no empty hedge cell where the file has other values, in ANY year
message("(1) two grids: the rim of the larger files is filled, every year is complete")
res2 <- completeHedges(d, 2022:2025, 200, numeric(0), mode = "mean"); stopifnot(nrow(res2) == 0); message("(1) a second call changes nothing")

# ---- (2) the rules, on a hand-made file: nearest valid cell, ties -> mean, no fill where the other layers are empty (outside the mask) ----
d2 <- tempfile(); dir.create(file.path(d2, "scale_02"), recursive = TRUE); f2 <- file.path(d2, "scale_02", "landuse_2024_habitat_scale_02.tif")
mk <- function(file, gaps, cropNA) { m <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 1000, ymin = 0, ymax = 1000, crs = "EPSG:3035", nlyrs = 2); names(m) <- c("crop", "hedges")
  cr <- runif(100); hd <- rep(0.2, 100); hd[cellFromRowCol(m, 3, 3)] <- 0.9; for (g in gaps) hd[cellFromRowCol(m, g[1], g[2])] <- NA; for (g in cropNA) cr[cellFromRowCol(m, g[1], g[2])] <- NA
  values(m) <- cbind(cr, hd); writeRaster(m, file, overwrite = TRUE) }
mk(f2, gaps = list(c(1, 1), c(3, 4), c(10, 1)), cropNA = list(c(10, 1)))
g <- fillHedgeGaps(f2); stopifnot(g$action == "gap filled", g$from == 2, g$year == 2024, g$scale == "scale_02")                       # (10,1) has no value in the other layer: not filled
hv <- values(rast(f2)[["hedges"]], mat = FALSE); cel <- function(rw, cl) cellFromRowCol(rast(f2), rw, cl)
stopifnot(isTRUE(all.equal(hv[cel(1, 1)], 0.2, tolerance = 1e-6)),                                                                                  # nearest: (1,2) and (2,1), both 0.2
          isTRUE(all.equal(hv[cel(3, 4)], (0.9 + 0.2 * 3) / 4, tolerance = 1e-6)),                                                                   # four cells at distance 1 (one is the 0.9 cell): their mean
          is.na(hv[cel(10, 1)]), isTRUE(all.equal(hv[cel(3, 3)], 0.9, tolerance = 1e-6)))                                                              # an existing value is never changed (files hold 32-bit floats: tolerance 1e-6)
stopifnot(file.exists(paste0(f2, ".hedgegaps")), is.null(fillHedgeGaps(f2))); message("(2) rules ok; the sidecar makes a second call a no-op")
g0 <- fillHedgeGaps(f2, dryRun = TRUE); stopifnot(is.null(g0)); message("(2) a dry run on a repaired file reports nothing")

# ---- (3) the stops: too many empty cells, and an empty cell far from any valid one, are NOT rim effects ----
mk(f2, gaps = list(c(1, 1), c(3, 4), c(10, 10)), cropNA = list()); unlink(paste0(f2, ".hedgegaps"))
e1 <- tryCatch({ fillHedgeGaps(f2, maxCells = 2L); "" }, error = function(e) conditionMessage(e)); stopifnot(grepl("not a rim effect", e1)); message("(3) too many cells: stops")
m3 <- rast(nrows = 20, ncols = 20, xmin = 0, xmax = 2000, ymin = 0, ymax = 2000, crs = "EPSG:3035", nlyrs = 2); names(m3) <- c("crop", "hedges"); hd <- rep(0.3, 400)
for (rw in 6:14) for (cl in 6:14) hd[cellFromRowCol(m3, rw, cl)] <- NA; values(m3) <- cbind(runif(400), hd); f3 <- file.path(d2, "scale_02", "landuse_2025_habitat_scale_02.tif"); writeRaster(m3, f3, overwrite = TRUE)
e2 <- tryCatch({ fillHedgeGaps(f3, maxDistM = 300); "" }, error = function(e) conditionMessage(e)); stopifnot(grepl("no valid cell within", e2)); message("(3) an empty block farther than maxDistM from valid cells: stops")
message("test_completeHedgesGap: ok")
