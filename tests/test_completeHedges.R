# Local test of completeHedges() on synthetic land-use files (from the repo root: Rscript modules/dataPrep_Monitor/tests/test_completeHedges.R). Years without hedge data (2005-2016, 2022-2023) must be filled in ALL files, also the unrequested ones.
suppressMessages(library(terra)); for (f in c("laeaAnalytic.R","studyArea.R","completeHedges.R")) source(file.path("modules/dataPrep_Monitor/R", f))
scaleLabel <- function(r) c("200"="scale_02","1000"="scale_1")[as.character(r)]
d <- tempfile(); dir.create(file.path(d, "scale_02"), recursive = TRUE)
mk <- function(y, h) { r <- rast(nrows=10,ncols=10,xmin=0,xmax=1000,ymin=0,ymax=1000,crs="EPSG:3035",nlyrs=2); names(r) <- c("crop","hedges"); values(r) <- cbind(runif(100), if (h) runif(100) else NA); writeRaster(r, file.path(d,"scale_02",paste0("landuse_",y,"_habitat_scale_02.tif")), overwrite=TRUE) }
for (y in 2005:2025) mk(y, y %in% c(2017:2021, 2024:2025))
res <- completeHedges(d, 2022:2025, 200, numeric(0), mode = "nearest")   # only 2022:2025 requested; 2005:2016 and 2022:2023 must be filled
print(table(res$action)); print(res[res$year %in% c(2005, 2016, 2022, 2023), ])
bad <- sapply(2005:2025, function(y) global(rast(file.path(d,"scale_02",paste0("landuse_",y,"_habitat_scale_02.tif")))[["hedges"]], function(x) any(is.na(x)))[1,1]); cat("years still with NA:", sum(bad), "\n")
stopifnot(sum(bad) == 0, nrow(res) == 14, all(res$from[res$year <= 2016] == 2017), all(res$from[res$year %in% 2022:2023] == 2021)); message("test_completeHedges: ok")

# ---- mode = "mean" (2026-11-03): every file gets the SAME fixed layer = mean of the years with data; a second call changes nothing; a recomputed file is fixed again ----
d2 <- tempfile(); dir.create(file.path(d2, "scale_02"), recursive = TRUE); set.seed(2)
mk2 <- function(y, h) { r <- rast(nrows=10,ncols=10,xmin=0,xmax=1000,ymin=0,ymax=1000,crs="EPSG:3035",nlyrs=2); names(r) <- c("crop","hedges"); values(r) <- cbind(runif(100), if (h) runif(100) else NA); writeRaster(r, file.path(d2,"scale_02",paste0("landuse_",y,"_habitat_scale_02.tif")), overwrite=TRUE) }
withData <- c(2017:2021, 2024:2025); for (y in 2005:2025) mk2(y, y %in% withData)
fl <- function(y) file.path(d2, "scale_02", paste0("landuse_", y, "_habitat_scale_02.tif"))
expected <- mean(rast(lapply(withData, function(y) rast(fl(y))[["hedges"]])), na.rm = TRUE)
r1 <- completeHedges(d2, 2022:2025, 200, numeric(0), mode = "mean"); stopifnot(nrow(r1) == 21, all(r1$action == "fixed mean"), all(r1$from == 7))
for (y in c(2005, 2016, 2017, 2022, 2024)) { h <- rast(fl(y))[["hedges"]]; stopifnot(isTRUE(all.equal(as.vector(values(h)), as.vector(values(expected)), tolerance = 1e-5))) }
stopifnot(all(file.exists(sapply(2005:2025, function(y) paste0(fl(y), ".hedgesfixed"))))); message("mean: every year has the mean layer of the 7 years with data")
r2 <- completeHedges(d2, 2022:2025, 200, numeric(0), mode = "mean"); stopifnot(nrow(r2) == 0); message("mean: a second call changes nothing")
mk2(2019, TRUE); file.remove(paste0(fl(2019), ".hedgesfixed")); mk2(2020, TRUE); file.remove(paste0(fl(2020), ".hedgesfixed"))      # two files recomputed (raw hedges, no sidecar)
gp <- tryCatch(completeHedges(d2, 2022:2025, 200, numeric(0), mode = "mean"), error = function(e) conditionMessage(e)); stopifnot(is.character(gp), grepl("partial recompute", gp)); message("mean: a partial recompute (2 of the 7 hedge years) stops (guard, audit S2-28)")
r3 <- completeHedges(d2, 2022:2025, 200, numeric(0), mode = "mean", allowPartialMean = TRUE); stopifnot(nrow(r3) == 21)                                     # the mean is made anew from the 2 raw years: all 21 files change
exp2 <- mean(rast(lapply(c(2019, 2020), function(y) rast(fl(y))[["hedges"]])), na.rm = TRUE)
stopifnot(isTRUE(all.equal(as.vector(values(rast(fl(2005))[["hedges"]])), as.vector(values(exp2)), tolerance = 1e-5))); message("mean: recomputed files give a new mean over the raw years; test_completeHedges (mean): ok")
