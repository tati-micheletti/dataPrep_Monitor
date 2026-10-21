# Local test of completeHedges() on synthetic land-use files (from the repo root: Rscript modules/dataPrep_Monitor/tests/test_completeHedges.R). Years without hedge data (2005-2016, 2022-2023) must be filled in ALL files, also the unrequested ones.
suppressMessages(library(terra)); for (f in c("laeaAnalytic.R","studyArea.R","completeHedges.R")) source(file.path("modules/dataPrep_Monitor/R", f))
scaleLabel <- function(r) c("200"="scale_02","1000"="scale_1")[as.character(r)]
d <- tempfile(); dir.create(file.path(d, "scale_02"), recursive = TRUE)
mk <- function(y, h) { r <- rast(nrows=10,ncols=10,xmin=0,xmax=1000,ymin=0,ymax=1000,crs="EPSG:3035",nlyrs=2); names(r) <- c("crop","hedges"); values(r) <- cbind(runif(100), if (h) runif(100) else NA); writeRaster(r, file.path(d,"scale_02",paste0("landuse_",y,"_habitat_scale_02.tif")), overwrite=TRUE) }
for (y in 2005:2025) mk(y, y %in% c(2017:2021, 2024:2025))
res <- completeHedges(d, 2022:2025, 200, numeric(0))   # only 2022:2025 requested; 2005:2016 and 2022:2023 must be filled
print(table(res$action)); print(res[res$year %in% c(2005, 2016, 2022, 2023), ])
bad <- sapply(2005:2025, function(y) global(rast(file.path(d,"scale_02",paste0("landuse_",y,"_habitat_scale_02.tif")))[["hedges"]], function(x) any(is.na(x)))[1,1]); cat("years still with NA:", sum(bad), "\n")
stopifnot(sum(bad) == 0, nrow(res) == 14, all(res$from[res$year <= 2016] == 2017), all(res$from[res$year %in% 2022:2023] == 2021)); message("test_completeHedges: ok")
