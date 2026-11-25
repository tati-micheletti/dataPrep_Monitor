# Test: a NEW year with raw hedge data adopts the existing fixed hedge layer; the files of the old years stay byte-identical (2026-11-26).
# From the repo root: Rscript modules/dataPrep_Monitor/tests/test_completeHedgesNewYear.R
suppressMessages(library(terra))
for (f in c("scaleLabel", "completeHedges")) source(file.path("modules/dataPrep_Monitor/R", paste0(f, ".R")))
chk <- function(label, ok) { cat(if (isTRUE(ok)) "ok   " else "FAIL ", label, "\n"); if (!isTRUE(ok)) quit(status = 1) }
root <- tempfile(); d <- file.path(root, scaleLabel(1000)); dir.create(d, recursive = TRUE)
mk <- function(y, hedge) { r <- rast(nrows = 6, ncols = 6, xmin = 0, xmax = 6000, ymin = 0, ymax = 6000, crs = "EPSG:3035"); cls <- r; values(cls) <- 1; hh <- r; values(hh) <- hedge
  out <- c(cls, hh); names(out) <- c("class", "hedges"); fn <- file.path(d, sprintf("landuse_%d_landscape_%s.tif", y, basename(d))); writeRaster(out, fn, overwrite = TRUE); fn }
oldTag <- "fixed mean of the hedge layers of 2017,2018"
f17 <- mk(2017, 5); f18 <- mk(2018, 5); for (f in c(f17, f18)) writeLines(oldTag, paste0(f, ".hedgesfixed"))     # the two old files carry the fixed layer (value 5)
f19 <- mk(2019, 9)                                                                                                   # the NEW year: raw hedge data (value 9), not fixed
h17 <- tools::md5sum(f17); h18 <- tools::md5sum(f18)
res <- suppressMessages(completeHedges(root, 2017:2019, numeric(0), 1000, mode = "mean", repairGaps = FALSE))
chk("the new year was fixed (one action row)", nrow(res) == 1 && res$year == 2019)
chk("the new year has the existing fixed layer (5), not its raw hedges (9)", all(values(rast(f19)[["hedges"]]) == 5))
chk("other layers of the new year are untouched", all(values(rast(f19)[["class"]]) == 1))
chk("the new year carries the SAME record as the old files", readLines(paste0(f19, ".hedgesfixed"))[1] == oldTag)
chk("the old files are byte-identical", identical(unname(h17), unname(tools::md5sum(f17))) && identical(unname(h18), unname(tools::md5sum(f18))))
res2 <- suppressMessages(completeHedges(root, 2017:2019, numeric(0), 1000, mode = "mean", repairGaps = FALSE)); chk("a second call changes nothing", nrow(res2) == 0)
# partial recompute (an old year gets raw hedges again, together with a new year) must still STOP (audit S2-28)
f20 <- mk(2020, 7); file.remove(paste0(f18, ".hedgesfixed")); r <- tryCatch(suppressMessages(completeHedges(root, 2017:2020, numeric(0), 1000, mode = "mean", repairGaps = FALSE)), error = function(e) "stopped")
chk("a partial recompute of an old year together with a new one still stops", identical(r, "stopped"))
cat("all passed\n")
