# Local test of the nodata-aware recode (2026-11-06; from the repo root: Rscript modules/dataPrep_Monitor/tests/test_landuseRecodeNodata.R). Synthetic cells.
suppressMessages(library(terra)); source("modules/dataPrep_Monitor/R/landuseRecode.R")
P <- rbind(grassland = c(0.9, 0.0, 0.1), maize = c(0.0, 0.95, 0.05), other_mapped = c(0.5, 0.1, 0.4), nodata = c(0.03, 0.01, 0.96)); colnames(P) <- c("grassland", "maize", "other"); stopifnot(all(abs(rowSums(P) - 1) < 1e-9))
cell <- function(g, m, mp) { r <- rast(nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1); list(g = setValues(r, g), m = setValues(r, m), mp = setValues(r, mp)) }
run <- function(g, m, mp) { x <- cell(g, m, mp); o <- applyLanduseRecode(list(grassland = x$g, maize = x$m), P, mapped = x$mp); sapply(o, function(r) as.numeric(values(r))) }
a <- run(1, 0, 1);   stopifnot(isTRUE(all.equal(unname(a), c(0.9, 0))))                                  # mapped grassland cell: P row grassland
b <- run(0, 0, 0);   stopifnot(isTRUE(all.equal(unname(b), c(0.03, 0.01))))                              # a cell WITHOUT data (forest, town): about 4% crop, not 50%
c1 <- run(0.2, 0.3, 0.7); stopifnot(isTRUE(all.equal(unname(c1), c(0.2 * 0.9 + 0.2 * 0.5 + 0.3 * 0.03, 0.3 * 0.95 + 0.2 * 0.1 + 0.3 * 0.01))))   # other_mapped = 0.7 - 0.5 = 0.2, nodata = 0.3
Pold <- rbind(P[c("grassland", "maize"), ], other = c(0.424, 0.1, 0.476)); o <- applyLanduseRecode(list(grassland = cell(0, 0, 0)$g, maize = cell(0, 0, 0)$m), Pold)
stopifnot(as.numeric(values(o$grassland)) > 0.4)                                                          # the old application: a cell without data became 42% grassland (the v3 bug, kept only for old matrices)
e <- tryCatch(applyLanduseRecode(list(grassland = cell(1, 0, 1)$g, maize = cell(1, 0, 1)$m), P), error = function(e) conditionMessage(e)); stopifnot(grepl("mapped", e))
message("test_landuseRecodeNodata: ok")
