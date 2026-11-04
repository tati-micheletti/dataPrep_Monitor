# Local test of computeLanduse() with the HCTM-first rule and the CTM -> HCTM recode, on tiny synthetic maps (from the repo root: Rscript modules/dataPrep_Monitor/tests/test_computeLanduse.R).
# 6 x 6 cells of 1 km. Column 1-2: grassland in both products; 3-4: winter cereals; 5: hedge in CTM / grassland in HCTM; 6: not mapped in CTM / grassland in HCTM.
suppressMessages(library(terra))
for (f in c("landuseCategoriesCTM.R", "landuseCategoriesHCTM.R", "getLanduseCategories.R", "makeCategoryProportionLayer.R", "regridToResolution.R", "isValidRasterFile.R", "scaleLabel.R", "landuseRecode.R", "computeLanduse.R"))
  source(file.path("modules/dataPrep_Monitor/R", f))
raw <- tempfile("raw"); proc <- tempfile("proc"); dir.create(raw); dir.create(proc)
mk <- function(file, ncol, codes) {                                                       # codes: value of each of the 6 target columns
  r <- rast(nrows = ncol, ncols = ncol, xmin = 0, xmax = 6000, ymin = 0, ymax = 6000, crs = "EPSG:3035"); perCell <- ncol / 6
  values(r) <- rep(rep(codes, each = perCell), times = ncol); writeRaster(r, file.path(raw, file), overwrite = TRUE) }
ctm <- c(200, 200, 1101, 1101, 3001, NA); hctm <- c(200, 200, 110, 110, 200, 200)
mk("CTM_GER_2019_rst_v202_COG.tif", 600, ctm); mk("HCTM_GER_2019_rst_v101_COG.tif", 300, hctm)
mk("HCTM_GER_2022_rst_v101_COG.tif", 300, hctm)
mk("CTM_GER_2024_rst_v302_COG.tif", 600, ctm)
# CTM and HCTM do not cover the same extent in reality (CTM 4016026-4676026 x 2654920-3554920, HCTM 4031326-4672516 x 2684080-3551470): 2018 = CTM larger than HCTM, same grid origin; 2020 = also another origin
mkx <- function(file, ncells, xmin, xmax, ymin, ymax, codes7) {
  r <- rast(nrows = ncells, ncols = ncells, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, crs = "EPSG:3035")
  cx <- xFromCol(r, seq_len(ncol(r))); col1km <- pmin(pmax(floor(cx / 1000) + 1, 1), 7)          # which 1 km column (1..7) each cell belongs to
  values(r) <- rep(codes7[col1km], times = nrow(r)); writeRaster(r, file.path(raw, file), overwrite = TRUE) }
ctm7 <- c(200, 200, 1101, 1101, 3001, NA, 200)
mkx("CTM_GER_2018_rst_v202_COG.tif", 700, 0, 7000, 0, 7000, ctm7); mkx("HCTM_GER_2018_rst_v101_COG.tif", 300, 0, 6000, 0, 6000, c(hctm, 200))
mkx("CTM_GER_2020_rst_v202_COG.tif", 700, 0, 7000, 0, 7000, ctm7); mkx("HCTM_GER_2020_rst_v101_COG.tif", 190, 500, 6200, 500, 6200, c(hctm, 200))
cats <- c(names(landuseCategoriesCTM()), "other"); P <- diag(length(cats)); dimnames(P) <- list(cats, cats)        # toy recode: identity except ...
P["grassland", ] <- 0; P["grassland", "grassland"] <- 0.5; P["grassland", "fallow"] <- 0.5                          # CTM grassland = half grassland, half fallow in HCTM
P["other", ] <- 0; P["other", "grassland"] <- 0.2; P["other", "other"] <- 0.8                                       # 20% of what CTM leaves unmapped is grassland in HCTM
P["hedges", ] <- 0; P["hedges", "grassland"] <- 1                                                                   # a CTM hedge is grassland in HCTM
recodeFile <- tempfile(fileext = ".csv"); write.csv(data.frame(from_ctm_class = cats, P, check.names = FALSE), recodeFile, row.names = FALSE)
run <- function(y, ...) { out <- computeLanduse(y, raw, proc, numeric(0), 1000, "EPSG:3035", recodeFile = recodeFile, ...); rast(out[[1]]) }
row1 <- function(r, nm) round(as.vector(values(r[[nm]]))[1:6], 3)
same <- function(a, b, what) { stopifnot(isTRUE(all.equal(a, b, check.attributes = FALSE))); cat("ok:", what, "\n") }
suppressMessages({
  r19 <- run(2019)                                                                         # both products: classes from HCTM, hedges from CTM
  same(row1(r19, "grassland"), c(1, 1, 0, 0, 1, 1), "2019 grassland from HCTM"); same(row1(r19, "winter_cereals"), c(0, 0, 1, 1, 0, 0), "2019 winter cereals from HCTM")
  same(row1(r19, "hedges"), c(0, 0, 0, 0, 1, 0), "2019 hedges from CTM")
  r22 <- run(2022); stopifnot(all(is.na(row1(r22, "hedges")))); same(row1(r22, "grassland"), c(1, 1, 0, 0, 1, 1), "2022 grassland from HCTM; hedges NA")
  r24 <- run(2024)                                                                         # CTM only: recoded
  same(row1(r24, "grassland"), c(0.5, 0.5, 0, 0, 1, 0.2), "2024 grassland recoded"); same(row1(r24, "fallow"), c(0.5, 0.5, 0, 0, 0, 0), "2024 fallow recoded")
  same(row1(r24, "winter_cereals"), c(0, 0, 1, 1, 0, 0), "2024 winter cereals kept"); same(row1(r24, "hedges"), c(0, 0, 0, 0, 1, 0), "2024 hedges kept from CTM")
  stopifnot(identical(names(r24), names(r19))); cat("ok: same 14 layers in the same order in every year\n")
  r18 <- run(2018)                                                                         # CTM covers more than HCTM: must not fail ("extents do not match"), hedges on the class grid
  same(row1(r18, "hedges"), c(0, 0, 0, 0, 1, 0), "2018 hedges from a larger CTM map land on the class grid"); same(row1(r18, "grassland"), c(1, 1, 0, 0, 1, 1), "2018 grassland from HCTM")
  r20 <- run(2020); stopifnot(isTRUE(compareGeom(r20[["hedges"]], r20[["grassland"]], stopOnError = FALSE)))
  hv <- as.vector(values(r20[["hedges"]])); stopifnot(max(hv, na.rm = TRUE) > 0.4, min(hv, na.rm = TRUE) >= 0, max(hv, na.rm = TRUE) <= 1, !anyNA(hv), abs(sum(hv) - nrow(r20)) < 0.01)   # a hedge column falls half into each of two class cells (0.5 + 0.5 per row): the total mass (one per row) is kept; cat("ok: 2020 hedges on the class grid although CTM and HCTM have another extent and grid origin\n")
  rl <- run(2019, preferHCTM = FALSE); same(row1(rl, "grassland"), c(1, 1, 0, 0, 0, 0), "2019 old rule: classes from CTM"); same(row1(rl, "hedges"), c(0, 0, 0, 0, 1, 0), "2019 old rule: hedges from CTM")
})
msgs <- function(expr) { m <- character(); withCallingHandlers(expr, message = function(c) { m <<- c(m, conditionMessage(c)); invokeRestart("muffleMessage") }); m }
m1 <- msgs(run(2024)); stopifnot(any(grepl("Cache hit", m1))); cat("ok: a second call with the same rule is a cache hit\n")
P["other", "grassland"] <- 0.3; P["other", "other"] <- 0.7; write.csv(data.frame(from_ctm_class = cats, P, check.names = FALSE), recodeFile, row.names = FALSE)
m2 <- msgs(r24b <- run(2024)); stopifnot(!any(grepl("Cache hit", m2))); same(row1(r24b, "grassland")[6], 0.3, "a changed recode recomputes the year")
m3 <- msgs(run(2019)); stopifnot(!any(grepl("Cache hit", m3)))                              # 2019 had been rewritten by the old rule: back to the new rule recomputes
cat("ok: a file made by another rule is recomputed\n")
# derived layers (heterogeneity, unmapped share) carry a stamp of the land-use file they come from
sd1 <- file.path(proc, "scale_1"); st1 <- landuseSchemeStamp(sd1, 2019, "landscape"); stopifnot(nzchar(st1), st1 != "legacy")
st2 <- landuseSchemeStamp(sd1, 2024, "landscape"); stopifnot(st2 != "legacy", st2 != st1); stopifnot(landuseSchemeStamp(sd1, 1999, "landscape") == "legacy")
invisible(suppressMessages(run(2019, preferHCTM = FALSE))); st3 <- landuseSchemeStamp(sd1, 2019, "landscape"); stopifnot(st3 != st1)
cat("ok: the land-use stamp differs between years, rules and recodes, and is 'legacy' without a record\n"); message("test_computeLanduse: ok")
