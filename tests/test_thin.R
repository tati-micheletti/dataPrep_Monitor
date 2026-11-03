# Test that the faster thin() (improvements.md item 24) returns EXACTLY the points of the old one, with the same random draws (from the repo root: Rscript modules/dataPrep_Monitor/tests/test_thin.R).
# thinOld below is the old function, unchanged (copy of modules/dataPrep_Monitor/R/thin.R before 2026-11-03).
suppressMessages({ library(sf); library(purrr) })
source("modules/dataPrep_Monitor/R/thin.R")
thinOld <- function(sfObj, thinDist, runs = 5) {

  sampleVec <- function(x, ...) x[sample(length(x), ...)]

  sfBuffer <- sf::st_buffer(sfObj, thinDist)
  buffInt <- sf::st_intersects(sfObj, sfBuffer)
  buffInt <- setNames(buffInt, seq_along(buffInt))
  nInt <- purrr::map_dbl(buffInt, length)

  seeds <- sample.int(n = runs)
  resultsRuns <- lapply(seeds, function(i) {
    set.seed(i)
    bi <- buffInt
    ni <- nInt
    while (max(ni) > 1) {
      maxNeighbors <- names(which(ni == max(ni)))
      sampledId <- sampleVec(maxNeighbors, 1)
      bi[[sampledId]] <- NULL
      bi <- purrr::map(bi, function(x) setdiff(x, as.numeric(sampledId)))
      ni <- purrr::map_dbl(bi, length)
    }
    unique(unlist(bi))
  })

  lengths <- purrr::map_dbl(resultsRuns, length)
  selectedRun <- resultsRuns[[sampleVec(which(lengths == max(lengths)), 1)]]
  sfObj[selectedRun, ]
}

mkpts <- function(x, y) { s <- st_as_sf(data.frame(id = seq_along(x), x = x, y = y), coords = c("x", "y"), crs = 3035, remove = FALSE); s }
same <- function(s, d, what, runs = 5) {
  set.seed(s); a <- thinOld(pts <- get("PTS", envir = globalenv()), d, runs = runs); set.seed(s); b <- thin(get("PTS", envir = globalenv()), d, runs = runs)
  stopifnot(identical(a$id, b$id), identical(st_coordinates(a), st_coordinates(b)), identical(names(a), names(b))); invisible(nrow(a)) }
chk <- function(x, y, d, what, seeds = 1:6) { force(x); force(y); force(d); assign("PTS", mkpts(x, y), envir = globalenv()); k <- sapply(seeds, function(s) same(s, d, what)); cat(sprintf("ok: %-62s n = %4d, retained %s
", what, length(x), paste(unique(k), collapse = "/"))); invisible(NULL) }
set.seed(100)
chk(runif(60, 0, 3000), runif(60, 0, 3000), 500, "random points, sparse (few neighbours)")
chk(runif(300, 0, 5000), runif(300, 0, 5000), 700, "random points, medium")
chk(runif(400, 0, 2000), runif(400, 0, 2000), 1000, "random points, dense (many neighbours)")
g <- expand.grid(x = seq(0, 9800, 200), y = seq(0, 3800, 200)); chk(g$x + 0, g$y + 0, 600, "points on a 200 m grid (the habitat case), thinning 600 m")
chk(g$x, g$y, 2000, "same grid, thinning 2000 m"); chk(g$x, g$y, 200, "same grid, thinning = grid spacing (points exactly at the distance)")
chk(g$x, g$y, 199.9, "same grid, thinning just below the spacing (no neighbours)"); chk(g$x, g$y, 200.1, "same grid, thinning just above the spacing")
dd <- 1000; chk(c(0, dd - 1, 5000, 5000 + dd + 1, 9000, 9000 + dd), c(0, 0, 0, 0, 0, 0), dd, "pairs just below / above / at the thinning distance")
chk(c(0, 0, 0, 0), c(0, 0, 0, 0), 500, "identical coordinates")
chk(c(0, 400, 800, 1200, 1600), rep(0, 5), 500, "a chain (each point reaches only its neighbours)")
chk(runif(40, 0, 100000), runif(40, 0, 100000), 100, "no neighbours at all")
chk(1, 1, 500, "one point"); chk(c(0, 100), c(0, 0), 500, "two points that are neighbours"); chk(c(0, 5000), c(0, 0), 500, "two points that are not")
cl <- do.call(rbind, lapply(1:25, function(i) { cx <- runif(1, 0, 20000); cy <- runif(1, 0, 20000); data.frame(x = cx + rnorm(24, 0, 300), y = cy + rnorm(24, 0, 300)) })); chk(cl$x, cl$y, 400, "25 clusters of 24 points")
for (s in 1:40) { n <- sample(5:150, 1); chk(runif(n, 0, 3000), runif(n, 0, 3000), sample(c(100, 300, 700, 1500), 1), sprintf("random set %d", s), seeds = s) }
for (r in c(1, 3, 10)) { assign("PTS", mkpts(runif(120, 0, 3000), runif(120, 0, 3000)), envir = globalenv()); same(7, 600, "runs", runs = r); cat("ok: runs =", r, "
") }
# speed: a clustered set of about 750 points (the old function needs about an hour for 5,000)
set.seed(5); big <- do.call(rbind, lapply(1:30, function(i) { cx <- runif(1, 0, 30000); cy <- runif(1, 0, 30000); data.frame(x = cx + rnorm(25, 0, 400), y = cy + rnorm(25, 0, 400)) })); assign("PTS", mkpts(big$x, big$y), envir = globalenv())
set.seed(1); to <- system.time(a <- thinOld(PTS, 700, runs = 5))[["elapsed"]]; set.seed(1); tn <- system.time(b <- thin(PTS, 700, runs = 5))[["elapsed"]]; stopifnot(identical(a$id, b$id))
cat(sprintf("ok: %d clustered points: old %.1f s, new %.2f s, identical (%d retained)
", nrow(PTS), to, tn, nrow(a)))
big2 <- do.call(rbind, lapply(1:200, function(i) { cx <- runif(1, 0, 60000); cy <- runif(1, 0, 60000); data.frame(x = cx + rnorm(25, 0, 400), y = cy + rnorm(25, 0, 400)) })); assign("PTS", mkpts(big2$x, big2$y), envir = globalenv())
set.seed(1); tn2 <- system.time(b2 <- thin(PTS, 700, runs = 5))[["elapsed"]]; cat(sprintf("new function alone: %d points in %.2f s (%d retained)
", nrow(PTS), tn2, nrow(b2)))
message("test_thin: ok")
