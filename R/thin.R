#' Spatial thinning of presence/absence points
#'
#' Iteratively removes the point with the most neighbours within
#' `thinDist`, repeated over `runs` random seeds, keeping the run that
#' retains the most points. Follows Wiedenroth et al.
#'
#' @param sfObj An `sf` object of points.
#' @param thinDist Numeric. Thinning distance in the units of `sfObj`'s CRS.
#' @param runs Integer. Number of random seeds to try; the best (largest)
#'   result is kept.
#' @return `sfObj` subset to the thinned rows.
thin <- function(sfObj, thinDist, runs = 5) {

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
