#' Spatial thinning of presence/absence points
#'
#' Iteratively removes the point with the most neighbours within
#' `thinDist`, repeated over `runs` random seeds, keeping the run that
#' retains the most points. Follows Wiedenroth et al.
#'
#' Same points as the earlier version of this function (improvements.md item 24; proved by
#' `tests/test_thin.R`, which keeps the old function as `thinOld`), only faster: the old version
#' rebuilt the neighbour lists of ALL points after every single removal (cost ~ removals x points,
#' 55 minutes for 5,000 records). Here the neighbour lists are made once with the same `st_buffer`
#' and `st_intersects` (so the point itself and the small asymmetry of the buffer polygon are
#' kept), one integer vector `ni` holds the number of neighbours of each point (-1 = removed),
#' and a removal subtracts 1 from the points whose list contained the removed one (a reverse index
#' of the lists made once, not the assumption that neighbours are symmetric). The random draws
#' (`set.seed(i)` per run, `sample(length(candidates), 1)` per removal, the same final choice among
#' the largest runs) are the same and in the same order.
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
  n <- length(buffInt)
  nInt <- lengths(buffInt)
  # reverse index: revInt[[j]] = the points whose list contains j (= the points that lose one neighbour when j is removed)
  revInt <- split(rep.int(seq_len(n), nInt), factor(unlist(buffInt, use.names = FALSE), levels = seq_len(n)))

  seeds <- sample.int(n = runs)
  resultsRuns <- lapply(seeds, function(i) {
    set.seed(i)
    ni <- as.integer(nInt)
    while (max(ni) > 1L) {
      candidates <- which(ni == max(ni))              # points still present with the most neighbours, ascending (as the names of the old list)
      sampledId <- sampleVec(candidates, 1)
      ni[sampledId] <- -1L
      hit <- revInt[[sampledId]]
      hit <- hit[ni[hit] >= 0L]
      ni[hit] <- ni[hit] - 1L
    }
    which(ni >= 0L)                                   # what unique(unlist(bi)) gave: every remaining list holds only the point itself
  })

  lengths <- vapply(resultsRuns, length, numeric(1))
  selectedRun <- resultsRuns[[sampleVec(which(lengths == max(lengths)), 1)]]
  sfObj[selectedRun, ]
}
