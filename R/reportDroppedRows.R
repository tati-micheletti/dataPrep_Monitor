#' Explain why most rows were dropped for NA covariates
#'
#' When NA-removal throws away most of a table (every row, in the worst case) the table is then
#' skipped and any OLD file on disk silently stays in place. This prints WHICH covariates are NA
#' and what the point coordinates look like, so that failure is diagnosable from the log.
#'
#' @param nBefore,nAfter Integer. Row counts before/after NA removal.
#' @param envDf data.frame of the extracted covariates (columns = covariates).
#' @param coordsMat Numeric matrix with columns x, y (the points).
#' @param label Character. What is being processed (species/year).
#' @return Invisibly NULL.
reportDroppedRows <- function(nBefore, nAfter, envDf, coordsMat, label = "") {
  if (nAfter >= 0.5 * nBefore) return(invisible(NULL))
  envDf <- envDf[, setdiff(names(envDf), "ID"), drop = FALSE]
  naBy <- vapply(envDf, function(v) sum(is.na(v)), numeric(1))
  naBy <- naBy[naBy > 0]
  message("  !! ", label, " kept only ", nAfter, " of ", nBefore, " rows after NA removal. NA count per covariate: ",
          if (length(naBy)) paste0(names(naBy), "=", naBy, collapse = ", ") else "none",
          " | NA coordinates: ", sum(is.na(coordsMat)),
          " | x range: ", paste(round(range(coordsMat[, 1], na.rm = TRUE)), collapse = ".."),
          " | y range: ", paste(round(range(coordsMat[, 2], na.rm = TRUE)), collapse = ".."))
  invisible(NULL)
}
