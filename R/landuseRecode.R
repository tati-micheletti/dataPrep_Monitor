#' Load the CTM -> HCTM recode matrix
#'
#' Written by `tools/buildLanduseRecode.R` (fitted where the two crop type maps overlap, 2017-2021). Row j = a CTM class (our 14 classes + "other"), column k = an HCTM class
#' (our 13 classes + "other"); entry = the share of the cells CTM puts in class j that HCTM puts in class k. Rows sum to 1.
#'
#' @param path Character. `data/landuseRecodeCTMtoHCTM.csv`.
#' @return Numeric matrix with row and column names.
loadLanduseRecode <- function(path) {
  if (is.null(path) || !file.exists(path)) stop("loadLanduseRecode(): recode file not found (", path, "). Build it with Rscript tools/buildLanduseRecode.R")
  d <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  P <- as.matrix(d[, setdiff(names(d), "from_ctm_class"), drop = FALSE]); rownames(P) <- d$from_ctm_class
  if (any(abs(rowSums(P) - 1) > 1e-3)) stop("loadLanduseRecode(): the rows of ", path, " must sum to 1")
  P
}

#' CTM class shares of a cell -> the class shares HCTM would have mapped
#'
#' @param fracs Named list of SpatRaster, the share of each CTM class (all classes of `landuseCategoriesCTM()`, hedges included) in each cell.
#' @param P Matrix from `loadLanduseRecode()`.
#' @param toClasses Character. HCTM classes to return (default: all columns of `P` except "hedges" and "other").
#' @return Named list of SpatRaster, the expected share of each HCTM class: sum over the CTM classes j (and of "other" = what the CTM classes do not cover) of share(j) * P[j, k].
applyLanduseRecode <- function(fracs, P, toClasses = setdiff(colnames(P), c("hedges", "other"))) {
  stopifnot(all(names(fracs) %in% rownames(P)), all(toClasses %in% colnames(P)))
  src <- fracs
  src[["other"]] <- terra::clamp(1 - Reduce(`+`, fracs), 0, 1)
  stats::setNames(lapply(toClasses, function(k) {
    out <- Reduce(`+`, lapply(names(src), function(j) P[j, k] * src[[j]])); names(out) <- k; out
  }), toClasses)
}

#' Stamp of the land-use file a derived layer (heterogeneity, unmapped share) is computed from
#'
#' Derived layers cache on the DEFINITION of the index only, so a change in the land-use inputs (here: the 2026-11-03 rule HCTM first + recoded CTM) would silently leave them stale.
#' The stamp is the md5 of the `<landuse file>.scheme.txt` written by `computeLanduse()` (source files and recode used); "legacy" for a file made before that record existed.
#' It is part of the version string of every derived file, so a file made from other land use is recomputed.
#'
#' @param scaleDir Character. Folder of the scale (e.g. `.../scale_1`). @param year Integer. @param scaleName Character. "habitat" or "landscape".
#' @return Character.
landuseSchemeStamp <- function(scaleDir, year, scaleName) {
  f <- file.path(scaleDir, paste0("landuse_", year, "_", scaleName, "_", basename(scaleDir), ".tif.scheme.txt"))
  if (file.exists(f)) unname(tools::md5sum(f)) else "legacy"
}
