#' Version of the occurrence-table "wiring", folded into every occurrence Cache() key
#'
#' `Cache()` keys on the function's code and its arguments. It cannot see changes that live OUTSIDE
#' those, such as a new covariate being offered, the habitat/landscape years rule, or how the
#' covariate stack is assembled. Bump this number whenever such wiring changes in a way that makes old
#' cached occurrence tables wrong, and every occurrence table is recomputed on the next run. Passed
#' as `.cacheExtra` in occurrencePrepEurope/GerHabitat/GerLandscape.
#'
#' History: 1 = original; 2 = 2026-10-05, after the EVE move (new covariates, per-species years).
#'
#' @format Integer scalar.
occurrenceCacheSchema <- 2L
