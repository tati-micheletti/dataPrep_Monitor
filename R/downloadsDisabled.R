#' Are data downloads switched off for this run?
#'
#' TRUE when env var `BIRDMONITOR_SKIP_DOWNLOAD=1`. Used on EVE, where compute
#' nodes have throttled internet and there is no Python virtualenv or CLMS token:
#' every `download*()` wrapper then returns immediately, and the `compute*()`/
#' `process*()` steps work from whatever is already under `inputs/predictors/`.
#' Unset (default): downloads behave exactly as before.
#'
#' @return Logical.
downloadsDisabled <- function() {
  identical(Sys.getenv("BIRDMONITOR_SKIP_DOWNLOAD"), "1")
}
