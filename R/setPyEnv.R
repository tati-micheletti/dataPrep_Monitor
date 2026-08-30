#' Set environment variables visible to BOTH R and an already-running
#' embedded Python session
#'
#' `reticulate`'s Python is initialized once per R session, and Python's
#' `os.environ` is a snapshot cached at that point -- `Sys.setenv()`
#' calls made AFTER Python has already been touched by an earlier
#' `py_run_file()` do NOT propagate into it (a real, verified Python/
#' reticulate embedding gotcha, not a hypothetical). Concretely: if
#' `downloadDEM()` runs first and initializes Python, then
#' `downloadLanduse()`'s `Sys.setenv(LANDUSE_OUT_DIR = ...)` later in
#' the same R session is invisible to Python, and `os.environ["LANDUSE_OUT_DIR"]`
#' raises `KeyError` even though `Sys.getenv("LANDUSE_OUT_DIR")` in R is
#' correct.
#'
#' This sets the variable in R's own environment (harmless, kept for
#' consistency/any subprocess use) AND pushes it directly into Python's
#' `os.environ` dict via reticulate, so it works regardless of whether
#' this is the first or a later `py_run_file()` call in the session.
#'
#' @param vars Named list of environment variables to set (values
#'   coerced to character).
#' @return Invisibly, NULL.
setPyEnv <- function(vars) {
  vars <- lapply(vars, as.character)
  do.call(Sys.setenv, vars)
  pyOs <- reticulate::import("os")
  pyOs$environ$update(vars)
  invisible(NULL)
}
