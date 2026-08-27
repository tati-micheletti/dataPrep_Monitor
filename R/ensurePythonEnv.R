#' Ensure a dedicated Python virtualenv exists and is active
#'
#' Creates (once) and activates a reticulate virtualenv provisioned from
#' this module's `python/requirements.txt`, so the Python download
#' scripts run in a self-contained, reproducible environment without
#' requiring any manual setup.
#'
#' @param envName Character. Name of the virtualenv.
#' @param requirementsPath Character. Path to a pip requirements.txt file.
#' @return Invisibly, `envName`.
ensurePythonEnv <- function(envName, requirementsPath) {
  if (!reticulate::virtualenv_exists(envName)) {
    message("Creating Python virtualenv '", envName, "' from ", requirementsPath, "...")
    reticulate::virtualenv_create(envName, requirements = requirementsPath)
  }
  reticulate::use_virtualenv(envName, required = TRUE)
  invisible(envName)
}
