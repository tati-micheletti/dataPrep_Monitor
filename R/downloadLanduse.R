#' Download crop type (land use) maps from Zenodo
#'
#' Wraps `python/download_landuse.py` via reticulate, provisioning a
#' dedicated virtualenv from `python/requirements.txt` on first use.
#'
#' NOTE: the Tetteh et al. 2026 (HCTM, 1990-2023) dataset is distributed
#' as a single 14.7GB zip that cannot be selectively fetched via the
#' Zenodo API -- it must be downloaded manually from
#' https://doi.org/10.5281/zenodo.20815677 and its `LU_Maps/` contents
#' placed in `landuseRawDir` before `computeLanduse()` will find them.
#'
#' @param landuseRawDir Character. Directory to save downloaded files in.
#' @param pythonScriptPath Character. Path to `download_landuse.py`.
#' @param requirementsPath Character. Path to `requirements.txt`.
#' @param envName Character. Name of the reticulate virtualenv to use.
#' @return Invisibly, `landuseRawDir`.
downloadLanduse <- function(landuseRawDir, pythonScriptPath, requirementsPath,
                             envName = "dataPrep_Monitor_env") {

  ensurePythonEnv(envName, requirementsPath)

  dir.create(landuseRawDir, recursive = TRUE, showWarnings = FALSE)

  Sys.setenv(LANDUSE_OUT_DIR = landuseRawDir)

  message("Downloading land use maps to ", landuseRawDir, "...")
  reticulate::py_run_file(pythonScriptPath)

  invisible(landuseRawDir)
}
