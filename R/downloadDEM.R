#' Download Copernicus GLO-30 DEM tiles covering a bounding box
#'
#' Wraps `python/download_dem.py` (Copernicus GLO-30 Public DEM on AWS
#' S3, no authentication needed) via reticulate, provisioning a
#' dedicated virtualenv from `python/requirements.txt` on first use.
#'
#' @param demRawDir Character. Directory to save downloaded tiles in.
#' @param bboxVec Numeric vector `c(N, W, S, E)` in WGS84.
#' @param pythonScriptPath Character. Path to `download_dem.py`.
#' @param requirementsPath Character. Path to `requirements.txt`.
#' @param envName Character. Name of the reticulate virtualenv to use.
#' @return Invisibly, `demRawDir`.
downloadDEM <- function(demRawDir, bboxVec, pythonScriptPath, requirementsPath,
                         envName = "dataPrep_Monitor_env") {

  ensurePythonEnv(envName, requirementsPath)

  dir.create(demRawDir, recursive = TRUE, showWarnings = FALSE)

  setPyEnv(list(DEM_OUT_DIR = demRawDir,
                DEM_BBOX_N = bboxVec[1],
                DEM_BBOX_W = bboxVec[2],
                DEM_BBOX_S = bboxVec[3],
                DEM_BBOX_E = bboxVec[4]))

  message("Downloading DEM tiles to ", demRawDir, "...")
  reticulate::py_run_file(pythonScriptPath)

  invisible(demRawDir)
}
