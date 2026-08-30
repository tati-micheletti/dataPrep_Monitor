#' Download CORINE Land Cover (2006/2012/2018) via the CLMS Download API
#'
#' Wraps `python/download_landcover.py` via reticulate, provisioning a
#' dedicated virtualenv from `python/requirements.txt` on first use.
#'
#' Requires a personal CLMS API token JSON file at `tokenJSONPath` --
#' see the setup instructions at the top of `python/download_landcover.py`
#' (you must create a free account at land.copernicus.eu yourself and
#' generate an API token from your profile; this cannot be automated).
#'
#' @param landcoverRawDir Character. Directory to save downloaded GeoTIFFs in.
#' @param bboxVec Numeric vector `c(N, W, S, E)` in WGS84.
#' @param tokenJSONPath Character. Path to your CLMS API token JSON file.
#' @param pythonScriptPath Character. Path to `download_landcover.py`.
#' @param requirementsPath Character. Path to `requirements.txt`.
#' @param envName Character. Name of the reticulate virtualenv to use.
#' @return Invisibly, `landcoverRawDir`.
downloadLandcover <- function(landcoverRawDir, bboxVec, tokenJSONPath,
                               pythonScriptPath, requirementsPath,
                               envName = "dataPrep_Monitor_env") {

  if (!file.exists(tokenJSONPath)) {
    stop("CLMS API token file not found: ", tokenJSONPath, "\n",
         "Create a free account at https://land.copernicus.eu, generate an API ",
         "token from your profile (\"API Tokens\" -> \"Create new Token\"), and ",
         "save the client_id/private_key/user_id/token_uri it shows you (once) as ",
         "a JSON file at this path. See python/download_landcover.py for the exact format.")
  }

  ensurePythonEnv(envName, requirementsPath)

  dir.create(landcoverRawDir, recursive = TRUE, showWarnings = FALSE)

  setPyEnv(list(CLMS_TOKEN_JSON = tokenJSONPath,
                LANDCOVER_OUT_DIR = landcoverRawDir,
                LANDCOVER_BBOX_N = bboxVec[1],
                LANDCOVER_BBOX_W = bboxVec[2],
                LANDCOVER_BBOX_S = bboxVec[3],
                LANDCOVER_BBOX_E = bboxVec[4]))

  message("Downloading CORINE Land Cover snapshots to ", landcoverRawDir, "...")
  reticulate::py_run_file(pythonScriptPath)

  invisible(landcoverRawDir)
}
