#' The crop type map (CTM) file of one year, found by pattern instead of by a file name written into the code (2026-11-25)
#'
#' The Thuenen Institute (ThEO) names its maps `CTM_GER_<year>_rst_v<version>_COG.tif`. A map of a year that is not finished yet carries the last month it used,
#' `CTM_GER_<year>_rst_v<version>_<year>_<month>_COG.tif` (2025: `..._2025_08_...`, 2026: `..._2026_09_...`), and gets the plain name when the year is complete.
#' A file name written into the code therefore breaks twice a year. This function takes every file of the year in `dir` and returns the best one:
#' the highest version first, and within one version the finished map (no month in the name) before an in-season map, and of two in-season maps the later month.
#'
#' @param year Integer. @param dir Character. Folder with the CTM files.
#' @return A list: `file` (path, NA if there is none), `version` (integer, e.g. 302), `inSeasonMonth` (integer month, NA for a finished map), `status` ("final", "in-season" or NA).
findCtmFile <- function(year, dir) {
  none <- list(file = NA_character_, version = NA_integer_, inSeasonMonth = NA_integer_, status = NA_character_)
  if (!dir.exists(dir)) return(none)
  fl <- list.files(dir, pattern = sprintf("^CTM_GER_%d_rst_v[0-9]+(_%d_[0-9]{2})?_COG\\.tif$", year, year))
  if (!length(fl)) return(none)
  ver <- as.integer(sub(sprintf("^CTM_GER_%d_rst_v([0-9]+).*$", year), "\\1", fl))
  mon <- suppressWarnings(as.integer(sub(sprintf("^CTM_GER_%d_rst_v[0-9]+_%d_([0-9]{2})_COG\\.tif$", year, year), "\\1", fl)))
  mon[!grepl(sprintf("_%d_[0-9]{2}_COG", year), fl)] <- NA_integer_
  # rank: version, then finished (no month) before in-season, then the later month
  o <- order(-ver, !is.na(mon), -ifelse(is.na(mon), 0L, mon))
  i <- o[1]
  list(file = file.path(dir, fl[i]), version = ver[i], inSeasonMonth = mon[i], status = if (is.na(mon[i])) "final" else "in-season")
}
