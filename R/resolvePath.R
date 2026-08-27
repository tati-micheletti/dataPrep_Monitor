#' Resolve a path that may be absolute or relative to a base directory
#'
#' Used for parameters like credential file locations, where an absolute
#' path (e.g. outside any git-tracked project, such as the user's home
#' directory) should be usable directly instead of being forced under
#' `dataPath(sim)`.
#'
#' @param basePath Character. Base directory used when `pathOrSubpath` is relative.
#' @param pathOrSubpath Character. Either an absolute path, or a path
#'   relative to `basePath`.
#' @return Character, the resolved absolute (or base-relative) path.
resolvePath <- function(basePath, pathOrSubpath) {
  isAbsolute <- grepl("^(/|[A-Za-z]:[\\\\/]|\\\\\\\\)", pathOrSubpath)
  if (isAbsolute) pathOrSubpath else file.path(basePath, pathOrSubpath)
}
