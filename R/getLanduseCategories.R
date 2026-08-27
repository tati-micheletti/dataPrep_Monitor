#' Determine which land use category scheme a raw file uses
#'
#' @param filename Character. Basename of the raw land use raster.
#' @return List with `cats` (the category list) and `has_hedges` (logical).
getLanduseCategories <- function(filename) {
  if (grepl("HCTM_GER", filename)) {
    return(list(cats = landuseCategoriesHCTM(), has_hedges = FALSE))
  }
  list(cats = landuseCategoriesCTM(), has_hedges = TRUE)
}
