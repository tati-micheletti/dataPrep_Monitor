#' Compute a proportion-of-category layer at one target resolution
#'
#' Shared by land use and land cover processing: binary mask for pixels
#' matching `codes`, aggregated to `targetRes` by mean (= proportion).
#'
#' @param categoricalMap SpatRaster, a reprojected categorical raster.
#' @param codes Numeric vector of raw category codes to include.
#' @param targetRes Numeric. Target resolution in metres.
#' @param catName Character. Output layer name.
#' @param targetCRS Character. Output CRS, e.g. "EPSG:3035".
#' @return SpatRaster, one proportion layer named `catName`.
makeCategoryProportionLayer <- function(categoricalMap, codes, targetRes, catName, targetCRS) {
  binary <- terra::app(categoricalMap, function(x) as.integer(x %in% codes))
  fact <- round(targetRes / terra::res(categoricalMap)[1])
  prop <- terra::aggregate(binary, fact = fact, fun = "mean", na.rm = TRUE)
  prop <- terra::project(prop, targetCRS, res = targetRes, method = "bilinear")
  names(prop) <- catName
  prop
}
