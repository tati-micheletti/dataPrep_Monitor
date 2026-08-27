#' Raw CORINE Land Cover filename for a given snapshot year
#'
#' @param corineYear Integer. One of 2006, 2012, 2018.
#' @return Character, the expected raw raster filename for that snapshot.
corineRawFilename <- function(corineYear) {
  files <- list("2006" = "U2012_CLC2006_V2020_20u1.tif",
                "2012" = "U2018_CLC2012_V2020_20u1_raster100m.tif",
                "2018" = "U2018_CLC2018_V2020_20u1.tif")
  files[[as.character(corineYear)]]
}
