#' Land use category codes for the CTM datasets
#'
#' Schwieder et al. 2024 (CTM v202: 2017-2021) and Tetteh et al. 2025a/b
#' (CTM v302: 2024-2025). NOTE: permanent_grassland (200) and
#' cultivated_grassland (1602) are collapsed into a single "grassland"
#' category for temporal consistency with Tetteh et al. 2026 (HCTM,
#' 1990-2023).
#'
#' @return Named list mapping output category name to raw code(s).
landuseCategoriesCTM <- function() {
  list(grassland = c(200, 1602),
       winter_cereals = c(1101, 1102, 1103),
       summer_cereals = c(1201, 1202),
       maize = 1300,
       root_crops = c(1401, 1402),
       rapeseed = 1501,
       sunflower = 1502,
       vegetables = 1603,
       legumes = c(1611, 1612, 1613, 1614),
       hedges = c(3001, 3011),
       fallow = 3003,
       grapevine = 4001,
       hops = 4002,
       orchards_and_berries = 4003)
}
