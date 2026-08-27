#' Land use category codes for the HCTM dataset
#'
#' Tetteh et al. 2026 (HCTM v101: 1990-2023). NOTE: grassland (200) is
#' already undifferentiated in HCTM. Hedges are not present in HCTM and
#' are handled separately (filled with NA). Horticultural crops assumed
#' equivalent to vegetables; plantation assumed equivalent to
#' orchards_and_berries (assumptions flagged to dataset authors, pending
#' response).
#'
#' @return Named list mapping output category name to raw code(s).
landuseCategoriesHCTM <- function() {
  list(grassland = 200,
       winter_cereals = 110,
       summer_cereals = 120,
       maize = 130,
       root_crops = c(1401, 1402),
       rapeseed = 1501,
       sunflower = 1502,
       vegetables = 1603,
       legumes = 1601,
       fallow = 3003,
       grapevine = 4001,
       hops = 4002,
       orchards_and_berries = 4003)
}
