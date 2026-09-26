#' German to Latin species name lookup table
#'
#' @return data.frame with columns `german` and `latin`.
speciesLookup <- function() {
  data.frame(
    german = c("Feldlerche", "Goldammer", "Neuntöter", "Heidelerche",
               "Grauammer", "Rotmilan", "Braunkehlchen", "Kiebitz",
               "Mäusebussard", "Star", "Rebhuhn", "Wiesenpieper"),
    latin = c("Alauda arvensis", "Emberiza citrinella",
              "Lanius collurio", "Lullula arborea",
              "Emberiza calandra", "Milvus milvus",
              "Saxicola rubetra", "Vanellus vanellus",
              "Buteo buteo", "Sturnus vulgaris",
              "Perdix perdix", "Anthus pratensis"),
    stringsAsFactors = FALSE
  )
}
