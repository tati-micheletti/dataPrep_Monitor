#' CORINE Level 1-3 code groupings used for land cover proportion layers
#'
#' Output categories derived from CORINE codes (sequential integers 1-44). Version 2 (2026-10-21) adds the semi-natural classes of Germany that the first three
#' categories and the crop map do not describe (measured 2026-10-20: 1.5% of the 200 m cells of Germany had less than 5% of their area mapped, 9.2% less than half;
#' 3% of the bird records sit in such cells). They are rare in Germany (each 0.26-0.63% of the area), which is why the rarest are grouped; THE GROUPING IS A DECISION FOR
#' GERMANY AND MUST BE REVISED when the study area grows (e.g. to Europe, where heath, scrub or wetlands are common and may deserve their own classes; DECISIONS.md 2026-10-21).
#'   built_up: codes 1-11 (all artificial surfaces, CORINE Level 1 class 1
#'     -- urban fabric, industrial, transport, construction, green urban
#'     areas (10) and sport/leisure facilities (11); all anthropogenic
#'     regardless of vegetation cover)
#'   trees: codes 23-25 (closed-canopy forest only, CORINE Level 2 class
#'     3.1 -- broad-leaved, coniferous, mixed; excludes scrub, heathland,
#'     transitional woodland 26-29)
#'   water: codes 40-41 (inland water courses/bodies only, CORINE Level 2
#'     class 5.1; excludes coastal/marine water 42-44)
#'   natural_grassland: code 26 (natural grasslands)
#'   heath_moor: code 27 (moors and heathland)
#'   scrub: code 29 (transitional woodland-shrub)
#'   wetland_peat: codes 35-38 (inland marshes, peat bogs, salt marshes, salines)
#' Everything else (bare ground, dunes, tidal flats, coastal waters, and the farmland cells where the crop map has no class) is the residual predictor `unmapped_other`
#' (1 - the sum of all mapped proportions; see `computeUnmappedShare()`).
#'
#' @return Named list mapping output category name to raw CORINE code(s).
landcoverCategoriesCorine <- function() {
  list(built_up = 1:11,
       trees = c(23, 24, 25),
       water = c(40, 41),
       natural_grassland = 26,
       heath_moor = 27,
       scrub = 29,
       wetland_peat = 35:38)
}
