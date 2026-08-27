#' CORINE Level 1-3 code groupings used for land cover proportion layers
#'
#' Three output categories derived from CORINE codes (sequential integers
#' 1-44):
#'   built_up: codes 1-11 (all artificial surfaces, CORINE Level 1 class 1
#'     -- urban fabric, industrial, transport, construction, green urban
#'     areas (10) and sport/leisure facilities (11); all anthropogenic
#'     regardless of vegetation cover)
#'   trees: codes 23-25 (closed-canopy forest only, CORINE Level 2 class
#'     3.1 -- broad-leaved, coniferous, mixed; excludes scrub, heathland,
#'     transitional woodland 26-29)
#'   water: codes 40-41 (inland water courses/bodies only, CORINE Level 2
#'     class 5.1; excludes coastal/marine water 42-44)
#'
#' @return Named list mapping output category name to raw CORINE code(s).
landcoverCategoriesCorine <- function() {
  list(built_up = 1:11,
       trees = c(23, 24, 25),
       water = c(40, 41))
}
