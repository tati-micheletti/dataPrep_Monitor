#' One-line log of how coordinate transformations behave RIGHT NOW in this R session
#'
#' On EVE the full SpaDES session returned x/y swapped from `st_transform(., 3035)` although a plain session
#' did not (DECISIONS.md, 2026-10-05). Calling this at the start of each data-prep event shows in which step
#' it starts, and which global GDAL/PROJ settings are active. Cheap: one transformation of one point.
#'
#' @param label Character. Where it is called from.
#' @return Invisibly NULL.
reportAxisState <- function(label) {
  tryCatch({
    p <- sf::st_as_sf(data.frame(lon = 11.5, lat = 48.1), coords = c("lon", "lat"), crs = 4326)
    xy <- sf::st_coordinates(sf::st_transform(p, 3035))
    cfg <- tryCatch(paste(unlist(terra::getGDALconfig("OSR_DEFAULT_AXIS_MAPPING_STRATEGY")), collapse = ""),
                    error = function(e) "?")
    net <- tryCatch(as.character(sf::sf_proj_network()), error = function(e) "?")
    message("[axis] ", label, ": (11.5E, 48.1N) -> EPSG:3035 x=", round(xy[1, 1]), " y=", round(xy[1, 2]),
            if (xy[1, 1] < xy[1, 2]) "  <<< SWAPPED" else "  (ok)",
            " | GDAL axis-strategy config='", cfg, "' | PROJ network=", net)
  }, error = function(e) message("[axis] ", label, ": check failed: ", conditionMessage(e)))
  invisible(NULL)
}
