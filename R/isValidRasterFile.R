isValidRasterFile <- function(path) {
  if (!file.exists(path)) return(FALSE)
  tryCatch({
    r <- terra::rast(path)
    terra::nlyr(r) > 0
  }, error = function(e) {
    message("  Corrupted file, will recreate: ", path)
    FALSE
  })
}
