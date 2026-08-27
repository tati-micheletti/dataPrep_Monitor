isValidRDSFile <- function(path) {
  if (!file.exists(path)) return(FALSE)
  tryCatch({
    x <- readRDS(path)
    nrow(x) > 0
  }, error = function(e) FALSE)
}
