#' Climate of a year after the last year with CHELSA data: an explicit, flagged copy of the last real layer (2026-11-25)
#'
#' CHELSA daily data end on 30 Sep 2025, so the 2026 window has no new data. Averaging "what exists" would quietly turn the 2026 layer into a 2021-2025 mean.
#' Rule decided by the project lead (2026-11-25): a target year AFTER `climateLastYear` uses the layer of `climateLastYear` itself (window 2020-2025 for
#' climateLastYear = 2025), written under the file name of the target year (`bioclim_2021-2026_<scale>.tif`) so that every consumer finds the file it expects,
#' with a note next to it (`<file>.carriedForward.txt`) that says what it is. The note is also what the manifest/README/Methods flag is built from.
#' When `climateLastYear` is raised (CHELSA resumes) the stale copy is deleted here and the year is computed from data again.
#'
#' @param targetYear Integer. @param climateLastYear Integer or NA (NA = no limit: nothing is carried forward, nothing changes).
#' @param windowLength Integer. @param climateOutputDir Character. Folder of the bioclim files of this scale.
#' @return A list: `carried` (TRUE if `targetYear` is carried forward), `file` (the layer of the target year), `source` (the layer it is a copy of), `note` (the note file),
#'   `removedStale` (TRUE if an old carried-forward copy was deleted because real data exist now).
bioclimFileName <- function(year, windowLength, climateOutputDir) {
  file.path(climateOutputDir, paste0("bioclim_", year - (windowLength - 1), "-", year, "_", basename(climateOutputDir), ".tif"))
}

carryForwardBioclim <- function(targetYear, climateLastYear, windowLength, climateOutputDir) {
  outFile <- bioclimFileName(targetYear, windowLength, climateOutputDir)
  note <- paste0(outFile, ".carriedForward.txt")
  res <- list(carried = FALSE, file = outFile, source = NA_character_, note = note, removedStale = FALSE)
  if (is.na(climateLastYear) || targetYear <= climateLastYear) {
    if (file.exists(note)) {                                        # a copy made earlier, but real data exist for this year now: recompute from data
      unlink(c(outFile, note)); res$removedStale <- TRUE
      message("Carried-forward climate layer of ", targetYear, " removed (climate data exist now): ", basename(outFile))
    }
    return(res)
  }
  res$carried <- TRUE; res$source <- bioclimFileName(climateLastYear, windowLength, climateOutputDir)
  res
}

#' Write the carried-forward copy (call after the source layer exists)
#' @param cf The list returned by carryForwardBioclim() with `carried` TRUE.
#' @param targetYear Integer. @param climateLastYear Integer.
writeCarriedForwardBioclim <- function(cf, targetYear, climateLastYear) {
  if (!file.exists(cf$source) || file.size(cf$source) == 0) stop("Source climate layer missing for the carry-forward: ", cf$source)
  if (file.exists(cf$file) && file.exists(cf$note)) return(invisible(cf$file))        # already carried forward
  ok <- file.copy(cf$source, cf$file, overwrite = TRUE); if (!ok) stop("Could not write ", cf$file)
  writeLines(c(paste0("CARRIED FORWARD: the climate layer of ", targetYear, " is a copy of the layer of ", climateLastYear, " (", basename(cf$source), ")."),
               paste0("Reason: no CHELSA data after ", climateLastYear, " (daily data end 30 Sep 2025). Rule decided 2026-11-25; the year is preliminary."),
               paste0("Replaced by a layer computed from data when climateLastYear is raised. Made: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))), cf$note)
  message("Climate ", targetYear, " carried forward from ", climateLastYear, ": ", basename(cf$file))
  invisible(cf$file)
}
