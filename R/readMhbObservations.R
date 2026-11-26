#' Read the raw MhB (common breeding bird monitoring, CBBM) point counts, from the OLD or the NEW layout, always with the old column names (2026-11-26)
#'
#' Old layout (`dbird_observations_CBBM.csv`, the export used up to run v4): upper-case columns (SPECIES_NAME_SCIENTIFIC, SPECIES_NAME_GERMAN, DATE_TIME, TOTAL_COUNT, ATLAS_CODE, LONGITUDE, LATITUDE,
#' AREA_NATIONAL_CODE, ...). Returned unchanged (what the preparation read before this function existed).
#'
#' New layout (`observations_cbbm_flat.csv`, DDA/Dachverband export of 13 Nov 2026, cleaned and quality-assured): lower-case columns, one row per record of the 18 species of the export (species,
#' count, timing, visit_start, atlas_code, samplearea_code, longitude, latitude, protocol, protocol_season, visit_id, ...), no German name. It is mapped to the old names, so nothing after the read changes:
#'   SPECIES_NAME_SCIENTIFIC <- species        SPECIES_NAME_GERMAN <- `germanNames` (latin -> german; NA for species not in it)
#'   DATE_TIME <- LOCAL date (Europe/Berlin) of `timing` (the record's own time, UTC in the file), `visit_start` where it is missing     TOTAL_COUNT <- count     ATLAS_CODE <- atlas_code
#'   AREA_NATIONAL_CODE <- samplearea_code     LONGITUDE <- longitude     LATITUDE <- latitude     ORNITHO_LIST_ID <- visit_id     PROTOCOL_NAME <- protocol
#' Only rows of the CBBM protocol are kept. Absences are NOT taken from this file: the preparation derives "surveyed" routes from qualifying records of the roster species (see occurrencePrepGerHabitat()),
#' which the new file contains for all roster species, so the lack of the other species and of visits without records changes nothing.
#' @param path Character. The CSV. @param germanNames Named character vector (latin -> german) or NULL; used only for the new layout.
#' @return A data.frame with (at least) the old column names.
readMhbObservations <- function(path, germanNames = NULL) {
  hdr <- names(utils::read.csv(path, nrows = 1, header = TRUE, fileEncoding = "UTF-8"))
  if ("SPECIES_NAME_SCIENTIFIC" %in% hdr) return(utils::read.csv(path, header = TRUE, fileEncoding = "UTF-8"))      # old layout: exactly as before
  need <- c("species", "count", "timing", "visit_start", "atlas_code", "samplearea_code", "longitude", "latitude", "protocol", "visit_id")
  if (!all(need %in% hdr)) stop("readMhbObservations(): ", path, " is neither the old layout (SPECIES_NAME_SCIENTIFIC) nor the new one; missing columns: ", paste(setdiff(need, hdr), collapse = ", "))
  cc <- stats::setNames(rep("NULL", length(hdr)), hdr); cc[need] <- "character"; cc[c("count", "longitude", "latitude")] <- "numeric"
  x <- utils::read.csv(path, header = TRUE, fileEncoding = "UTF-8", colClasses = cc, na.strings = c("", "NA"))
  x <- x[x$protocol == "CBBM", , drop = FALSE]
  # The timestamps of the new export are UTC; the old DATE_TIME is the LOCAL date (a date-only record is local midnight = 22:00/23:00 UTC of the day before). So the date is taken in Europe/Berlin
  # (checked 2026-11-26 on 273,186 records present in both files: 99.991% identical dates; taking the first 10 characters instead gave 71.6%, one day early for date-only records).
  ts <- ifelse(!is.na(x$timing) & nzchar(x$timing), x$timing, x$visit_start)
  loc <- format(as.POSIXct(ts, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"), tz = "Europe/Berlin", format = "%Y-%m-%d")
  when <- ifelse(is.na(loc), substr(ts, 1, 10), loc)                      # a timestamp that cannot be parsed keeps its own first 10 characters
  data.frame(SPECIES_NAME_SCIENTIFIC = x$species,
             SPECIES_NAME_GERMAN = if (is.null(germanNames)) NA_character_ else unname(germanNames[x$species]),
             DATE_TIME = when, TOTAL_COUNT = x$count, ATLAS_CODE = x$atlas_code, AREA_NATIONAL_CODE = x$samplearea_code,
             LONGITUDE = x$longitude, LATITUDE = x$latitude, ORNITHO_LIST_ID = x$visit_id, PROTOCOL_NAME = x$protocol, stringsAsFactors = FALSE)
}
