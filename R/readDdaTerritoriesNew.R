#' Read the NEW DDA territory export (territories_cbbm.csv, 13 Nov 2026) as the two tables the landscape preparation reads from the old xlsx files (2026-11-26)
#'
#' The old input is two xlsx tables: territories (one row per route x species x year: `Reviere` = number of territories, `Art deutsch`, ROUTENCODE, Countdate) and visits (the route-years that were mapped;
#' every route-year listed there and without a territory row of a species is an ABSENCE of that species). The new export has one row per territory polygon (`count` territories each, mostly 1) for the
#' seasons after the old tables end (2025, 2026), for 10 species only. It is converted to the same two tables:
#'   territories: Reviere = sum of `count` per sample area x species x season (checked 2026-11-26: the mean per route is the same as in the old table, e.g. Goldammer 5.62 against 5.64, Feldlerche 9.5 against 10.1)
#'   visits: the route-years that have at least one territory of any species in the file (a PROXY: the exact list of mapped route-years of 2025-2026 is not in the delivery. Tested on 2018-2024 against the real
#'           list: every proxy route-year is a real one (0 false), but the proxy covers only ~94% of them, the plots without any of the 10 species are missing, so a few real absences are lost.
#'           It is replaced by the real list as soon as the DDA delivers it.)
#' Only the protocol CBBM and seasons AFTER `lastOldYear` are kept (a season that the old tables already have is taken from them). The German name comes from `germanNames` (latin -> german, the roster names,
#' folded for umlauts later exactly as for the old table); species of the file that are not in `germanNames` are dropped.
#' @param path Character. territories_cbbm.csv. @param germanNames Named character vector latin -> german. @param lastOldYear Integer. Last year of the old tables.
#' @return A list: `territories` (data.frame: ROUTENCODE, Countdate, Reviere, `Art deutsch`), `visits` (ROUTENCODE, Jahr, Countdate), `speciesCovered` (latin names with data), `years` (seasons).
readDdaTerritoriesNew <- function(path, germanNames, lastOldYear) {
  hdr <- names(utils::read.csv(path, nrows = 1, header = TRUE, fileEncoding = "UTF-8"))
  need <- c("species", "count", "samplearea_code", "protocol", "protocol_season")
  if (!all(need %in% hdr)) stop("readDdaTerritoriesNew(): ", path, " lacks column(s): ", paste(setdiff(need, hdr), collapse = ", "))
  cc <- stats::setNames(rep("NULL", length(hdr)), hdr); cc[need] <- c("character", "numeric", "character", "character", "integer")          # the polygon column is large and not needed
  x <- utils::read.csv(path, header = TRUE, colClasses = cc, fileEncoding = "UTF-8")
  x <- x[x$protocol == "CBBM" & !is.na(x$protocol_season) & x$protocol_season > lastOldYear & !is.na(x$count) & x$count > 0 & !is.na(x$samplearea_code) & nzchar(x$samplearea_code), , drop = FALSE]
  unknown <- setdiff(unique(x$species), names(germanNames))
  if (length(unknown)) message("readDdaTerritoriesNew(): species of the file that are not in the roster lookup are dropped: ", paste(unknown, collapse = ", "))
  x <- x[x$species %in% names(germanNames), , drop = FALSE]
  if (!nrow(x)) return(list(territories = data.frame(ROUTENCODE = character(), Countdate = as.POSIXct(character(), tz = "UTC"), Reviere = numeric(), `Art deutsch` = character(), check.names = FALSE),
                            visits = data.frame(ROUTENCODE = character(), Jahr = numeric(), Countdate = as.POSIXct(character(), tz = "UTC")), speciesCovered = character(), years = integer()))
  agg <- stats::aggregate(count ~ samplearea_code + species + protocol_season, data = x, FUN = sum)
  date <- as.POSIXct(paste0(agg$protocol_season, "-01-01"), tz = "UTC")
  territories <- data.frame(ROUTENCODE = agg$samplearea_code, Countdate = date, Reviere = agg$count, `Art deutsch` = unname(germanNames[agg$species]), check.names = FALSE, stringsAsFactors = FALSE)
  rv <- unique(data.frame(ROUTENCODE = agg$samplearea_code, Jahr = as.numeric(agg$protocol_season), stringsAsFactors = FALSE))
  rv$Countdate <- as.POSIXct(paste0(rv$Jahr, "-01-01"), tz = "UTC")
  list(territories = territories, visits = rv, speciesCovered = sort(unique(agg$species)), years = sort(unique(agg$protocol_season)))
}
