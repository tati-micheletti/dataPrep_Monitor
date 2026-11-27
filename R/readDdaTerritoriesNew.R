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
#'
#' 2026-11-27, MhB team (Sven, via Lukas Seemann/DDA): for the PRELIMINARY seasons ONLY sample areas with the full 4 visits count as surveyed, because Autoterri only generates territories for sample areas
#' with all 4 visits. With `visitsDir` (the dbird folder with visits.csv, survey_units.csv, sampleareas.csv) and `minVisits` (4) the surveyed list is built that way: sample areas with >= `minVisits` CBBM visits
#' in the season (Lukas's query, `visit_count`), and the territories of sample areas with fewer visits are dropped with them (a plot is either surveyed or not). The final MhB results (Visits Excel) follow other,
#' partly manual rules (some 4-visit plots are excluded, some with fewer are included, the first survey year of a new sample area is never included): this is the best approximation, not the final list.
#' Without `visitsDir`/`minVisits` the old proxy above is used.
#' @param path Character. territories_cbbm.csv. @param germanNames Named character vector latin -> german. @param lastOldYear Integer. Last year of the old tables.
#' @param visitsDir Character or NULL. Folder with visits.csv, survey_units.csv, sampleareas.csv (default NULL = proxy). @param minVisits Integer or NA. Minimum CBBM visits per sample area and season (4).
#' @return A list: `territories` (data.frame: ROUTENCODE, Countdate, Reviere, `Art deutsch`), `visits` (ROUTENCODE, Jahr, Countdate), `speciesCovered` (latin names with data), `years` (seasons).
readDdaTerritoriesNew <- function(path, germanNames, lastOldYear, visitsDir = NULL, minVisits = NA_integer_) {
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
  if (!is.null(visitsDir) && !is.na(minVisits)) {
    vc <- readDbirdVisitCounts(visitsDir, seasons = sort(unique(agg$protocol_season)))
    ok <- vc[vc$visits >= minVisits, , drop = FALSE]
    rv <- data.frame(ROUTENCODE = ok$code, Jahr = as.numeric(ok$season), stringsAsFactors = FALSE)
    key <- function(code, yr) paste(tolower(code), yr)
    tkey <- key(territories$ROUTENCODE, format(territories$Countdate, "%Y"))
    keep <- tkey %in% key(rv$ROUTENCODE, rv$Jahr)
    message("readDdaTerritoriesNew(): surveyed = sample areas with >= ", minVisits, " CBBM visits: ",
            paste(vapply(sort(unique(vc$season)), function(s) paste0(s, ": ", sum(ok$season == s), " of ", sum(vc$season == s), " visited plots"), ""), collapse = "; "),
            "; territory rows dropped with their plots (fewer visits): ", sum(!keep), " of ", length(keep), " (", length(unique(tkey[!keep])), " plot-seasons)")
    territories <- territories[keep, , drop = FALSE]
  }
  rv$Countdate <- as.POSIXct(paste0(rv$Jahr, "-01-01"), tz = "UTC")
  list(territories = territories, visits = rv, speciesCovered = sort(unique(agg$species)), years = sort(unique(agg$protocol_season)))
}

#' CBBM visits per sample area and season from the dbird tables (the query Lukas Seemann gave, 2026-11-26): `sampleareas` JOIN `survey_units` JOIN `visits` WHERE v.protocol = 'CBBM',
#' counted per sample-area code and protocol_season. Our reproduction equals his visits_cbbm_2025_2026.csv (1,471 plots / 5,454 visits in 2025; 1,354 / 4,996 in 2026).
#' @param dir Character. Folder with visits.csv, survey_units.csv, sampleareas.csv. @param seasons Integer vector of seasons to keep.
#' @return data.frame(code, season, visits).
readDbirdVisitCounts <- function(dir, seasons) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("readDbirdVisitCounts(): the data.table package is needed (visits.csv is ~1 GB)")
  f <- file.path(dir, c("visits.csv", "survey_units.csv", "sampleareas.csv")); if (!all(file.exists(f))) stop("readDbirdVisitCounts(): missing ", paste(f[!file.exists(f)], collapse = ", "))
  v  <- data.table::fread(f[1], select = c("id", "protocol", "protocol_season", "survey_unit_id"), colClasses = list(character = c("id", "protocol", "survey_unit_id")))
  su <- data.table::fread(f[2], select = c("id", "samplearea_id"), colClasses = list(character = c("id", "samplearea_id")))
  sa <- data.table::fread(f[3], select = c("id", "code"), colClasses = list(character = c("id", "code")))
  v <- v[v$protocol == "CBBM" & v$protocol_season %in% seasons, ]
  q <- merge(merge(v[, c("survey_unit_id", "protocol_season")], su, by.x = "survey_unit_id", by.y = "id"), sa, by.x = "samplearea_id", by.y = "id")
  r <- q[, list(visits = .N), by = list(code, season = protocol_season)]
  data.frame(code = r$code, season = as.integer(r$season), visits = as.integer(r$visits), stringsAsFactors = FALSE)
}
