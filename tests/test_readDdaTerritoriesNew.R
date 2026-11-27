# Test of readDdaTerritoriesNew() (2026-11-26; from the repo root: Rscript modules/dataPrep_Monitor/tests/test_readDdaTerritoriesNew.R)
source("modules/dataPrep_Monitor/R/readDdaTerritoriesNew.R")
chk <- function(label, ok) { cat(if (isTRUE(ok)) "ok   " else "FAIL ", label, "\n"); if (!isTRUE(ok)) quit(status = 1) }
d <- tempfile(); dir.create(d)
x <- data.frame(id = 1:9, last_change = "2026-11-10", species = c("Alauda arvensis", "Alauda arvensis", "Alauda arvensis", "Sturnus vulgaris", "Alauda arvensis", "Alauda arvensis", "Turdus merula", "Alauda arvensis", "Alauda arvensis"),
                count = c(1, 2, 1, 1, 1, 1, 1, 0, 1), area = "WKBPLACEHOLDER", samplearea_code = c("BB1", "BB1", "bw2", "BB1", "BB1", "bw9", "BB1", "bw2", "bw3"),
                protocol = c("CBBM", "CBBM", "CBBM", "CBBM", "CBBM", "CBBM", "CBBM", "CBBM", "OTHER"), protocol_season = c(2025, 2025, 2025, 2025, 2026, 2024, 2025, 2025, 2025))
write.csv(x, f <- file.path(d, "t.csv"), row.names = FALSE, fileEncoding = "UTF-8")
gn <- c("Alauda arvensis" = "Feldlerche", "Sturnus vulgaris" = "Star")
r <- suppressMessages(readDdaTerritoriesNew(f, gn, lastOldYear = 2024))
t <- r$territories
chk("columns the preparation uses", all(c("ROUTENCODE", "Countdate", "Reviere", "Art deutsch") %in% names(t)))
chk("Reviere = sum of count per route x species x season (BB1 Feldlerche 2025: 1 + 2 = 3)", t$Reviere[t$ROUTENCODE == "BB1" & t$`Art deutsch` == "Feldlerche" & format(t$Countdate, "%Y") == "2025"] == 3)
chk("a season the old tables have (2024) is dropped", !any(format(t$Countdate, "%Y") == "2024"))
chk("only the CBBM protocol, count > 0, species of the lookup", nrow(t) == 4 && !any(t$ROUTENCODE == "bw3") && !any(t$`Art deutsch` == "Amsel"))
chk("the year is read from Countdate as the preparation does (format %Y)", setequal(format(t$Countdate, "%Y"), c("2025", "2026")))
chk("covered species and seasons", identical(r$speciesCovered, c("Alauda arvensis", "Sturnus vulgaris")) && identical(r$years, c(2025L, 2026L)))
v <- r$visits
chk("visits = distinct route-years (proxy), Jahr numeric as in the old table", is.numeric(v$Jahr) && nrow(v) == 3 && all(c("ROUTENCODE", "Jahr", "Countdate") %in% names(v)))
chk("route codes are kept as given (the preparation lower-cases them)", setequal(v$ROUTENCODE, c("BB1", "bw2")))
e <- suppressMessages(readDdaTerritoriesNew(f, gn, lastOldYear = 2030)); chk("nothing after the last old year: empty tables, no error", nrow(e$territories) == 0 && nrow(e$visits) == 0 && length(e$speciesCovered) == 0)
write.csv(x[, c("id", "species", "count")], fb <- file.path(d, "bad.csv"), row.names = FALSE); rr <- tryCatch(readDdaTerritoriesNew(fb, gn, 2024), error = function(e) "error"); chk("a wrong layout is an error naming the columns", identical(rr, "error"))
# --- 2026-11-27: surveyed = sample areas with >= 4 CBBM visits (MhB team), from visits.csv / survey_units.csv / sampleareas.csv
dd <- file.path(d, "dbird"); dir.create(dd); file.copy(f, file.path(dd, "territories_cbbm.csv"))
write.csv(data.frame(id = c("sa1", "sa2", "sa3", "sa4"), code = c("BB1", "bw2", "zz9", "bw3")), file.path(dd, "sampleareas.csv"), row.names = FALSE)
write.csv(data.frame(id = c("u1", "u2", "u3", "u4"), samplearea_id = c("sa1", "sa2", "sa3", "sa4")), file.path(dd, "survey_units.csv"), row.names = FALSE)
nv <- c(u1 = 4, u2 = 3, u3 = 4, u4 = 4); vr <- do.call(rbind, lapply(names(nv), function(u) data.frame(id = paste0(u, "_v", seq_len(nv[[u]])), protocol = "CBBM", protocol_season = 2025, survey_unit_id = u)))
vr <- rbind(vr, data.frame(id = paste0("b", 1:5), protocol = "CBBM", protocol_season = 2026, survey_unit_id = "u1"),        # BB1 2026: 5 visits (counts as complete)
            data.frame(id = "o1", protocol = "OTHER", protocol_season = 2025, survey_unit_id = "u2"),                       # another protocol: not counted
            data.frame(id = "o2", protocol = "CBBM", protocol_season = 2024, survey_unit_id = "u2"))                        # a season the old tables have: not used
write.csv(vr, file.path(dd, "visits.csv"), row.names = FALSE)
vc <- readDbirdVisitCounts(dd, 2025:2026)
chk("visit counts per code and season (CBBM only, the asked seasons)", identical(vc$visits[vc$code == "bw2" & vc$season == 2025], 3L) && identical(vc$visits[vc$code == "BB1" & vc$season == 2026], 5L) && !any(vc$season == 2024))
m <- suppressMessages(readDdaTerritoriesNew(file.path(dd, "territories_cbbm.csv"), gn, lastOldYear = 2024, visitsDir = dd, minVisits = 4L))
chk("surveyed = plots with >= 4 visits, incl. a plot without any territory (zeros) and a 5-visit plot", setequal(paste(tolower(m$visits$ROUTENCODE), m$visits$Jahr), c("bb1 2025", "zz9 2025", "bw3 2025", "bb1 2026")))
chk("territories of a plot with 3 visits (bw2, 2025) are dropped with the plot", !any(tolower(m$territories$ROUTENCODE) == "bw2") && all(tolower(m$territories$ROUTENCODE) == "bb1") && nrow(m$territories) == 3)
chk("covered species stay those of the file", identical(m$speciesCovered, c("Alauda arvensis", "Sturnus vulgaris")))
p <- suppressMessages(readDdaTerritoriesNew(file.path(dd, "territories_cbbm.csv"), gn, lastOldYear = 2024)); chk("without visitsDir/minVisits: the old proxy, unchanged", nrow(p$visits) == 3 && nrow(p$territories) == 4)
cat("all passed\n")
