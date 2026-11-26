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
cat("all passed\n")
