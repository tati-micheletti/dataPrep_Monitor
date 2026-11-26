# Test of readMhbObservations() (2026-11-26; from the repo root: Rscript modules/dataPrep_Monitor/tests/test_readMhbObservations.R)
source("modules/dataPrep_Monitor/R/readMhbObservations.R")
chk <- function(label, ok) { cat(if (isTRUE(ok)) "ok   " else "FAIL ", label, "\n"); if (!isTRUE(ok)) quit(status = 1) }
d <- tempfile(); dir.create(d)
# old layout: returned unchanged
old <- data.frame(ORNITHO_ID = 1:2, SPECIES_NAME_GERMAN = c("Feldlerche", "Star"), SPECIES_NAME_SCIENTIFIC = c("Alauda arvensis", "Sturnus vulgaris"), DATE_TIME = c("2024-05-04", "2024-06-01"),
                  TOTAL_COUNT = c(2, 1), ATLAS_CODE = c("A2", "B3"), LONGITUDE = c(10.1, 11.2), LATITUDE = c(50.1, 51.2), AREA_NATIONAL_CODE = c("bb1", "BY22"))
write.csv(old, fo <- file.path(d, "old.csv"), row.names = FALSE, fileEncoding = "UTF-8")
chk("old layout is returned as it was read before", identical(readMhbObservations(fo), utils::read.csv(fo, header = TRUE, fileEncoding = "UTF-8")))
# new layout
nw <- data.frame(id = c("a", "b", "c", "d"), species = c("Alauda arvensis", "Sturnus vulgaris", "Alauda arvensis", "Milvus milvus"), count = c(2, 1, 3, 1),
                 timing = c("2025-05-17 05:29:54", "", "2025-06-02 04:00:00", "2025-05-01 06:00:00"), visit_start = c("2025-05-17 03:16:00", "2025-05-20 04:00:00", "2025-06-02 03:00:00", "2025-05-01 05:00:00"),
                 atlas_code = c("A2", "B3", "C12", "A1"), samplearea_code = c("bb115", "by22", "bw5", "bb1"), longitude = c(13.7, 11.2, 9.1, 8.0), latitude = c(52.5, 51.2, 48.1, 50.0),
                 protocol = c("CBBM", "CBBM", "CBBM", "OTHER"), visit_id = c("v1", "v2", "v3", "v4"), extra = 1:4, stringsAsFactors = FALSE)
write.csv(nw, fn <- file.path(d, "flat.csv"), row.names = FALSE, fileEncoding = "UTF-8")
x <- readMhbObservations(fn, germanNames = c("Alauda arvensis" = "Feldlerche", "Sturnus vulgaris" = "Star"))
chk("only the CBBM protocol is kept", nrow(x) == 3)
chk("columns carry the old names", all(c("SPECIES_NAME_SCIENTIFIC", "SPECIES_NAME_GERMAN", "DATE_TIME", "TOTAL_COUNT", "ATLAS_CODE", "AREA_NATIONAL_CODE", "LONGITUDE", "LATITUDE") %in% names(x)))
chk("date: the record's own time, the visit start where it is empty", identical(x$DATE_TIME, c("2025-05-17", "2025-05-20", "2025-06-02")))
chk("the old date parse of the preparation works on it", identical(as.Date(x$DATE_TIME, "%Y-%m-%d"), as.Date(c("2025-05-17", "2025-05-20", "2025-06-02"))))
# UTC timestamps -> LOCAL date (a date-only record is local midnight = 22:00 UTC in summer, 23:00 UTC in winter, of the day before)
tz <- nw[1:4, ]; tz$timing <- c("2025-05-16 22:00:00", "2025-03-14 23:00:00", "2025-06-30 21:59:59", ""); tz$visit_start <- c("2025-05-16 20:00:00", "x", "x", "2025-06-01 21:30:00"); tz$protocol <- "CBBM"
write.csv(tz, ft <- file.path(d, "tz.csv"), row.names = FALSE, fileEncoding = "UTF-8"); y <- readMhbObservations(ft)
chk("summer local midnight (22:00 UTC) is the NEXT local date, winter (23:00 UTC) too", y$DATE_TIME[1] == "2025-05-17" && y$DATE_TIME[2] == "2025-03-15")
chk("21:59:59 UTC in summer is still the same local date (23:59:59 local)", y$DATE_TIME[3] == "2025-06-30")
chk("no timing: the visit start, also converted from UTC (21:30 UTC in June = 23:30 local, same day)", y$DATE_TIME[4] == "2025-06-01")
chk("german names from the lookup", identical(x$SPECIES_NAME_GERMAN, c("Feldlerche", "Star", "Feldlerche")))
chk("without a lookup the german name is NA, not an error", all(is.na(readMhbObservations(fn)$SPECIES_NAME_GERMAN)))
chk("counts, codes and coordinates are carried over", identical(x$TOTAL_COUNT, c(2, 1, 3)) && identical(x$ATLAS_CODE, c("A2", "B3", "C12")) && isTRUE(all.equal(x$LONGITUDE, c(13.7, 11.2, 9.1))))
chk("area code kept as given (the preparation lower-cases it)", identical(x$AREA_NATIONAL_CODE, c("bb115", "by22", "bw5")))
write.csv(nw[, c("id", "species", "count")], fb <- file.path(d, "bad.csv"), row.names = FALSE); r <- tryCatch(readMhbObservations(fb), error = function(e) "error"); chk("an unknown layout is an error naming the missing columns", identical(r, "error"))
cat("all passed\n")
