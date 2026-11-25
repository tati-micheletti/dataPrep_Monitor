# Test of findCtmFile() (2026-11-25; from the repo root: Rscript modules/dataPrep_Monitor/tests/test_findCtmFile.R).
source("modules/dataPrep_Monitor/R/findCtmFile.R")
d <- tempfile(); dir.create(d)
mk <- function(...) for (f in c(...)) file.create(file.path(d, f))
mk("CTM_GER_2017_rst_v202_COG.tif", "CTM_GER_2024_rst_v302_COG.tif", "CTM_GER_2025_rst_v302_2025_08_COG.tif", "CTM_GER_2026_rst_v302_2026_09_COG.tif",
   "CTM_GER_2017-21_raster_accuracy_v202.pdf", "HCTM_GER_2017_rst_v101_COG.tif", "CTM_GER_2027_rst_v302_2027_03_COG.tif", "CTM_GER_2027_rst_v302_2027_09_COG.tif")
chk <- function(label, ok) { cat(if (isTRUE(ok)) "ok   " else "FAIL ", label, "\n"); if (!isTRUE(ok)) quit(status = 1) }
r <- findCtmFile(2017, d); chk("v202 map is found (series 1)", basename(r$file) == "CTM_GER_2017_rst_v202_COG.tif" && r$version == 202 && r$status == "final")
r <- findCtmFile(2024, d); chk("finished v302 map", basename(r$file) == "CTM_GER_2024_rst_v302_COG.tif" && r$status == "final" && is.na(r$inSeasonMonth))
r <- findCtmFile(2025, d); chk("in-season 2025 map (old name, month 8)", basename(r$file) == "CTM_GER_2025_rst_v302_2025_08_COG.tif" && r$status == "in-season" && r$inSeasonMonth == 8L)
r <- findCtmFile(2026, d); chk("in-season 2026 map (month 9)", basename(r$file) == "CTM_GER_2026_rst_v302_2026_09_COG.tif" && r$status == "in-season" && r$inSeasonMonth == 9L)
r <- findCtmFile(2027, d); chk("of two in-season maps the later month", basename(r$file) == "CTM_GER_2027_rst_v302_2027_09_COG.tif")
mk("CTM_GER_2025_rst_v302_COG.tif"); r <- findCtmFile(2025, d); chk("a finished map replaces the in-season one of the same year", basename(r$file) == "CTM_GER_2025_rst_v302_COG.tif" && r$status == "final")
mk("CTM_GER_2025_rst_v303_COG.tif"); r <- findCtmFile(2025, d); chk("a higher version wins", r$version == 303L)
r <- findCtmFile(2019, d); chk("no file: NA, no error", is.na(r$file) && is.na(r$status))
chk("no folder: NA, no error", is.na(findCtmFile(2025, file.path(d, "nope"))$file))
cat("all passed\n")
