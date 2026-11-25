# Test of the climate carry-forward (2026-11-25; from the repo root: Rscript modules/dataPrep_Monitor/tests/test_carryForwardBioclim.R). No CHELSA download.
source("modules/dataPrep_Monitor/R/carryForwardBioclim.R")
chk <- function(label, ok) { cat(if (isTRUE(ok)) "ok   " else "FAIL ", label, "\n"); if (!isTRUE(ok)) quit(status = 1) }
d <- file.path(tempfile(), "50km"); dir.create(d, recursive = TRUE)
src <- bioclimFileName(2025, 6, d); chk("file name of 2025 is bioclim_2020-2025_50km.tif", basename(src) == "bioclim_2020-2025_50km.tif")
writeLines("fake layer of 2025", src)

cf <- carryForwardBioclim(2025, 2025, 6, d); chk("the last year with data is NOT carried forward", !cf$carried && !cf$removedStale)
cf <- carryForwardBioclim(2020, 2025, 6, d); chk("an earlier year is NOT carried forward", !cf$carried)
cf <- carryForwardBioclim(2026, NA_integer_, 6, d); chk("climateLastYear = NA: nothing is carried forward (old behaviour)", !cf$carried)

cf <- carryForwardBioclim(2026, 2025, 6, d); chk("2026 is carried forward from 2025", cf$carried && basename(cf$source) == "bioclim_2020-2025_50km.tif" && basename(cf$file) == "bioclim_2021-2026_50km.tif")
writeCarriedForwardBioclim(cf, 2026, 2025)
chk("copy has the content of the 2025 layer", identical(readLines(cf$file), readLines(src)))
chk("note exists and names the years", file.exists(cf$note) && grepl("2026", readLines(cf$note)[1]) && grepl("2025", readLines(cf$note)[1]))
chk("the 2025 layer itself is unchanged", identical(readLines(src), "fake layer of 2025"))
m1 <- file.mtime(cf$file); Sys.sleep(1.1); writeCarriedForwardBioclim(carryForwardBioclim(2026, 2025, 6, d), 2026, 2025); chk("a second call does not rewrite the copy", identical(m1, file.mtime(cf$file)))

cf2 <- carryForwardBioclim(2027, 2025, 6, d); writeCarriedForwardBioclim(cf2, 2027, 2025); chk("2027 is also a copy of the 2025 layer, not of 2026", identical(readLines(cf2$file), readLines(src)) && grepl("2025", readLines(cf2$note)[1]))

cfNow <- carryForwardBioclim(2026, 2026, 6, d); chk("when climateLastYear is raised to 2026 the stale copy is deleted", !cfNow$carried && cfNow$removedStale && !file.exists(cfNow$file) && !file.exists(cfNow$note))
chk("the copy of 2027 is still there (not yet real data)", file.exists(cf2$file))
r <- tryCatch(writeCarriedForwardBioclim(carryForwardBioclim(2030, 2024, 6, d), 2030, 2024), error = function(e) "error"); chk("a missing source layer is an error, not a silent gap", identical(r, "error"))
cat("all passed\n")
