# Test of corineYear()/corineYearMap() after the end year was taken out of the code (2026-11-25; from the repo root: Rscript modules/dataPrep_Monitor/tests/test_corineYear.R).
source("modules/dataPrep_Monitor/R/corineYearMap.R"); source("modules/dataPrep_Monitor/R/corineYear.R")
chk <- function(label, ok) { cat(if (isTRUE(ok)) "ok   " else "FAIL ", label, "\n"); if (!isTRUE(ok)) quit(status = 1) }
old <- list("2006" = 2005:2011, "2012" = 2012:2017, "2018" = 2018:2025)    # the hard-coded map used up to 2025
oldYear <- function(y) { for (k in names(old)) if (y %in% old[[k]]) return(as.integer(k)); 2018L }
chk("2005-2025 give the same snapshot as the old hard-coded map", all(vapply(2005:2025, corineYear, integer(1)) == vapply(2005:2025, oldYear, integer(1))))
chk("corineYearMap(2005:2025) equals the old list", identical(corineYearMap(2005:2025), old))
chk("names of the map are the snapshot years (consumers use names())", identical(names(corineYearMap()), c("2006", "2012", "2018")))
chk("2026 and 2030 use the last snapshot", corineYear(2026) == 2018L && corineYear(2030) == 2018L)
chk("years before the first snapshot use the first one", corineYear(2000) == 2006L)
chk("boundaries: 2011 -> 2006, 2012 -> 2012, 2017 -> 2012, 2018 -> 2018", all(vapply(c(2011, 2012, 2017, 2018), corineYear, integer(1)) == c(2006L, 2012L, 2012L, 2018L)))
cat("all passed\n")
