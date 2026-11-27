# Test of readPlotPolygons() (2026-11-27; from the repo root: Rscript modules/dataPrep_Monitor/tests/test_readPlotPolygons.R)
suppressMessages({library(sf); library(dplyr)})
source("modules/dataPrep_Monitor/R/readPlotPolygons.R")
chk <- function(label, ok) { cat(if (isTRUE(ok)) "ok   " else "FAIL ", label, "\n"); if (!isTRUE(ok)) quit(status = 1) }
sq <- function(x0, y0, s = 500) st_polygon(list(rbind(c(x0, y0), c(x0 + s, y0), c(x0 + s, y0 + s), c(x0, y0 + s), c(x0, y0))))
d <- tempfile(); dir.create(d)
# shapefile (EPSG:25832): plots A and B
shp <- st_sf(SITE_ID = c("A1", "B2"), other = c("x", "y"), geometry = st_sfc(sq(500000, 5500000), sq(510000, 5510000), crs = 25832))
st_write(shp, shpf <- file.path(d, "plots.shp"), quiet = TRUE)
# sampleareas.csv (EPSG:4326, EWKB hex): A1 (also in the shapefile, 1 m shifted in the 25832 plane), C3 (new, only here), and a non-CBBM row that must be ignored
mk <- function(code, protocol, x0, y0) { g <- st_transform(st_sfc(sq(x0, y0), crs = 25832), 4326); data.frame(code = code, protocol = protocol, area = st_as_binary(g, EWKB = TRUE, hex = TRUE)[[1]]) }
sa <- rbind(mk("a1", "CBBM", 500001, 5500000), mk("C3", "CBBM", 520000, 5520000), mk("D4", "OTHER", 530000, 5530000), data.frame(code = "E5", protocol = "CBBM", area = ""))
write.csv(sa, saf <- file.path(d, "sampleareas.csv"), row.names = FALSE)
o <- suppressMessages(readPlotPolygons(shpf)); chk("without sampleareas: the shapefile as it is", nrow(o) == 2 && identical(o$SITE_ID, c("A1", "B2")))
o <- suppressMessages(readPlotPolygons(shpf, NA_character_)); chk("NA path = the shapefile as it is", nrow(o) == 2)
m <- suppressMessages(readPlotPolygons(shpf, saf))
chk("A1 + C3 from sampleareas.csv, B2 from the shapefile (fallback); the non-CBBM and the empty-polygon rows are ignored", nrow(m) == 3 && setequal(tolower(m$SITE_ID), c("a1", "b2", "c3")))
chk("same CRS and id column as the shapefile", st_crs(m) == st_crs(shp) && "SITE_ID" %in% names(m))
cen <- st_coordinates(st_centroid(st_geometry(m))); i <- which(tolower(m$SITE_ID) == "a1")
chk("a plot in both comes from sampleareas.csv (centroid 1 m east of the shapefile's)", abs(cen[i, 1] - 500250 - 1) < 0.1 && abs(cen[i, 2] - 5500250) < 0.1)
chk("C3 has coordinates where sampleareas.csv puts it", abs(cen[which(tolower(m$SITE_ID) == "c3"), 1] - 520250) < 0.1)
sa2 <- rbind(mk("A1", "CBBM", 500400, 5500000)); write.csv(sa2, saf2 <- file.path(d, "sa_bad.csv"), row.names = FALSE)
r <- tryCatch(suppressMessages(readPlotPolygons(shpf, saf2)), error = function(e) conditionMessage(e)); chk("centroids 400 m apart: stops (limit 50 m) with a clear message", is.character(r) && grepl("differ by up to", r))
cat("all passed\n")
