# Aim: add geographical variables to flow data

library(tmap)
library(dplyr)
library(sp)

wyflows = readRDS("data/wyflows.Rds")
wyflows = spTransform(wyflows, CRS("+init=epsg:4326"))
summary(wyflows$response)

names(wyflows)
tmap_mode("view")
wyflows_all = wyflows
wyflows = wyflows[wyflows$distance > 0,]
(m = qtm(wyflows[wyflows$npeople > 200,], lines.col = "response", lines.style = "pretty") )# fails when interactive

# most important geographical vars for car dependency
# distance from motorway. hypothesis: more % (relative distance) drive
# devtools::install_github("osmdatar/osmdata") # use new osmdata package
library(osmdata)
q_motorways = opq(bbox = bbox(wyflows)) %>%
  add_feature(key = "highway", value = "motorway")
motorways = osmdata_sp(q_motorways)
(m1 = m + qtm(motorways$osm_points))
sel_mways = seq(from = 1, to = nrow(motorways$osm_poi), by = 100)
m + qtm(motorways$osm_points[sel_mways,])
l_points = stplanr::line2points(wyflows) # takes a while to run


spDistsN1(wyflows, motorways$osm_points[sel_mways,])
wyflow_proj = spTransform(wyflows, CRS("+init=epsg:27700"))
mpoints_proj = spTransform(motorways$osm_points[sel_mways,], CRS("+init=epsg:27700"))

dm = rgeos::gDistance(wyflow_proj, mpoints_proj, byid = T, )
dm = unlist(dm)
dmin = apply(dm, 2, min)


# parks
# bus stops


# out-takes
# summary(is.na(coords)) # no nas
# summary(wyflows$distance)

