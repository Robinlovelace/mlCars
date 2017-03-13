# Aim: add geographical variables to flow data

library(tmap)
library(dplyr)
library(sp)

wyflows = readRDS("data/wyflows.Rds")

summary(wyflows$response)

names(wyflows)
tmap_mode("view")
wyflows_all = wyflows
wyflows = wyflows[wyflows$distance > 0,]
qtm(wyflows[wyflows$npeople > 200,], lines.col = "response", lines.style = "pretty") # fails when interactive

# most important geographical vars for car dependency
# distance from motorway. hypothesis: more % (relative distance) drive
# devtools::install_github("osmdatar/osmdata") # use new osmdata package
library(osmdata)
q_motorways = opq(bbox = bbox(wyflows)) %>%
  add_feature(key = "motorway", value = "motorway")
motorways = osmdata_sf(q_m)
# parks
# bus stops


# out-takes
# summary(is.na(coords)) # no nas
# summary(wyflows$distance)

