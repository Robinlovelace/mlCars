# Aim: add geographical variables to flow data

library(tmap)
library(dplyr)
library(sp)

wyflows = readRDS("data/wyflows_w_response.Rds")
wyflows = spTransform(wyflows, CRS("+init=epsg:4326"))
summary(wyflows$response)

names(wyflows)
tmap_mode("view")
wyflows_all = wyflows
wyflows = wyflows[wyflows$distance > 0,]
qtm(wyflows[wyflows$npeople > 200,], lines.col = "response", lines.style = "pretty")
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
# l_points = stplanr::line2points(wyflows) # takes a while to run


wyflow_proj = spTransform(wyflows, CRS("+init=epsg:27700"))

wyzones = readRDS("data/WY_zones.Rds")
wyzones = spTransform(wyzones, CRSobj = proj4string(wyflow_proj))

# Download coordinates of train stations
# http://naptan.app.dft.gov.uk/datarequest/help
#download.file("http://naptan.app.dft.gov.uk/DataRequest/Naptan.ashx?format=csv", "data/naptan.zip")
#unzip("data/naptan.zip", exdir = "data/naptan")

trainstn = readr::read_csv("data/naptan/RailReferences.csv")
trainstn = trainstn %>% distinct(., .keep_all=T) %>% dplyr::select(StationName,Easting,Northing)
coachstn = readr::read_csv("data/naptan/CoachReferences.csv")
coachstn = coachstn %>% distinct(., .keep_all=T) %>%  dplyr::select(Name,Easting,Northing)
busstops = readr::read_csv("data/naptan/StopAreas.csv")
busstops = busstops %>% distinct(., .keep_all=T) %>% dplyr::select(Name,Easting,Northing) %>% filter(!is.na(Easting)&!is.na(Northing))


trains_sp = SpatialPointsDataFrame(
  coords = as.matrix(trainstn[c("Easting", "Northing")]),
  data = trainstn,
  proj4string = CRS("+init=epsg:27700")
  )
trains_sp = spTransform(trains_sp, CRSobj = proj4string(wyflow_proj))
trains_sp = trains_sp[wyzones,]
plot(wyzones); plot(trains_sp, col="red", add=T)
trains_dm = rgeos::gDistance(wyflow_proj, trains_sp, byid = T)
trains_dm = unlist(trains_dm)
dmin = apply(trains_dm, 2, min)
summary(dmin) / 1000
wyflows$disttrainstn = dmin/1000
rm(trains_dm)

coach_sp = SpatialPointsDataFrame(
  coords = as.matrix(coachstn[c("Easting", "Northing")]),
  data = coachstn,
  proj4string = CRS("+init=epsg:27700")
)
coach_sp = spTransform(coach_sp, CRSobj = proj4string(wyflow_proj))
coach_sp = coach_sp[wyzones,]
plot(wyzones); plot(coach_sp, col="red", add=T)
coach_dm = rgeos::gDistance(wyflow_proj, coach_sp, byid = T)
coach_dm = unlist(coach_dm)
dmin = apply(coach_dm, 2, min)
summary(dmin) / 1000
wyflows$distcoachstn = dmin/1000
rm(coach_dm)

bus_sp = SpatialPointsDataFrame(
  coords = as.matrix(busstops[c("Easting", "Northing")]),
  data = busstops,
  proj4string = CRS("+init=epsg:27700")
)
bus_sp = spTransform(bus_sp, CRSobj = proj4string(wyflow_proj))
bus_sp = bus_sp[wyzones,]
plot(wyzones); plot(bus_sp, col="red", add=T)
bus_dm = rgeos::gDistance(wyflow_proj, bus_sp, byid = T)
bus_dm = unlist(bus_dm)
dmin = apply(bus_dm, 2, min)
summary(dmin) / 1000
wyflows$distbusstop = dmin/1000
rm(bus_dm)

# spDistsN1(wyflows, motorways$osm_points[sel_mways,])
mpoints_proj = spTransform(motorways$osm_points[sel_mways,], proj4string(wyflow_proj))
mpoints_proj = mpoints_proj[wyzones,]
plot(wyzones); plot(mpoints_proj, col="red", add=T)
dm = rgeos::gDistance(wyflow_proj, mpoints_proj, byid = T)
dm = unlist(dm)
dmin = apply(dm, 2, min)
summary(dmin) / 1000
wyflows$distmway = dmin/1000


plot(wyflows[wyflows$distmway > 5,])
plot(motorways$osm_lines, col="red" , add = T)



# parks
# q_parks_l = opq(bbox = bbox(wyflows)) %>%
#   add_feature(key = "leisure", value = "park")
# parks = osmdata_sp(q_parks_l)
# plot(parks$osm_polygons)
# qtm(parks$osm_points)

# bus stops


# out-takes
# summary(is.na(coords)) # no nas
# summary(wyflows$distance)

