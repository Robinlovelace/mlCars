# http://wicid.ukdataservice.ac.uk/
# Download WU03EW dataset (OD lines)
# WU02EW Location of usual residence and place of work by age
# WU01EW Location of usual residence and place of work by sex

library(stplanr)
library(dplyr)
library(sp)

#unzip("data/wu03ew_v2.zip", exdir="data/")
#unzip("data/wu02ew_v2.zip", exdir="data/")
#unzip("data/wu01ew_v2.zip", exdir="data/")


################################################
# READ IN THE ORIGIN-DESTINATION MSOA-LEVEL DATA
odall = readr::read_csv("data/wu03ew_v2.csv")
odall = odall %>% distinct(., .keep_all=T)
#glimpse(odall)

################################################
# READ IN THE ORIGIN-DESTINATION MSOA-LEVEL AGE DATA
locage = readr::read_csv("data/wu02ew_v2.csv")
locage = locage %>% distinct(., .keep_all=T)
#glimpse(locage)

################################################
# READ IN THE ORIGIN-DESTINATION MSOA-LEVEL GENDER DATA
locgen = readr::read_csv("data/wu01ew_v2.csv")
locgen = locgen %>% distinct(., .keep_all=T)
#glimpse(locgen)

################################################
# JOIN THESE THREE ORIGIN-DESTINATION DATASETS
df = left_join(odall, locage, by=c("Area of residence","Area of workplace"))
df = left_join(df, locgen, by=c("Area of residence","Area of workplace"))

names(df)[names(df)=="Area of residence"] = "homeMSOA"
names(df)[names(df)=="Area of workplace"] = "workMSOA"

str(df)


#Amelia::missmap(df)


################################################
# WE CAN NOW START ADDING IN GEODEMOGRAPHIC DATA BY RESIDENCE MSOA, AND WORKPLACE CLASSIFICATION DATA BY WORKPLACE MSOA
# https://www.nomisweb.co.uk/census/2011/data_finder


# ##############################################
# READ IN DATA ON AREA OF RESIDENCE

# car or van availability
# https://www.nomisweb.co.uk/census/2011/ks404ew
carsdf = readr::read_csv("data/car_van_availability_KS404EW.csv")
carsdf = carsdf %>% distinct(., .keep_all=T)

#names(carsdf)
names(carsdf)[names(carsdf)=="geography"] = "nameMSOA"
names(carsdf)[names(carsdf)=="geography code"] = "homeMSOA"
names(carsdf)[names(carsdf)=="Cars: All categories: Car or van availability; measures: Value"] = "nhouses"
names(carsdf)[names(carsdf)=="Cars: No cars or vans in household; measures: Value"] = "housesw0car"
names(carsdf)[names(carsdf)=="Cars: 1 car or van in household; measures: Value"] = "housesw1car"
names(carsdf)[names(carsdf)=="Cars: 2 cars or vans in household; measures: Value"] = "housesw2car"
names(carsdf)[names(carsdf)=="Cars: 3 cars or vans in household; measures: Value"] = "housesw3car"
names(carsdf)[names(carsdf)=="Cars: 4 or more cars or vans in household; measures: Value"] = "housesw4ormorecar"
names(carsdf)[names(carsdf)=="Cars: sum of all cars or vans in the area; measures: Value"] = "totalcar"
#carsdf

#carsdf$carsdfperhouse = carsdf$totalcarsdf/carsdf$nhouses
carsdf$house0carpct = carsdf$housesw0car/carsdf$nhouses
carsdf$house1carpct = carsdf$housesw1car/carsdf$nhouses
carsdf$house2carpct = carsdf$housesw2car/carsdf$nhouses
carsdf$house3carpct = carsdf$housesw3car/carsdf$nhouses
carsdf$house4carpct = carsdf$housesw4ormorecar/carsdf$nhouses

carsdf = carsdf %>% select(homeMSOA,house0carpct,house1carpct,house2carpct,house3carpct,house4carpct)

# population density
# https://www.nomisweb.co.uk/census/2011/qs102ew
popden = readr::read_csv("data/population_density_QS102EW.csv")
popden = popden %>% distinct(., .keep_all=T)

names(popden)[names(popden)=="geography code"] = "homeMSOA"
names(popden)[names(popden)=="Area/Population Density: Density (number of persons per hectare); measures: Value"] = "ppperhect"

popden = popden %>% select(homeMSOA,ppperhect)

# economic activity
# https://www.nomisweb.co.uk/census/2011/qs601ew
econact = readr::read_csv("data/economic_activity_QS601EW.csv")
econact = econact %>% distinct(., .keep_all=T)
str(econact)

#names(econact)[names(econact)=="geography code"] = "homeMSOA"

econact = econact %>% select(`geography code`,`Economic Activity: Economically active: Total; measures: Value`,`Economic Activity: Economically inactive: Total; measures: Value`,`Economic Activity: All categories: Economic activity; measures: Value`)
econact = econact %>% transmute(homeMSOA=`geography code`,econactivpct=`Economic Activity: Economically active: Total; measures: Value`/`Economic Activity: All categories: Economic activity; measures: Value`, econinactivpct=`Economic Activity: Economically inactive: Total; measures: Value`/`Economic Activity: All categories: Economic activity; measures: Value`)


# JOIN THE HOME MSOA DATA IN
homevars = inner_join(carsdf, popden, by="homeMSOA")
homevars = inner_join(homevars, econact, by="homeMSOA")

df = left_join(df, homevars, by="homeMSOA")
# distance travelled to work
# dist = readr::read_csv("data/distance_to_work_QS702EW.csv")
# dist = dist %>% distinct(., .keep_all=T)
# dist

# ##############################################
# READ IN DATA ON AREA OF WORKPLACE



################################################
# NEED TO MATCH THE MSOAS TO LOCAL AUTHORITY SO WE CAN SPLIT OFF WEST YORKSHIRE, WHICH WILL BE OUR CASE STUDY
# http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/products/census/lookup/other/index.html
# 2011 OAs to current counties in England
#unzip("data/Postcodes_(Enumeration)_(2011)_to_output_areas_(2011)_to_lower_layer_SOA_(2011)_to_middle_layer_SOA_(2011)_to_local_authority_districts_(2011)_E+W_lookup.zip", exdir="data/")
lookup = readr::read_csv("data/PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU.csv")
glimpse(lookup)

lookup = lookup %>% select(MSOA11CD,MSOA11NM,LAD11NM)

# select only West Yorkshire for now
lookup_wy = lookup %>% filter(grepl("Leeds|Bradford|Kirklees|Calderdale|Wakefield", LAD11NM))
unique(lookup_wy$LAD11NM)

wydf = df %>% filter(homeMSOA %in% lookup_wy$MSOA11CD & workMSOA %in% lookup_wy$MSOA11CD)
glimpse(wydf)


# READ IN THE SPATIAL DATA##############################################
# download shapefile for west yorkshire
# https://geoportal.statistics.gov.uk/datasets/826dc85fb600440889480f4d9dbb1a24_3
#unzip("data/Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales.zip", exdir="data/")
shpfile = raster::shapefile("data/Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales.shp")
shpfile = shpfile[!duplicated(shpfile@data),]
shpfile

summary(shpfile$msoa11cd %in% lookup$MSOA11CD)
shpfile_wy = shpfile[shpfile$msoa11cd %in% wydf$homeMSOA | shpfile$msoa11cd %in% wydf$workMSOA,]
plot(shpfile_wy)

#wydf = sp::merge(shpfile_wy, wydf, by)



##################################################
# READ IN THE CENTROIDS TO COMPUTE HOME-WORK EUCLIDEAN DISTANCE
# http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/products/census/spatial/centroids/index.html
#unzip("data/middle_layer_super_output_areas_(e+w)_2011_population_weighted_centroids_v2.zip")
cents = raster::shapefile("data/Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales.shp")
cents = cents[!duplicated(cents@data),]
#plot(cents)
#names(cents)

if(!file.exists("data/wyflows.Rda")){
  starttime = proc.time()
  #df = df[df$homeMSOA %in% cents$msoa11cd & df$workMSOA %in% cents$msoa11cd, ]
  wydf = wydf[wydf$homeMSOA %in% cents$msoa11cd & wydf$workMSOA %in% cents$msoa11cd, ]
  wycents = cents[shpfile_wy,]
  wyflows = od2line(flow=wydf, zones=wycents, origin_code="homeMSOA", dest_code="workMSOA", zone_code="msoa11cd")
  saveRDS(wyflows, "data/wyflows.Rda")
  print(proc.time() - starttime)
}else{
  wyflows = readRDS("data/wyflows.Rda")
}

plot(shpfile_wy); plot(wyflows[wyflows$`All categories: Method of travel to work`>=500,], col="red", add=T)
png("figures/flows_500_westyorkshire.png", res=100)
plot(shpfile_wy); plot(wyflows[wyflows$`All categories: Method of travel to work`>=500,], col="red", add=T)
dev.off()
#######################################################################


# SPLIT WEST YORKSHIRE INTO TRAINING, VALIDATION, AND TEST SETS
nrow(wyflows)

idx = seq(1,nrow(wyflows),1)
set.seed(5)
idx = sample(idx, replace=F)
set.seed(NULL)
#runif(1); .Random.seed[1:6]; runif(1); .Random.seed[1:6]

trainsize = floor(nrow(wyflows)*0.5)
valsize = floor(nrow(wyflows)*0.25)
testsize = floor(nrow(wyflows)*0.25)

splits = split(idx, rep(1:3, c(trainsize,valsize,testsize)))
traindf = wyflows[splits[[1]],]
valdf = wyflows[splits[[2]],]
testdf = wyflows[splits[[3]],]

#saveRDS(df, "full_dataset.Rds")
saveRDS(traindf, "data/training_set.Rds")
saveRDS(valdf, "data/validation_set.Rds")
saveRDS(testdf, "data/test_set.Rds")
