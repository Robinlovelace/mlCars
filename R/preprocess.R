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

# note that we include both drivers and passengers in the car category!!
#odall = odall %>% transmute(homeMSOA=`Area of residence`, workMSOA=`Area of workplace`, workhome=`Work mainly at or from home`/`All categories: Method of travel to work`, metro=`Underground, metro, light rail, tram`/`All categories: Method of travel to work`, train=Train/`All categories: Method of travel to work`, bus=`Bus, minibus or coach`/`All categories: Method of travel to work`, taxi=Taxi/`All categories: Method of travel to work`, motorcycle=`Motorcycle, scooter or moped`/`All categories: Method of travel to work`, car=(`Driving a car or van`+`Passenger in a car or van`)/`All categories: Method of travel to work`, cycle=Bicycle/`All categories: Method of travel to work`, walk=`On foot`/`All categories: Method of travel to work`, othertransp=`Other method of travel to work`/`All categories: Method of travel to work`, npeople=`All categories: Method of travel to work`)
#all(rowSums(odall[, !names(odall) %in% c("homeMSOA", "workMSOA")]), na.rm = T) == 1
odall = odall %>% transmute(homeMSOA=`Area of residence`, workMSOA=`Area of workplace`, metro=`Underground, metro, light rail, tram`, train=Train, bus=`Bus, minibus or coach`, taxi=Taxi, motorcycle=`Motorcycle, scooter or moped`, car=(`Driving a car or van`+`Passenger in a car or van`), cycle=Bicycle, walk=`On foot`, othertransp=`Other method of travel to work`, npeople=`All categories: Method of travel to work`-`Work mainly at or from home`)

################################################
# READ IN THE ORIGIN-DESTINATION MSOA-LEVEL AGE DATA
locage = readr::read_csv("data/wu02ew_v2.csv")
locage = locage %>% distinct(., .keep_all=T)
#locage = locage %>% transmute(homeMSOA=`Area of residence`, workMSOA=`Area of workplace`, `16-24`=`16-24`/`All categories: Age 16 and over`, `25-34`=`25-34`/`All categories: Age 16 and over`, `35-49`=`35-49`/`All categories: Age 16 and over`, `50-54`=`50-64`/`All categories: Age 16 and over`, `65-74`=`65-74`/`All categories: Age 16 and over`, `75+`=`75+`/`All categories: Age 16 and over`)
#all(rowSums(locage[, !names(locage) %in% c("homeMSOA", "workMSOA")])) == 1
locage = locage %>% rename(homeMSOA=`Area of residence`, workMSOA=`Area of workplace`)
locage = locage %>% dplyr::select(-`All categories: Age 16 and over`)

################################################
# READ IN THE ORIGIN-DESTINATION MSOA-LEVEL GENDER DATA
locgen = readr::read_csv("data/wu01ew_v2.csv")
locgen = locgen %>% distinct(., .keep_all=T)
#locgen = locgen %>% transmute(homeMSOA=`Area of residence`, workMSOA=`Area of workplace`, female=Female/`All categories: Sex`)
locgen = locgen %>% rename(homeMSOA=`Area of residence`, workMSOA=`Area of workplace`, male=Male, female=Female)
locgen = locgen %>% dplyr::select(-`All categories: Sex`)

################################################
# JOIN THESE THREE ORIGIN-DESTINATION DATASETS
df = left_join(odall, locage, by=c("homeMSOA","workMSOA"))
df = left_join(df, locgen, by=c("homeMSOA","workMSOA"))

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

##carsdf$carsdfperhouse = carsdf$totalcarsdf/carsdf$nhouses
# carsdf$house0carpct = carsdf$housesw0car/carsdf$nhouses
# carsdf$house1carpct = carsdf$housesw1car/carsdf$nhouses
# carsdf$house2carpct = carsdf$housesw2car/carsdf$nhouses
# carsdf$house3carpct = carsdf$housesw3car/carsdf$nhouses
# carsdf$house4carpct = carsdf$housesw4ormorecar/carsdf$nhouses

#carsdf = carsdf %>% dplyr::select(homeMSOA,house0carpct,house1carpct,house2carpct,house3carpct,house4carpct)
carsdf = carsdf %>% dplyr::select(homeMSOA,housesw0car,housesw1car,housesw2car,housesw3car,housesw4ormorecar)


# population density
# https://www.nomisweb.co.uk/census/2011/qs102ew
#popden = readr::read_csv("data/population_density_QS102EW.csv")
#popden = popden %>% distinct(., .keep_all=T)

#names(popden)[names(popden)=="geography code"] = "homeMSOA"
#names(popden)[names(popden)=="Area/Population Density: Density (number of persons per hectare); measures: Value"] = "ppperhect"

#popden = popden %>% dplyr::select(homeMSOA,ppperhect)


# economic activity
# https://www.nomisweb.co.uk/census/2011/qs601ew
econact = readr::read_csv("data/economic_activity_QS601EW.csv")
econact = econact %>% distinct(., .keep_all=T)

econact = econact %>% dplyr::select(`geography code`,`Economic Activity: Economically active: Total; measures: Value`,`Economic Activity: Economically inactive: Total; measures: Value`,`Economic Activity: All categories: Economic activity; measures: Value`)
#econact = econact %>% transmute(homeMSOA=`geography code`,econactivpct=`Economic Activity: Economically active: Total; measures: Value`/`Economic Activity: All categories: Economic activity; measures: Value`, econinactivpct=`Economic Activity: Economically inactive: Total; measures: Value`/`Economic Activity: All categories: Economic activity; measures: Value`)
#all(rowSums(econact[, !names(econact) %in% c("homeMSOA")])) == 1
econact = econact %>% rename(homeMSOA=`geography code`,econactiv=`Economic Activity: Economically active: Total; measures: Value`, econinactiv=`Economic Activity: Economically inactive: Total; measures: Value`)
econact = econact %>% dplyr::select(-`Economic Activity: All categories: Economic activity; measures: Value`)

# general health
# https://www.nomisweb.co.uk/census/2011/qs302ew
health = readr::read_csv("data/general_health_QS302EW.csv")
health = health %>% distinct(., .keep_all=T)
#health = health %>% transmute(homeMSOA=`geography code`, vghealth=`General Health: Very good health; measures: Value`/`General Health: All categories: General health; measures: Value`, ghealth=`General Health: Good health; measures: Value`/`General Health: All categories: General health; measures: Value`, fhealth=`General Health: Fair health; measures: Value`/`General Health: All categories: General health; measures: Value`, bhealth=`General Health: Bad health; measures: Value`/`General Health: All categories: General health; measures: Value`, vbhealth=`General Health: Very bad health; measures: Value`/`General Health: All categories: General health; measures: Value`)
#all(rowSums(health[, !names(health) %in% c("homeMSOA")])) == 1
health = health %>% rename(homeMSOA=`geography code`, vghealth=`General Health: Very good health; measures: Value`, ghealth=`General Health: Good health; measures: Value`, fhealth=`General Health: Fair health; measures: Value`, bhealth=`General Health: Bad health; measures: Value`, vbhealth=`General Health: Very bad health; measures: Value`)
health = health %>% dplyr::select(-`General Health: All categories: General health; measures: Value`)

# ethnic group
# https://www.nomisweb.co.uk/census/2011/ks201ew
ethnic = readr::read_csv("data/ethnic_group_KS201EW.csv")
ethnic = ethnic %>% distinct(., .keep_all=T)
#ethnic = ethnic %>% transmute(homeMSOA=`geography code`, white=`Ethnic Group: White; measures: Value`/`Ethnic Group: All usual residents; measures: Value`, mixed=`Ethnic Group: Mixed/multiple ethnic groups; measures: Value`/`Ethnic Group: All usual residents; measures: Value`, asian=`Ethnic Group: Asian/Asian British; measures: Value`/`Ethnic Group: All usual residents; measures: Value`, black=`Ethnic Group: Black/African/Caribbean/Black British; measures: Value`/`Ethnic Group: All usual residents; measures: Value`,otherethn=`Ethnic Group: Other ethnic group; measures: Value`/`Ethnic Group: All usual residents; measures: Value`)
#all(rowSums(ethnic[, !names(ethnic) %in% c("homeMSOA")])) == 1
ethnic = ethnic %>% rename(homeMSOA=`geography code`, white=`Ethnic Group: White; measures: Value`, mixed=`Ethnic Group: Mixed/multiple ethnic groups; measures: Value`, asian=`Ethnic Group: Asian/Asian British; measures: Value`, black=`Ethnic Group: Black/African/Caribbean/Black British; measures: Value`, otherethn=`Ethnic Group: Other ethnic group; measures: Value`)
ethnic = ethnic %>% dplyr::select(homeMSOA, white, mixed, asian, black, otherethn)

# education
# https://www.nomisweb.co.uk/census/2011/qs501ew
educ = readr::read_csv("data/education_QS501EW.csv")
educ = educ %>% distinct(., .keep_all=T)
#educ = educ %>% transmute(homeMSOA=`geography code`, noqual=`Qualification: No qualifications; measures: Value`/`Qualification: All categories: Highest level of qualification; measures: Value`,aptshpqual=`Qualification: Apprenticeship; measures: Value`/`Qualification: All categories: Highest level of qualification; measures: Value` , lev1qual=`Qualification: Level 1 qualifications; measures: Value`/`Qualification: All categories: Highest level of qualification; measures: Value`, lev2qual=`Qualification: Level 2 qualifications; measures: Value`/`Qualification: All categories: Highest level of qualification; measures: Value`, lev3qual=`Qualification: Level 3 qualifications; measures: Value`/`Qualification: All categories: Highest level of qualification; measures: Value`, lev4qual=`Qualification: Level 4 qualifications and above; measures: Value`/`Qualification: All categories: Highest level of qualification; measures: Value`,otherqual=`Qualification: Other qualifications; measures: Value`/`Qualification: All categories: Highest level of qualification; measures: Value`)
#all(rowSums(educ[, !names(educ) %in% c("homeMSOA")])) == 1
educ = educ %>% transmute(homeMSOA=`geography code`, noqual=`Qualification: No qualifications; measures: Value`, aptshpqual=`Qualification: Apprenticeship; measures: Value`, lev1qual=`Qualification: Level 1 qualifications; measures: Value`, lev2qual=`Qualification: Level 2 qualifications; measures: Value`, lev3qual=`Qualification: Level 3 qualifications; measures: Value`, lev4qual=`Qualification: Level 4 qualifications and above; measures: Value`, otherqual=`Qualification: Other qualifications; measures: Value`)

# # students  # total number of people not included so can't compute fractions
# # https://www.nomisweb.co.uk/census/2011/ks501ew
students = readr::read_csv("data/students_KS501EW.csv")
students = students %>% distinct(., .keep_all=T)
# str(students)
students = students %>% transmute(homeMSOA=`geography code`, schoolstudents=`Qualifications: Schoolchildren and full-time students: Age 16 to 17; measures: Value`, unistudents=`Qualifications: Full-time students: Age 18 to 74: Economically active: In employment; measures: Value`+`Qualifications: Full-time students: Age 18 to 74: Economically active: Unemployed; measures: Value`+`Qualifications: Full-time students: Age 18 to 74: Economically inactive; measures: Value`)

# rooms, bedrooms, and central heating
# https://www.nomisweb.co.uk/census/2011/ks403ew
rooms = readr::read_csv("data/rooms_central_heating_KS403EW.csv")
rooms = rooms %>% distinct(., .keep_all=T)
#rooms = rooms %>% transmute(homeMSOA=`geography code`, centheat=`Central Heating: Does have central heating; measures: Value`/`Central Heating: All categories: Type of central heating in household; measures: Value`, nrooms=`Central Heating: Average number of rooms per household; measures: Value`)
rooms = rooms %>% transmute(homeMSOA=`geography code`, centheat=`Central Heating: Does have central heating; measures: Value`, nrooms=`Central Heating: Average number of rooms per household; measures: Value`)


# JOIN ALL THE HOME MSOA DATA IN
#homevars = inner_join(carsdf, popden, by="homeMSOA")
homevars = inner_join(carsdf, econact, by="homeMSOA")
homevars = inner_join(homevars, health, by="homeMSOA")
homevars = inner_join(homevars, ethnic, by="homeMSOA")
homevars = inner_join(homevars, educ, by="homeMSOA")
homevars = inner_join(homevars, students, by="homeMSOA")
homevars = inner_join(homevars, rooms, by="homeMSOA")


df = left_join(df, homevars, by="homeMSOA")
df = df %>% select(-`Rural Urban`)
str(df)
# distance travelled to work
# dist = readr::read_csv("data/distance_to_work_QS702EW.csv")
# dist = dist %>% distinct(., .keep_all=T)
# dist

# ##############################################
# READ IN DATA ON AREA OF WORKPLACE
# THIS DIDN'T WORK BECAUSE CLASSIFICATION IS AT THE OUTPUT AREA LEVEL, SO MANY CLASS
# http://cowz.geodata.soton.ac.uk/download/
work = readr::read_csv("data/COWZ_EW_2011_Classification.csv")
work = work %>% distinct(., .keep_all=T)
work %>% distinct(COWZEW_SGN, COWZEW_SG) %>% arrange(COWZEW_SG)

##work %>% group_by(MSOA11CD) %>% summarise(n=n()) %>% arrange(desc(n))
##table(work %>% group_by(MSOA11CD) %>% summarise(n=n()) %>% arrange(desc(n)) %>% dplyr::select(n))

# Take the mode of the workplace zone classification of the output areas as the classification of the MSOA
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## the WZ classifications within MSOAs look pretty skewed towards a single one, so taking mode should be faithful representation
##table(work %>% filter(MSOA11CD=="E02000575") %>% dplyr::select(COWZEW_SG))

work = work %>% group_by(MSOA11CD) %>% summarise(wzclass=Mode(COWZEW_SG))
names(work)[names(work)=="MSOA11CD"] = "workMSOA"

df = left_join(df, work, by="workMSOA")

#saveRDS(df, "full_dataset.Rds")

################################################
# NEED TO MATCH THE MSOAS TO LOCAL AUTHORITY SO WE CAN SPLIT OFF WEST YORKSHIRE, WHICH WILL BE OUR CASE STUDY
# http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/products/census/lookup/other/index.html
# 2011 OAs to current counties in England
#unzip("data/Postcodes_(Enumeration)_(2011)_to_output_areas_(2011)_to_lower_layer_SOA_(2011)_to_middle_layer_SOA_(2011)_to_local_authority_districts_(2011)_E+W_lookup.zip", exdir="data/")
lookup = readr::read_csv("data/PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU.csv")
glimpse(lookup)

lookup = lookup %>% dplyr::select(MSOA11CD,MSOA11NM,LAD11NM)

# select only West Yorkshire for now
lookup_wy = lookup %>% filter(grepl("Leeds|Bradford|Kirklees|Calderdale|Wakefield", LAD11NM))
unique(lookup_wy$LAD11NM)


# READ IN THE SPATIAL DATA##############################################
# download shapefile for west yorkshire
# https://geoportal.statistics.gov.uk/datasets/826dc85fb600440889480f4d9dbb1a24_3
#unzip("data/Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales.zip", exdir="data/")
shpfile = raster::shapefile("data/Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales.shp")
shpfile = shpfile[!duplicated(shpfile@data),]
shpfile

# Compute the MSOA areas
shpfile$area = stplanr::gprojected(shpfile, rgeos::gArea, byid = T) / 1000000

df = left_join(df, dplyr::select(shpfile@data, msoa11cd, area), by=c("homeMSOA"="msoa11cd"))

wydf = df %>% filter(homeMSOA %in% lookup_wy$MSOA11CD & workMSOA %in% lookup_wy$MSOA11CD)

summary(shpfile$msoa11cd %in% lookup$MSOA11CD)
shpfile_wy = shpfile[shpfile$msoa11cd %in% wydf$homeMSOA | shpfile$msoa11cd %in% wydf$workMSOA,]
plot(shpfile_wy)

saveRDS(shpfile_wy, "data/WY_zones.Rds")


#glimpse(wydf)

#wydf = sp::merge(shpfile_wy, wydf, by)



##################################################
# READ IN THE CENTROIDS TO COMPUTE HOME-WORK EUCLIDEAN DISTANCE
# http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/products/census/spatial/centroids/index.html
#unzip("data/middle_layer_super_output_areas_(e+w)_2011_population_weighted_centroids_v2.zip")
cents = raster::shapefile("data/Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales.shp")
cents = cents[!duplicated(cents@data),]
#plot(cents)
#names(cents)

if(!file.exists("data/wyflows.Rds")){
  starttime = proc.time()
  #df = df[df$homeMSOA %in% cents$msoa11cd & df$workMSOA %in% cents$msoa11cd, ]
  wydf = wydf[wydf$homeMSOA %in% cents$msoa11cd & wydf$workMSOA %in% cents$msoa11cd, ]
  wycents = cents[shpfile_wy,]
  wyflows = od2line(flow=wydf, zones=wycents, origin_code="homeMSOA", dest_code="workMSOA", zone_code="msoa11cd")
  wyflows$distance = rgeos::gLength(wyflows, byid=T)/1000
  saveRDS(wyflows, "data/wyflows.Rds")
  print(proc.time() - starttime)
}else{
  wyflows = readRDS("data/wyflows.Rds")
}

plot(shpfile_wy); plot(wyflows[wyflows$npeople>=500,], col="red", add=T)
png("figures/flows_500_westyorkshire.png", res=100)
plot(shpfile_wy); plot(wyflows[wyflows$npeople,], col="red", add=T)
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

saveRDS(traindf, "data/training_set.Rds")
saveRDS(valdf, "data/validation_set.Rds")
saveRDS(testdf, "data/test_set.Rds")
