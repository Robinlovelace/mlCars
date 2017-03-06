# http://wicid.ukdataservice.ac.uk/
# Download WU03EW dataset (OD lines)
# WU02EW Location of usual residence and place of work by age
# WU01EW Location of usual residence and place of work by sex

library(dplyr)
library(sp)

#unzip("data/wu03ew_v2.zip", exdir="data/")
#unzip("data/wu02ew_v2.zip", exdir="data/")
#unzip("data/wu01ew_v2.zip", exdir="data/")

odall = readr::read_csv("data/wu03ew_v2.csv")
odall = odall %>% distinct(., .keep_all=T)
glimpse(odall)

locage = readr::read_csv("data/wu02ew_v2.csv")
locage = locage %>% distinct(., .keep_all=T)
glimpse(locage)

locgen = readr::read_csv("data/wu01ew_v2.csv")
locgen = locgen %>% distinct(., .keep_all=T)
glimpse(locgen)

df = left_join(odall, locage, by=c("Area of residence","Area of workplace"))
df = left_join(df, locgen, by=c("Area of residence","Area of workplace"))

glimpse(df)

#Amelia::missmap(df)

# http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/products/census/lookup/other/index.html
# 2011 OAs to current counties in England
#unzip("data/Postcodes_(Enumeration)_(2011)_to_output_areas_(2011)_to_lower_layer_SOA_(2011)_to_middle_layer_SOA_(2011)_to_local_authority_districts_(2011)_E+W_lookup.zip", exdir="data/")
lookup = readr::read_csv("data/PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU.csv")
glimpse(lookup)

lookup = lookup %>% select(MSOA11CD,MSOA11NM,LAD11NM)

# select only West Yorkshire for now
lookup_wy = lookup %>% filter(grepl("Leeds|Bradford|Kirklees|Calderdale|Wakefield", LAD11NM))
unique(lookup_wy$LAD11NM)

wydf = df %>% filter(`Area of residence` %in% lookup_wy$MSOA11CD & `Area of workplace` %in% lookup_wy$MSOA11CD)
glimpse(wydf)


# READ IN THE SPATIAL DATA##############################################
# download shapefile for west yorkshire
# https://geoportal.statistics.gov.uk/datasets/826dc85fb600440889480f4d9dbb1a24_3
#unzip("data/Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales.zip", exdir="data/")
shpfile = raster::shapefile("data/Middle_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped_Boundaries_in_England_and_Wales.shp")
shpfile

summary(shpfile$msoa11cd %in% lookup$MSOA11CD)
shpfile_wy = shpfile[shpfile$msoa11cd %in% wydf$`Area of residence` | shpfile$msoa11cd %in% wydf$`Area of workplace`,]
plot(shpfile_wy)

#wydf = sp::merge(shpfile_wy, wydf, by)
#######################################################################


# SPLIT WEST YORKSHIRE INTO TRAINING, VALIDATION, AND TEST SETS
nrow(wydf)
?sample

idx = seq(1,nrow(wydf),1)
set.seed(5)
idx = sample(idx, replace=F)
set.seed(NULL)
#runif(1); .Random.seed[1:6]; runif(1); .Random.seed[1:6]

trainsize = floor(nrow(wydf)*0.5)
valsize = floor(nrow(wydf)*0.25)
testsize = floor(nrow(wydf)*0.25)

splits = split(idx, rep(1:3, c(trainsize,valsize,testsize)))
traindf = wydf[splits[[1]],]
valdf = wydf[splits[[2]],]
testdf = wydf[splits[[3]],]

