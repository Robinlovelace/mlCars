# Aim: add geographical variables to flow data

wyflows = readRDS("data/wyflows.Rds")
library(tmap)
library(dplyr)
names(wyflows)
wyflows$all = rowSums(wyflows@data[4:11])
qtm(wyflows[sample(nrow(wyflows), 1000),], lines.col = "all") 
