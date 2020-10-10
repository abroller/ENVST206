#Activity 6

#install.packages(c("sp","rgdal","dplyr"))

#package for vector data
library(sp)
#package for reading in spatial data
library(rgdal)
#package for data management
library(dplyr)

#read in glacier national park data
  #readOGR in rgdal reads in a shape file 
  #will tell R to look for all the related files
#glaciers in 1966
g1966 <- readOGR("/Users/abby/Documents/ENVST206/ao6/GNPglaciers/GNPglaciers_1966.shp")

#glaciers in 2015
g2015 <- readOGR("/Users/abby/Documents/ENVST206/ao6/GNPglaciers/GNPglaciers_2015.shp")
