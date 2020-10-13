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
g1966 <- readOGR("/Users/abby/Documents/ENVST206/a06/GNPglaciers/GNPglaciers_1966.shp")
#glaciers in 2015
g2015 <- readOGR("/Users/abby/Documents/ENVST206/a06/GNPglaciers/GNPglaciers_2015.shp")

#investigate format of data
str(g2015)

#map the glaciers with specified colors
plot(g1966, col = "lightblue2", border="grey50")

#preview the first 6 lines and columns of shape file data table 
  #using @ is a feature specific to spatial data
head(g2015@data)

#vector object projection info
g1966@proj4string

#check glacier names (they don't match exactly)
g1966@data$GLACNAME
g2015@data$GLACNAME
#fix glacier names to be consistent across years
g2015@data$GLACNAME <-ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                             "N. Swiftcurrent Glacier",
                             ifelse(g2015@data$GLACNAME == "Miche Wabun",
                                    "Miche Wabun Glacier",
                                    as.character(g2015@data$GLACNAME)))


