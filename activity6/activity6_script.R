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

#make smaller data frames with glacier names and area
gdf66 <-data.frame(GLACNAME = g1966@data$GLACNAME,
                   area66 = g1966@data$Area1966)
gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)
#join these two data tables by glacier name
  # full_join is a dplyr function
gAll <- full_join(gdf66, gdf15, by="GLACNAME")

# calculate percent change in area from 1966 to 2015 
gAll$gdiff <- ((gAll$area66-gAll$area15)/gAll$area66)*100

## question 7 ## make a scatterplot of 1996 area vs percent change
plot(gAll$area66, gAll$gdiff, pch=19, col="cornflowerblue", xlab="Glacier Area in 1966 (meters squared)", ylab="% change in area from 1966 to 2015")


# join data with the spatial data table 
g1966@data <- left_join(g1966@data, gAll, by="GLACNAME")
# use spplot to shade polygons based on % change
    #first argument is the spatial object
    #second is the column of data you want shaded
    #add a title
    #change color of borders to transparent
spplot(g1966,"gdiff", main="% change in area", col="transparent")

# you can also subset spatial data
# ex. look at the Vulture glacier in 1966
vulture66 <- g1966[g1966@data$GLACNAME == "Vulture Glacier",]
plot(vulture66, main = "Vulture Glacier in 1966", col="slategray")


## question 8 ## 

# find mean of % change
mean(gAll$gdiff)
# find std dev of % change
sd(gAll$gdiff)

# find glacier with largest 1966 area 
gAll[gAll$area66 == max(gAll$area66),]
# find glacier with smallest 1966 area
gAll[gAll$area66 == min(gAll$area66),]


## question 9 ##

# make a map of 1966 and 2015 areas for the glacier with the largest % loss
gAll[gAll$gdiff == max(gAll$gdiff),]
BoulderGlacier <- g1966[g1966@data$GLACNAME == "Boulder Glacier",]
plot(BoulderGlacier, col="lightblue", main ="Boulder Glacier area 1966 and 2015")
BoulderGlacier15 <- g2015[g2015@data$GLACNAME == "Boulder Glacier",]
plot(BoulderGlacier15, col = "royalblue4", add = TRUE)     
legend("topleft",
       legend = c("1966 area (meters squared)","2015 area (meters squared)"),
       col = c("lightblue", "royalblue4"),
       pch = 15,
       bty = "n")

# make a map of 1966 and 2015 areas for the glacier with the smallest % loss
gAll[gAll$gdiff == min(gAll$gdiff),]
PumpellyGlacier <- g1966[g1966@data$GLACNAME == "Pumpelly Glacier",]
PumpellyGlacier15 <-g2015[g2015@data$GLACNAME == "Pumpelly Glacier",]
plot(PumpellyGlacier, col="lightblue", main = "Pumpelly Glacier area 1966 and 2015")
plot(PumpellyGlacier15, col="royalblue4", add = TRUE)
legend("topleft", 
       legend = c("1966 area (meters squared)","2015 area (meters squared)"),
       col = c("lightblue", "royalblue4"),
       pch = 15,
       bty = "n")
