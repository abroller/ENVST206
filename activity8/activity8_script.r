## Activity 8 ##

install.packages(c("raster"))
library(raster)
library(rgdal)
library(ggplot2)

#set up directory for oneida data folder
dirR <- "/Users/abby/Documents/ENVST206/a08/oneida"

## read in sentinel data
# band 2 is blue light
rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
# band 3 is green light
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
# band 4 is red light
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
# band 8 is Near Infrared
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))

## mapping raster data ----

# use plot to map the raster data
plot(rdatB2/10000)

# question 2 ---
# stack red green and blue to create true color image
rgbS <- stack(rdatB4, rdatB3, rdatB2)/10000
# create color composite to view raster, set scale to include some reflectance >1
plotRGB(rgbS, scale=2)
# add a contrast stretch to see the colors better (don't need scale argument when adding a stretch)
plotRGB(rgbS, stretch="lin")

# the plotting function lowers resolution
# bring it back to full resolution
# get the total number of pixels by multiplying rows x columns in the raster
plotRGB(rgbS, stretch="lin", maxpixels=rgbS@nrows*rgbS@ncols)

# question 3 ---
# calculate number of pixels
rgbS@nrows*rgbS@ncols

# question 4 ---
# create false color map
plotRGB(rgbS, 2, 3, 1, stretch="lin", maxpixels=rgbS@nrows*rgbS@ncols )

## Analyzing raster data -----

## question 5 ---
# use raster math to calculate NDVI
# NIR - red/(NIR + red)
NDVI <- (rdatB8-rdatB4)/(rdatB8+rdatB4)
# visualize NDVI 
plot(NDVI)

## Extracting data for different landcover classes ----

# read in landcover points data
algae <- readOGR(paste0(dirR,"/Oneida/algae.shp"), verbose=FALSE)
agri <- readOGR(paste0(dirR,"/Oneida/agriculture.shp"), verbose=FALSE)
forest <- readOGR(paste0(dirR,"/Oneida/forest.shp"), verbose=FALSE)
water <- readOGR(paste0(dirR,"/Oneida/water.shp"), verbose=FALSE)
wetlands <- readOGR(paste0(dirR,"/Oneida/wetlands.shp"), verbose=FALSE)
# plot these points and true color
plotRGB(rgbS, stretch="lin", maxpixels=2297430)
plot(algae, add=TRUE, col=rgb(0.5,0.5,0.5,0.5), pch=19)
plot(agri, add=TRUE, col=rgb(0.75,0.5,0.5,0.5), pch=19)
plot(forest, add=TRUE, col=rgb(0.75,0.75,0.25,0.5), pch=19)
plot(water, add=TRUE, col=rgb(0.33,0.75,0.75,0.5), pch=19)
plot(wetlands, add=TRUE, col=rgb(0.33,0.33,0.65,0.5), pch=19)
legend("bottomleft", c("algae","agri","forest","water","wetlands"),
       pch=19, col=c(rgb(0.5,0.5,0.5,0.5),rgb(0.75,0.5,0.5,0.5),rgb(0.75,0.75,0.25,0.5),rgb(0.33,0.75,0.75,0.5),rgb(0.33,0.33,0.65,0.5)),
       bty="n", cex=0.75)

# set up a data frame with all of the point coordinates
landExtract <- data.frame(landcID = as.factor(rep(c("algae","water","agri","forest","wetland"),each=120)),
                          x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],forest@coords[,1],wetlands@coords[,1]),
                          y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],forest@coords[,2],wetlands@coords[,2]))



