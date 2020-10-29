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

# use plot to map the raster data
plot(rdatB2/10000)

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
# calculate number of pixels
rgbS@nrows*rgbS@ncols

# create false color map
plotRGB(rgbS, 2, 1, 3, stretch="lin", maxpixels=rgbS@nrows*rgbS@ncols )


