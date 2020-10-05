## start activity 5

# read in NOAA data file
datW <- read.csv("/Users/abby/Documents/ENVST206/noaa2011124.csv")
# specify the name column as a factor
datW$NAME <- as.factor(datW$NAME)
# create vector of all names for each level
nameS <- levels(datW$NAME)
nameS


## question 2 ##

# make dataframe with just precip, year, and site name
# remove NA data points using na.omit
datP <- na.omit(data.frame(PRCP = datW$PRCP,
                   NAME = datW$NAME,
                   year = datW$year))
# total annual precipitation (mm) for each site
precip <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN="sum", na.rm = TRUE)
# rename columns
colnames(precip) <- c("NAME","year","totalP")

#add a column looking at the number of observations in each year
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN="length")$x

# make a new data frame
# only use years with enough observations
pr <- precip[precip$ncount >= 364,]

#look at livermore, CA and morrisvile, NY precip
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]

# make a plot of california precip
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year",
     yaxt = "n",
     ylim = c(0,1600))
# add y axis
# arguments are axis number: 1 bottom, 2 left, 3 top, 4 right
# las = 2 changes the labels to be read in the horizontal direction
axis(2, seq(0,1600, by=400), las=2)
# add new york
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col = "tomato3")
# add legend
legend("topleft",
       c("California", "New York"),
       col = c("black", "tomato3"),
       pch = 19,
       lwd = 1,
       bty = "n")


## question 3 ##
## make a plot of mean annual maximum temperatures in NY and ND 

# make a data frame of max temp
# remove NA
datT <- na.omit(data.frame(TMAX = datW$TMAX,
                           NAME = datW$NAME,
                           year = datW$year))
# calculate average max temp per year
avgTmax <- aggregate(datT$TMAX, by=list(datT$NAME, datT$year), FUN="mean", na.rm = TRUE)

#change column names
colnames(avgTmax) <- c("NAME", "Year", "Tmax")

# look at number of observations per year
avgTmax$ncount <- aggregate(datT$TMAX, by=list(datT$NAME, datT$year), FUN="length")$x

# only use years with enough observations
Tmx <- avgTmax[avgTmax$ncount >= 364, ]

# for each location
NY <- Tmx[Tmx$NAME == nameS[5],]
ND <- Tmx[Tmx$NAME == nameS[3],]

# make a plot
plot(NY$Year, NY$Tmax,
     type = "b",
     pch = 20,
     col = "royalblue1",
     main = "Mean Annual Maximum Temperatures in NY and ND",
     ylab = "Mean Maximum Temperature (Celcius)",
     xlab = "Year",
     yaxt = "n",
     ylim = c(8,16))
axis(2, seq(0,20, by=2), las=2)
points(ND$Year, ND$Tmax,
       type = "b",
       pch = 20,
       col = "saddlebrown")
legend("topleft",
       c("New York", "North Dakota"),
       col = c("royalblue1", "saddlebrown"),
       pch=19,
       lwd=1,
       bty="n")


## plotting with ggplot2 ##

#install.packages("ggplot2")
library(ggplot2)

## question 5 ##

# scatterplot of annual precipitation across all sites
ggplot(data = pr, aes(x = year, y = totalP, color = NAME))+
  geom_point(alpha=0.5)+
  geom_path(alpha=0.5)+
  labs(x="year", y="Annual Precipitation (mm)")+
  theme_classic()+
  scale_color_manual(values = c("darkseagreen", "lightpink2", "lightblue3","gold","lightslateblue"))

# violin plot of daily minimum temp
ggplot(data = datW, aes(x=NAME, y=TMIN))+
  geom_violin(fill=rgb(0.933,0.953,0.98))+
  geom_boxplot(width=0.2, size=0.25, fill="grey90")+
  theme_classic()

## question 7 ##

# look more closely at daily patterns in Mormon Flat AZ in 1974
sub <- datW[datW$NAME == nameS[4] & datW$year == 1974, ]
# specify date format
sub$DATE <- as.Date(sub$DATE, "%Y-%m-%d")
# make scatterplot for max temp
ggplot(data=sub, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="Year", y="Maximum Temperature (C)", title="Mormon Flat, AZ: Daily Maximum Temperature 1974")
# make a barplot for precipitation
ggplot(data=sub, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="Year", y="Daily precipitation (mm)", title="Mormon Flat, AZ: Daily Precipitation 1974")

## question 8 ##

# plot daily patterns in  WA in 1974
subWA <- datW[datW$NAME == nameS[1] & datW$year == 1974,]
# specify date format
subWA$DATE <- as.Date(subWA$DATE, "%Y-%m-%d")
# make scatterplot of max temp
ggplot(data=subWA, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="Year", y="Maximum Temperature (C)", title="Aberdeen, WA: Daily Maximum Temperature 1974")
#make barplot of precipitation
ggplot(data=subWA, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="Year", y="Daily precipitation (mm)", title="Aberdeen, WA: Daily Precipitation 1974")

## question 9 ##
# compare daily minimum temperature since 2000 in CA
subCA <- datW[datW$NAME == nameS[2] & datW$year >= 2000, ]
# format date
subCA$DATE <- as.Date(subCA$DATE, "%Y-%m-%d")
# plot of tmin
ggplot(data=subCA, aes(x=DATE, y=TMIN))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="Year", y="Daily Minimum Temperature (C)", title="Livermore, CA: Daily Minimum Temperature (2000-2019)")


