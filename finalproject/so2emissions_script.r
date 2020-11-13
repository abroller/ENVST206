## Environmental Data Science 
## Final Project
## Topic: Spatial distribution of pollution under SO2 trading program

library(dplyr)
library(ggplot2)
library(lubridate)

# set up directory for final project data folder
dirR <- "/Users/abby/Documents/ENVST206/finalproject"

### read in data ----
# from https://www.epa.gov/outdoor-air-quality-data/download-daily-data
# New York - Newark - Jersey City, NY-NJ
datNY1990 <- read.csv(paste0(dirR,"/NY1990.csv"))
datNY2008 <- read.csv(paste0(dirR,"/NY2008.csv"))

# Cleveland - Elyria, OH
datOH1990 <- read.csv(paste0(dirR,"/OH1990.csv"))
datOH2008 <- read.csv(paste0(dirR,"/OH2008.csv"))

# Philadelphia - Camden - Wilmington, PA-NJ-DE-MD
datDE1990 <- read.csv(paste0(dirR,"/DE1990.csv"))
datDE2008 <- read.csv(paste0(dirR,"/DE2008.csv"))

#combining 1990 and 2008 into same data frame by location
dat.NY <- rbind(datNY1990, datNY2008)
dat.OH <- rbind(datOH1990, datOH2008)
dat.DE <- rbind(datDE1990, datDE2008)

### Make smaller data frames ----
# for NY
SO2.NY <- data.frame(Date=dat.NY$Date,
                     SiteID=dat.NY$Site.ID,
                     SO2=dat.NY$Daily.Max.1.hour.SO2.Concentration,
                     SiteLat=dat.NY$SITE_LATITUDE,
                     SiteLong=dat.NY$SITE_LONGITUDE)
# for OH
SO2.OH <- data.frame(Date=dat.OH$Date,
                     SiteID=dat.OH$Site.ID,
                     SO2=dat.OH$Daily.Max.1.hour.SO2.Concentration,
                     SiteLat=dat.OH$SITE_LATITUDE,
                     SiteLong=dat.OH$SITE_LONGITUDE)
# for DE
SO2.DE <- data.frame(Date=dat.DE$Date,
                     SiteID=dat.DE$Site.ID,
                     SO2=dat.DE$Daily.Max.1.hour.SO2.Concentration,
                     SiteLat=dat.DE$SITE_LATITUDE,
                     SiteLong=dat.DE$SITE_LONGITUDE)

# format date column in each data frame
SO2.NY$Date <- as.Date(SO2.NY$Date, "%m/%d/%y")
SO2.OH$Date <- as.Date(SO2.OH$Date, "%m/%d/%y")
SO2.DE$Date <- as.Date(SO2.DE$Date, "%m/%d/%y")

# add year column to each data frame
SO2.NY$Year <- year(SO2.NY$Date)
SO2.OH$Year <- year(SO2.OH$Date)
SO2.DE$Year <- year(SO2.DE$Date)


### summary statistics ----

# create a data table of yearly mean and standard deviation of daily max so2 concentration from each site 
SO2stats <- aggregate(SO2.NY$SO2, by=list(SO2.NY$Year), FUN="mean")
colnames(SO2stats) <- c("Year", "avg.NY")
SO2stats$sd.NY <- aggregate(SO2.NY$SO2, by=list(SO2.NY$Year), FUN="sd")$x
SO2stats$avg.OH <- aggregate(SO2.OH$SO2, by=list(SO2.OH$Year), FUN="mean")$x
SO2stats$sd.OH <- aggregate(SO2.OH$SO2, by=list(SO2.OH$Year), FUN="sd")$x
SO2stats$avg.DE <- aggregate(SO2.DE$SO2, by=list(SO2.DE$Year), FUN="mean")$x
SO2stats$sd.DE <- aggregate(SO2.DE$SO2, by=list(SO2.DE$Year), FUN="sd")$x

# make box plots to compare distribution by year for each site
ggplot(data = SO2.NY, aes(x=Year, y=SO2, group=Year))+
  geom_boxplot()+
  theme_classic()+
  labs(y= "Daily Max 1-hour SO2 Concentration", title="Distribution of SO2 Concentration in NYC area")

ggplot(data = SO2.OH, aes(x=Year, y=SO2, group=Year))+
  geom_boxplot()+
  theme_classic()+
  labs(y= "Daily Max 1-hour SO2 Concentration", title="Distribution of SO2 Concentration in Cleveland, OH area")

ggplot(data = SO2.DE, aes(x=Year, y=SO2, group=Year))+
  geom_boxplot()+
  theme_classic()+
  labs(y= "Daily Max 1-hour SO2 Concentration", title="Distribution of SO2 Concentration in Delaware area")

# test significance of changes in SO2 
## need to fix NY data 
t.test(SO2.NY$SO2[SO2.NY$Year == "1990"],SO2.NY$SO2[SO2.NY$Year == "2008"], alternative = "greater")
t.test(SO2.OH$SO2[SO2.OH$Year == "1990"],SO2.OH$SO2[SO2.OH$Year == "2008"], alternative = "greater")
t.test(SO2.DE$SO2[SO2.DE$Year == "1990"],SO2.DE$SO2[SO2.DE$Year == "2008"], alternative = "greater")


# calculate reduction of SO2 at each site (as ppm and percentage)


# plot locations of stations

# aggregate by year and site and remove sites with more than 15 missing days
NY.SO2year <- aggregate(NY.SO2$SO2, by=list(NY.SO2$Year, NY.SO2$SiteID), FUN="length")
colnames(NY.SO2year) <- c("Year","StationID", "ncount")
NYSO2year <- subset(NY.SO2year, NY.SO2year$ncount >= 350)




