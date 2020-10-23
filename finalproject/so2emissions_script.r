## Environmental Data Science 
## Final Project
## Topic: Spatial distribution of pollution under SO2 trading program

library(dplyr)
library(ggplot2)

# read in daily SO2 data
# from https://www.epa.gov/outdoor-air-quality-data/download-daily-data
# site in eisenhower park, ny
datNY1990 <- read.csv("/Users/abby/Documents/ENVST206/finalproject/NY1990.csv")
datNY2008 <- read.csv("/Users/abby/Documents/ENVST206/finalproject/NY2008.csv")

#combine 1990 and 2008 into same data frame
datNY <- rbind(datNY1990, datNY2008)

# edit column names
# SO2 column refers to "Daily Max 1-hour SO2 Concentration"
colnames(datNY) <- c("Date","Source","SiteID", "POC", "SO2", "UNITS", "DAILY_AQI_VALUE", "SiteName", "DAILY_OBS_COUNT", "PERCENT_COMPLETE", "AQS_PARAMETER_CODE", "AQS_PARAMETER_DESC", "CBSA_CODE", "CBSA_NAME", "STATE_CODE", "STATE", "COUNTY_CODE", "COUNTY", "SITE_LAT", "SITE_LONG")

# make  dataframe 
NY.SO2 <- data.frame(SiteID=datNY$SiteID,
                    Name=datNY$SiteName,
                    Date=datNY$Date,
                    SO2=datNY$SO2)
# format date column
NY.SO2$Date <- as.Date(NY.SO2$Date, "%m/%d/%y")
# add a year column
NY.SO2$Year <- year(NY.SO2$Date)


### question 5: brief summary statistics ---

# mean and standard deviation of daily max so2 concentration 
SO2stats <- aggregate(NY.SO2$SO2, by=list(NY.SO2$Year), FUN="mean")
colnames(SO2stats) <- c("Year", "SO2avg")
SO2stats$SO2stdev <- aggregate(NY.SO2$SO2, by=list(NY.SO2$Year), FUN="sd")$x

# make box plot of both years to compare distribution
ggplot(data = NY.SO2, aes(x=Year, y=SO2, group=Year))+
  geom_boxplot()+
  theme_classic()+
  labs(y= "Daily Max 1-hour SO2 Concentration", title="Distribution of SO2 Concentration in Eisenhower Park, NY")


