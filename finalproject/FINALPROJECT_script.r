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
# specify date format
datNY1990$Date <- as.Date(datNY1990$Date, "%m/%d/%y")
datNY2008$Date <- as.Date(datNY2008$Date, "%m/%d/%Y")

# Cleveland - Elyria, OH
datOH1990 <- read.csv(paste0(dirR,"/OH1990.csv"))
datOH2008 <- read.csv(paste0(dirR,"/OH2008.csv"))
# specify date format
datOH1990$Date <- as.Date(datOH1990$Date, "%m/%d/%y")
datOH2008$Date <- as.Date(datOH2008$Date, "%m/%d/%y")

# Philadelphia - Camden - Wilmington, PA-NJ-DE-MD
datDE1990 <- read.csv(paste0(dirR,"/DE1990.csv"))
datDE2008 <- read.csv(paste0(dirR,"/DE2008.csv"))
# specify date format
datDE1990$Date <- as.Date(datDE1990$Date, "%m/%d/%y")
datDE2008$Date <- as.Date(datDE2008$Date, "%m/%d/%y")


### organize data ----

# combine everything into one big data frame
dat.all <- rbind(datNY1990, datNY2008, datOH1990, datOH2008, datDE1990, datDE2008)
# keep only relevant columns
SO2.all <- data.frame(Date=dat.all$Date,
                      SO2=dat.all$Daily.Max.1.hour.SO2.Concentration,
                      cityID=dat.all$CBSA_NAME,
                      stationID=dat.all$Site.ID,
                      Year=year(dat.all$Date),
                      DOY=yday(dat.all$Date))
# treat year and city ID columns as factors
SO2.all$Year <- as.factor(SO2.all$Year)
SO2.all$cityID <- as.factor(SO2.all$cityID)
City <- levels(SO2.all$cityID)

# make smaller data frames to work with
# subset to 1990
SO2.1990 <- SO2.all[SO2.all$Year == "1990",]
# subset to 2008
SO2.2008 <- SO2.all[SO2.all$Year == "2008",]
# subset to NY
SO2.NY <- SO2.all[SO2.all$cityID == City[2],]
# subset to OH
SO2.OH <- SO2.all[SO2.all$cityID == City[1],]
# subset to DE
SO2.DE <- SO2.all[SO2.all$cityID == City[3],]

# subset to one station in NY
# first, find a station with complete data for both years
countNY <- aggregate(SO2.NY$SO2, by=list(SO2.NY$Year,SO2.NY$stationID), FUN="length")
colnames(countNY) <- c("Year","Station","count")
# station 360590005 has the most complete data for both years
NYstn <- SO2.NY[SO2.NY$stationID == "360590005",]


### summary statistics ----

# create a data table of yearly mean and standard deviation of daily max so2 concentration from each site 
SO2stats <- aggregate(SO2.NY$SO2, by=list(SO2.NY$Year), FUN="mean")
colnames(SO2stats) <- c("Year", "NY Average SO2")
SO2stats$sd.NY <- aggregate(SO2.NY$SO2, by=list(SO2.NY$Year), FUN="sd")$x
SO2stats$avg.OH <- aggregate(SO2.OH$SO2, by=list(SO2.OH$Year), FUN="mean")$x
SO2stats$sd.OH <- aggregate(SO2.OH$SO2, by=list(SO2.OH$Year), FUN="sd")$x
SO2stats$avg.DE <- aggregate(SO2.DE$SO2, by=list(SO2.DE$Year), FUN="mean")$x
SO2stats$sd.DE <- aggregate(SO2.DE$SO2, by=list(SO2.DE$Year), FUN="sd")$x
colnames(SO2stats) <- c("Year","NY Avg", "NY StdDev", "OH Avg", "OH StdDev", "DE Avg", "DE StdDev")

## calculate emissions reductions
# in NY
SO2stats[1,2]-SO2stats[2,2]
# in OH
SO2stats[1,4]-SO2stats[2,4]
# in DE
SO2stats[1,6]-SO2stats[2,6]

## calculate maximum emissions
max(SO2.NY$SO2[SO2.NY$Year == "1990"])
max(SO2.NY$SO2[SO2.NY$Year == "2008"])
max(SO2.OH$SO2[SO2.OH$Year == "1990"])
max(SO2.OH$SO2[SO2.OH$Year == "2008"])
max(SO2.DE$SO2[SO2.DE$Year == "1990"])
max(SO2.DE$SO2[SO2.DE$Year == "2008"])

# calculate mean SO2 emissions for NY Station 360590005
mean(NYstn$SO2[NYstn$Year == "1990"])
mean(NYstn$SO2[NYstn$Year == "2008"])


### t-test: is there a reduction in SO2 between 1990 and 2008 ? ----

## assumptions for t test 

# qq plot to test for normality
# New York
qqnorm(SO2.NY$SO2[SO2.NY$Year == "1990"])
qqline(SO2.NY$SO2[SO2.NY$Year == "1990"])
qqnorm(SO2.NY$SO2[SO2.NY$Year == "2008"])
qqline(SO2.NY$SO2[SO2.NY$Year == "2008"])
# Ohio
qqnorm(SO2.OH$SO2[SO2.OH$Year == "1990"])
qqline(SO2.OH$SO2[SO2.OH$Year == "1990"])
qqnorm(SO2.OH$SO2[SO2.OH$Year == "2008"])
qqline(SO2.OH$SO2[SO2.OH$Year == "2008"])
# Delaware
qqnorm(SO2.DE$SO2[SO2.DE$Year == "1990"])
qqline(SO2.DE$SO2[SO2.DE$Year == "1990"])
qqnorm(SO2.DE$SO2[SO2.DE$Year == "2008"])
qqline(SO2.DE$SO2[SO2.DE$Year == "2008"])

# bartlett test for equal variance
bartlett.test(SO2.NY$SO2 ~ as.factor(SO2.NY$Year))
bartlett.test(SO2.OH$SO2 ~ as.factor(SO2.OH$Year))
bartlett.test(SO2.DE$SO2 ~ as.factor(SO2.DE$Year))

## assumptions not met for an sample ##

# test significance of changes in SO2 --- not valid bc assumptions not met
t.test(SO2.NY$SO2[SO2.NY$Year == "1990"],SO2.NY$SO2[SO2.NY$Year == "2008"], alternative = "greater")
t.test(SO2.OH$SO2[SO2.OH$Year == "1990"],SO2.OH$SO2[SO2.OH$Year == "2008"], alternative = "greater")
t.test(SO2.DE$SO2[SO2.DE$Year == "1990"],SO2.DE$SO2[SO2.DE$Year == "2008"], alternative = "greater")

# try to run a t test for NY station 360590005
# test for normality
shapiro.test(NYstn$SO2[NYstn$Year == "1990"])
shapiro.test(NYstn$SO2[NYstn$Year == "2008"])
# test for equal variance
bartlett.test(NYstn$SO2 ~ NYstn$Year)

## both assumption tests failed ##
# so we can't run a t test for this station 


### ANOVA: is there a difference between cities? ----

# test assumptions
# we know from trying to do t test above that they don't meet normality assumption
# test for equal variance
bartlett.test(SO2.all$SO2[SO2.all$Year == "1990"] ~ SO2.all$cityID[SO2.all$Year == "1990"])
bartlett.test(SO2.all$SO2[SO2.all$Year == "2008"] ~ SO2.all$cityID[SO2.all$Year == "2008"])


## Visualizations ----

# boxplot of all cities and both years
ggplot(data=SO2.all, aes(x=cityID, y=SO2, fill=Year))+
  geom_boxplot(outlier.size=.5)+
  scale_fill_manual(values=c("salmon","royalblue1"))+
  theme_classic()+
  labs(x= "Urban Area", y="Daily Maximum 1-hour SO2 Concentration (ppb)", title="Annual Distributions of Sulfur Dioxide Concentrations")+
  theme(plot.title = element_text(size = 20,hjust=0.5))
 

#  separate box plots by urban area to compare distribution between years
# NY
ggplot(data = SO2.NY, aes(x=Year, y=SO2, fill=Year))+
  geom_violin(fill="gray88")+
  geom_boxplot(width=0.1, size=0.25, outlier.size=.5)+
  scale_fill_manual(values=c("salmon","royalblue1"))+
  theme_classic()+
  labs(y= "Daily Max 1-hour SO2 Concentration", title="Sulfur Dioxide Concentration in New York-Newark-Jersey City Area")+
  theme(plot.title = element_text(size = 15, hjust=0.5))

#OH
ggplot(data = SO2.OH, aes(x=Year, y=SO2, fill=Year))+
  geom_violin(fill="gray88")+
  geom_boxplot(width=0.1, size=0.25, outlier.size=.5)+
  scale_fill_manual(values=c("salmon","royalblue1"))+
  theme_classic()+
  labs(y= "Daily Max 1-hour SO2 Concentration", title="Sulfur Dioxide Concentration in Cleveland-Elyria Area")+
  theme(plot.title = element_text(size = 15, hjust=0.5))

#DE
ggplot(data = SO2.DE, aes(x=Year, y=SO2, fill=Year))+
  geom_violin(fill="gray88")+
  geom_boxplot(width=0.1, size=0.25, outlier.size=.5)+
  scale_fill_manual(values=c("salmon","royalblue1"))+
  theme_classic()+
  labs(y= "Daily Max 1-hour SO2 Concentration", title="Sulfur Dioxide Concentration in Philadelphia-Camden-Wilmington Area")+
  theme(plot.title = element_text(size = 15, hjust=0.5))


# separate box plots by year to compares distributions between locations
ggplot(data=SO2.1990, aes(x=cityID, y=SO2))+
  geom_boxplot(fill="mistyrose",outlier.size=.5)+
  theme_classic()+
  labs(x= "Urban Area", y="Daily Maximum 1-hour SO2 Concentration (ppb)", title="Sulfur Dioxide Concentrations in 1990")+
  theme(plot.title = element_text(size = 15, hjust=0.5))

ggplot(data=SO2.2008, aes(x=cityID, y=SO2))+
  geom_boxplot(fill="lightsteelblue1", outlier.size=.5)+
  theme_classic()+
  labs(x= "Urban Area", y="Daily Maximum 1-hour SO2 Concentration (ppb)", title="Sulfur Dioxide Concentrations in 2008")+
  theme(plot.title = element_text(size = 15, hjust=0.5))

# line graph of the one station in NY
ggplot(data=NYstn, aes(x=DOY, y=SO2, color=Year))+
  geom_path()+
  labs(x="Day of Year", y="Daily Maximum 1-hour SO2 Concentration (ppb)", title="Sulfur Dioxide Concentration at NY Station 360590005")+
  theme_classic()+
  scale_color_manual(values = c("salmon","royalblue1"))+
  theme(plot.title = element_text(size = 15, hjust=0.5))








