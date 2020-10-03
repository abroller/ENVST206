## start activity 5

# read in NOAA data file
datW <- read.csv("/Users/abby/Documents/ENVST206/noaa2011124.csv")
# specify the name column as a factor
datW$NAME <- as.factor(datW$NAME)
# create vector of all names for each level
nameS <- levels(datW$NAME)
nameS

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

# make a plot of mean annual maximum temperatures in NY and ND


install.packages("ggplot2")
library(ggplot2)

#base r plot
plot(pr$year, pr$totalP)

ggplot(data = pr,
         aes(x = year,
             y = totalP,
             color = NAME))+
  geom_point()+
  geom_path()+
  labs(x = "year", y = "Annual precipitation (mm)")+
  theme_classic()

