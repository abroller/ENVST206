## in class example ##

datW <- read.csv("/Users/abby/Documents/ENVST206/noaa2011124.csv")
datW$NAME <- as.factor(datW$NAME)
#create vector of names
nameS <- levels(datW$NAME)
nameS
nameS[2]

#make dataframe
datP <- na.omit(data.frame(PRCP = datW$PRCP,
                   NAME = datW$NAME,
                   year = datW$year))
#total annual precipitation for each site
pr <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN="sum")
colnames(pr) <- c("NAME","year","totalP")

#how many observations in each year?
pr$ncount <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN="length")$x

#only use years with enough observations
pr <- pr[pr$ncount >= 364,]

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


