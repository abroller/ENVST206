## in class example ##

datB <- read.csv("/Users/abby/Documents/ENVST206/beaver_dam.csv")

#visualize datB with a scatterplot
plot(datB$dams.n, datB$area.ha, pch = 19)

#create linear model
dam.mod <- lm(datB$area.ha ~ datB$dams.n)

#check assumptions
#residuals normally distributed with Q-Q plot
dam.res <- rstandard(dam.mod)
qqnorm(dam.res)
qqline(dam.res)

#check equal variance
plot(datB$dams.n, dam.res, pch=19)
abline(h=0)

#look at linear model
summary(dam.mod)

## starting activity 4 ##
datB <- read.csv("/Users/abby/Documents/ENVST206/beaver_dam.csv")
head(datB)

plot(datB$dams.n, datB$area.ha, 
     pch = 19,
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab = "Number of beaver dams")

#set up regression
dam.mod <- lm(datB$area.ha ~ datB$dams.n)
#get standardized residuals, give them a name to refer to
dam.res <- rstandard(dam.mod)

## check assumptions
#qq plot to check normality
qqnorm(dam.res)
qqline(dam.res)
#double check with a shapiro-wilk test
shapiro.test(dam.res)

#check residual plot for equal variance
plot(datB$dams.n, dam.res,
     xlab = "beaver dams",
     ylab = "standardized residual")
#add horizontal line @ zero
abline(h=0)

#interpreting results
summary(dam.mod)
#make a plot with regression line
plot(datB$dams.n, datB$area.ha, 
      pch = 20,
      col = "royalblue4",
      ylab = "Surface water area (ha)",
      xlab = "Number of beaver dams")
abline(dam.mod, lwd=2)

## multiple linear regression ##

#read in phenology data
pheno <- read.csv("/Users/abby/Documents/ENVST206/red_maple_pheno.csv")
#set up panel of plots with one row and two columns
par(mfrow=c(1,2))
plot(pheno$Tmax,pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Maximum temperature (C)")
plot(pheno$Prcp,pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Precipitation (mm)")
#question 3
par(mfrow=c(2,2))
plot(pheno$Lat,pheno$doy,
     pch = 20,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Latitude")
plot(pheno$elev,pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Elevation (m)")
plot(pheno$Tmax,pheno$day,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Maximum temperature (C)")

siteD <- as.factor(pheno$siteDesc)
plot(siteD,pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Site Description")

##check for multi-collinearity
#covariance plot
plot(~ pheno$Lat + pheno$Tmax + pheno$Tmin + pheno$Prcp + pheno$elev + pheno$siteDesc)

##run the regression
#code urban/rural designation as 0,1 using ifelse function
pheno$urID <- ifelse(pheno$siteDesc == "Urban", 1, 0)
help(ifelse)
#multiple regression
mlr <- lm(pheno$doy ~ pheno$Tmax + pheno$Prcp + pheno$elev + pheno$urID)

dev.off()
#check assumptions
mlr.res <- rstandard(mlr)
qqnorm(mlr.res)
qqline(mlr.res)
mlFitted <- fitted(mlr)


plot(mlFitted, mlr.res, pch=20, main="Residual Variance Plot")
abline(h=0)

## interpret the regression
summary(mlr)




