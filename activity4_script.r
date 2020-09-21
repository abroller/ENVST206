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

## interpreting results
summary(dam.mod)
#make a plot with regression line
plot(datB$dams.n, datB$area.ha, 
      pch = 20,
      col = "royalblue4",
      ylab = "Surface water area (ha)",
      xlab = "Number of beaver dams")
abline(dam.mod, lwd=2)



