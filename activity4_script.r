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


